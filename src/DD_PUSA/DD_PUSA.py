import numpy as np
import pandas as pd
import json
import os
import base
import re

from math import exp, ceil
from functools import partial

import base.solution

from . import  cooling_schedules as schedules
from .sets import pareto_set, candidate_set
from .mo_ocba_soln import mo_ocba_solution
from .utils import p_accept

from base.optimizers import popMultiObjOptim, wrapper
from utils import read_yaml, zip_dir, convert_to_serializable
from dask.distributed import Client


class dd_pusa(popMultiObjOptim):
    def __init__(
        self,
        config: dict = None,
        mo_ocba_cfg: dict = None,
        hyperparam_cfg: dict = None,
        evaluation_fn = None,
        worker_pool: Client|str = None,
        create_checkpoint: bool = False
    ):        
        super().__init__(config=config, create_checkpoint=create_checkpoint)
        np.random.seed(config.get('seed'))
        self.worker_pool = worker_pool
        
        if isinstance(worker_pool,Client):
            self.evaluation_fn = staticmethod(evaluation_fn) if evaluation_fn else None        
            zip_dir('src','tmp_worker_dir.zip')
            worker_pool.upload_file('tmp_worker_dir.zip')
            # os.remove('tmp_worker_dir.zip')
        else:
            self.evaluation_fn = evaluation_fn

        self.config["mo_ocba_cfg"] = mo_ocba_cfg
        self.config["hyperparam_cfg"] = hyperparam_cfg


        if config is not None:
            self.cooling_schedule = read_yaml("src/DD_PUSA/configs/cooling_schedules.yaml").get(self.config.get("cooling_schedule"))
            self.report_interval = self.config.get('report_progress_interval')
            
            if mo_ocba_cfg is None:
                self.sol_skeleton = partial(base.solution,
                                            obj_fns = self.config.get("obj_fns"))
            else:
                self.sol_skeleton = partial(
                                        mo_ocba_solution,
                                        init_reps=self.config.get("mo_ocba_cfg").get("initial_reps_per_sol"),
                                        obj_fns=self.config.get("obj_fns"),
                                        decode_sols = self.config.get('var_info').get('constraint_info'))
        with open(os.path.join(self.config.get('job_dir'),"optim-config.json"),'w') as f:
                json.dump(obj = self.config,
                        fp = f,
                        default=convert_to_serializable,
                        indent = 4)

    def _evaluate(self,inp):
        return self.evaluation_fn(inp)
    
    def reset(self):
        self.history = {}
        self.cummulative_samples = 0
        self.tabu_list = [] if self.config.get("hyperparam_cfg").get('tabu_limit') else None
        self.temp = self.cooling_schedule.get("t_0")
        self.current_iteration = 0
        self.pareto_set = pareto_set(
            obj_fns=self.config.get("obj_fns")
        )

    def generate_candidate(self):
        tweak_lim = self.config.get("hyperparam_cfg").get("max_tweak") 
        acceptance_prob = p_accept(
            t_0=self.cooling_schedule.get("t_0"),
            t_damp=self.cooling_schedule.get("t_damp"),
            schedule=self.config.get("cooling_schedule"),
            it=self.current_iteration,
        )
            
        def fix_cell(cell):
            if cell < 0:
                cell = abs(cell + tweak_lim)
            elif cell > tweak_lim:
                cell = (tweak_lim - cell) + tweak_lim
            return cell
        
        # Vectorized version of the fix cell function
        vfix_cell = np.vectorize(fix_cell)

        counter = 0
        new_sol = None
        while new_sol is None:
            # Loop candidate generation until a valid solution is generated or the counter ends
            if self.pareto_set.best is not None:
                # Modifications are made to the sol_rep attribute and then decoded into an allocation using decode()
                changes = np.array([np.random.uniform(-tweak_lim, tweak_lim) if np.random.uniform(0,1,1) < acceptance_prob else 0 for _ in range(self.config.get('var_info').get('num_variables'))])
                new_sol = vfix_cell(changes + self.pareto_set.best.sol_rep)
            else:
                new_sol = np.random.uniform(0, self.config.get("hyperparam_cfg").get('max_tweak'), self.config.get('var_info').get('num_variables'))
                        
            new_sol =  self.sol_skeleton(solution=new_sol)
            if self.tabu_list is not None:
                if not any(map(lambda i: np.array_equal(new_sol.solution, i), self.tabu_list)):
                    self.tabu_list.append(new_sol.solution)
                else:
                    counter += 1
                    new_sol = None
                
            if counter > 40:
                break
            
        return new_sol

    def mo_ocba(self, sol_set, pareto_adj = False):
        self.mo_ocba_reps = 0

        replication_limit = self.config.get("mo_ocba_cfg").get("replications_per_sol_x_iter") * self.sols_per_iter
        
        
        if not pareto_adj:
            k_val = self.sols_per_iter
            sol_set.procedure_I(delta=self.config.get("mo_ocba_cfg").get("replications_per_sol_x_iter"))
            sP = candidate_set(
                candidates=[
                    sol_set.set[i]
                    for i in np.argsort(np.array(sol_set.get_attribute("psi")[:k_val]))
                ],
                obj_fns=self.config.get("obj_fns")
            )
        else:
            sol_set.procedure_I(delta=self.config.get("mo_ocba_cfg").get("replications_per_sol_x_iter"))
            sP = candidate_set(
                candidates=[sol for sol in sol_set.set if sol.replications < self.config.get("mo_ocba_cfg").get("replications_per_sol_x_iter")],
                obj_fns=self.config.get("obj_fns"),
            )
            k_val = sP.length
            
        print(f"Iteration {self.current_iteration} Generating MO-OCBA Samples")
        reps_used = 0
        while min(sP.get_attribute("psi")) > self.config.get("mo_ocba_cfg").get("psi_target") and sP.length > 1:
            sP.procedure_II(sol_set=sol_set, K=k_val, delta=self.config.get("mo_ocba_cfg").get("replications_per_sol_x_iter"))
            sP.reorder("delta_psi_d", decreasing=True)
            if any(i == 0 for i in sP.get_attribute("delta_psi_d")):
                k_test = max(
                    [
                        0,
                        min(
                            [
                                index
                                for index, value in enumerate(
                                    sP.get_attribute("delta_psi_d")
                                )
                                if value == 0
                            ]
                        ),
                    ]
                )
            else:
                k_test = sP.length - 1

            psi_ref = sP.set[k_test].delta_psi_d
            reps_used += sP.allocate_replications(
                delta=min(self.config.get("mo_ocba_cfg").get("replications_per_sol_x_iter"), replication_limit),
                K=k_test,
                delta_ref=(psi_ref * self.config.get("mo_ocba_cfg").get("replications_per_sol_x_iter"))
                / np.sum(np.abs(np.array(sP.get_attribute("delta_psi_d")[:k_test]))),
                psi_ref=psi_ref,
            )
            
            
            self.generate_samples(sol_set=sP)

            self.mo_ocba_reps += reps_used
            replication_limit -= reps_used

            if replication_limit <= 0:
                break

            sol_set.procedure_I(delta=self.config.get("mo_ocba_cfg").get("replications_per_sol_x_iter"))
        print(f"Iteration {self.current_iteration} Finished MO-OCBA Samples")
        return sP.set, list(set(sol_set.set).difference(set(sP.set)))

    def cool_temp(self):
        self.temp = getattr(schedules, self.config.get("cooling_schedule"))(
            t_0=self.cooling_schedule.get("t_0"), t_damp=self.cooling_schedule.get("t_damp"), current_iteration=self.current_iteration
        )

    def update_history(self, return_val: bool = False):
        self.sols_full_data_df = pd.concat(
            [i.data.mean(axis=0) for i in self.pareto_set.set], axis=1
        ).T
        self.sols_full_data_df = self.sols_full_data_df.assign(id=[i.id for i in self.pareto_set.set])
        if return_val:
            return self.history[-1]
        else:
            self.history.update(
                {f"Iteration-{self.current_iteration}":
                    {
                    "Iteration Replications": self.sample_counter,
                    "Total Replications": self.cummulative_samples,
                    "Temperature": self.temp,
                    "Pareto Set Length": self.pareto_set.length,
                    "Estimated Pareto Front": self.sols_full_data_df,
                    "Pareto Set Counter": self.pareto_set.counter
                }})

    def results_to_console(self, termination_message=False):
        messages = [
            f"Iteration {self.current_iteration} required {self.sample_counter} replications at temperature {self.temp}.",
            f"Current pareto set is unchanged for {self.pareto_set.counter} iterations.",
            f"There are {self.pareto_set.length} estimated solutions in the pareto set.",
            "Current Pareto Front:\n {}".format(
                self.sols_full_data_df.sort_values(
                    by=list(self.sols_full_data_df.columns)
                ).to_string()
            ),
        ]
        if self.config.get("mo_ocba_cfg"):
            messages.append(
                f"M.O.O.C.B.A allocated {self.mo_ocba_reps} more replications. \n"
            )
        messages.append("------------------------------------------------------------\n")
        if termination_message:
            messages.append("              OPTIMIZATION JOB HAS CONCLUDED                \n")
            messages.append("------------------------------------------------------------\n")
            

        for m in messages:
            print(m)

    def execute_iteration(self):
        self.current_iteration += 1

        if self.pareto_set.length > 0:  # Find the
            old_pset_ids = self.pareto_set.get_attribute("id")
        else:
            old_pset_ids = None

        candidate_solutions = candidate_set(
            candidates=[self.generate_candidate() for _ in range(self.sols_per_iter)],
            obj_fns=self.config.get("obj_fns"),
        )
        
        print(f"Iteration {self.current_iteration} Generating Candidate Samples")
        self.generate_samples(sol_set=candidate_solutions)
        print(f"Finished Iteration {self.current_iteration} Initial Samples")
        
        self.sample_counter = np.sum(
            np.array([sol.replications for sol in candidate_solutions.set])
        )
        if candidate_solutions.length > 0:
            if self.config.get("mo_ocba_cfg"):
                candidate_solutions, _ = self.mo_ocba(candidate_solutions)
                self.sample_counter += self.mo_ocba_reps
            [self.pareto_set.add_solution(sol) for sol in candidate_solutions]
            if self.config.get("mo_ocba_cfg"): 
                if self.current_iteration % self.config.get("mo_ocba_cfg").get("p_set_interval") == 0:
                
                    _ , rejects = self.mo_ocba(self.pareto_set)
                
                    for sol in rejects:
                        self.pareto_set.remove_solution(index = self.pareto_set.get_attribute('id').index(sol.id))
                
                    self.sample_counter += self.mo_ocba_reps
            self.pareto_set.update(prev_set=old_pset_ids,
                                   stochastic = (self.config.get("mo_ocba_cfg") is not None),
                                   alpha = self.config.get("hyperparam_cfg").get("solution_comparison_alpha"))
            self.pareto_set.find_best()
        else:
            self.pareto_set.counter += 1
        self.update_history()
        if self.report_interval:
            if self.current_iteration % self.report_interval == 0:
                self.results_to_console()
        self.cummulative_samples += self.sample_counter
        self.sample_counter = 0
        if old_pset_ids is None or self.pareto_set.get_attribute("id") != old_pset_ids:
            self.cool_temp()
        self.tabu_list[: -min(self.config.get("hyperparam_cfg").get("tabu_limit"), len(self.tabu_list))]

    def terminate_experiment(self, checkpoint=True):
        self.results_to_console(termination_message=True)
        self.update_history()
        if checkpoint and self.checkpoint:
            self.create_checkpoint()

        print("~~~~~~~~~~Optimization Job Concluded~~~~~~~~~~~~~~~")
        return self.pareto_set
    
    def generate_samples(self, sol_set):
        job_list = []
        for sol in sol_set.set:
            for _ in range(sol.pending_samples):
                job_list.append(
                    {"index": sol_set.set.index(sol),
                     "solution": sol.solution,
                     "seed": np.random.randint(9999)}
                )        
            
        if self.worker_pool is not None:
            if isinstance(self.worker_pool, Client):
                results = self.worker_pool.map(self.evaluation_fn,job_list)
                results = self.worker_pool.gather(results)
            else:
                results = self.worker_pool.map(wrapper, [(self, inp) for inp in job_list])
        else:
            results = [self._evaluate(arg) for arg in job_list]
                  
        for idx in range(len(results)):
            sol_set.set[job_list[idx].get("index")].update_data(new_data=results[idx])
            
        for sol in sol_set.set:
            sol.replications += sol.pending_samples
            sol.pending_samples = 0

    def __reduce__(self):
        state = self.__dict__.copy()
        del state["worker_pool"]
        return (
            self.__class__,
            (
                self.config,
                self.config.get("hyperparam_cfg"),
                self.cooling_schedule,
                self.config.get("mo_ocba_cfg"),
            ),
            state,
        )

    def _to_dict(self):
        self_dict, config_dict = super()._to_dict()
        self_dict.update({
            "cummulative_samples": self.cummulative_samples,
            "temp": self.temp,
            "pareto_set": self.pareto_set._to_dict(),
            "report_interval": self.report_interval,
            "tabu_list":np.stack(self.tabu_list),
            
        })
        
        config_dict.update({
            "hyperparam_cfg": self.config.get("hyperparam_cfg"),
            'mo_ocba_cfg': self.config.get("mo_ocba_cfg"),
            })
        return self_dict, config_dict
          
    @classmethod
    def from_checkpoint(cls,workers,evaluation_fn, exp_dir: str):
        instance, data = super()._from_checkpoint(exp_dir)
        # instance.tabu_list = np.ndarray(data.get("tabu_list"))
        instance.evaluation_fn = evaluation_fn  
        if instance.mo_ocba_cfg is None:
            instance.pareto_set = pareto_set._from_dict(
                data = instance.pareto_set,
                sol_class=base.solution)
            instance.sol_skeleton = partial(
                base.solution, 
                obj_fns = instance.config.get("obj_fns"))        
        else:
            instance.pareto_set = pareto_set._from_dict(
                data = instance.pareto_set,
                sol_class=mo_ocba_solution)
            instance.sol_skeleton = partial(
                mo_ocba_solution,
                init_reps=instance.mo_ocba_cfg.get("initial_reps_per_sol"),
                obj_fns=instance.config.get("obj_fns"),
                decode_sols = instance.config.get('var_info').get('constraint_info'))
          
        instance.cooling_schedule = read_yaml("src/DD_PUSA/configs/cooling_schedules.yaml").get(instance.config.get("cooling_schedule"))
        if isinstance(workers,Client):
            for module in os.listdir("src"):
                workers.upload_file(f"src/{module}")
        instance.worker_pool = workers
        return instance

        