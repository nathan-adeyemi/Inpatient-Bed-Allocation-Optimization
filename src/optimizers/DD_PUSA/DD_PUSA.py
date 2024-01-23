import numpy as np
import pandas as pd
import logging
import os

from omegaconf import OmegaConf, DictConfig
from utils.utils import decode, p_accept
from utils.r_communication import worker_pool
from math import exp, ceil
from sets.DD_PUSA import pareto_set, candidate_set
from solution.DD_PUSA import mo_ocba_solution
from ..base import popMultiObjOptim
from ..DD_PUSA import cooling_schedules as schedules
from pathlib import Path


class dd_pusa(popMultiObjOptim):
    def __init__(
        self,
        experiment_info: DictConfig,
        hyper_params: DictConfig,
        moocba: DictConfig = None,
    ):
        super().__init__(config=experiment_info)

        # Assign the configs to the class
        self.experiment_info = experiment_info
        self.hyper_params = hyper_params
        self.moocba = moocba

        # Set optimization problem information not set by the base class
        self.stoch_optim = self.experiment_info.stoch_optim
        self.sim_dict = self.experiment_info.capacity_dictionary
        self.num_variables = np.sum(
            np.array(
                [self.sim_dict[i]["num_pools"] for i in list(self.sim_dict.keys())]
            )
        )
        self.num_workers = self.experiment_info.num_workers
        self.obj_fns = self.experiment_info.obj_fns

        # Set DD-PUSA options and hyper_parameters
        self.use_moocba = self.experiment_info.use_moocba
        if self.stoch_optim:
            self.alpha = self.hyper_params.solution_comparison_alpha.value
        else:
            self.alpha = None
        self.pareto_limit = self.hyper_params.pareto_limit.value
        self.tabu_limit = self.hyper_params.tabu_limit.value
        self.tweak_limit = self.hyper_params.max_tweak.value
        if self.use_moocba:
            self.psi_target = self.moocba.psi_target.value
            self.replications_per_iter = self.moocba.replications_per_sol_x_iter.value
            self.init_reps = self.moocba.initial_reps_per_sol.value
        self.history = []

        # Set logging settings
        self.report_progress = self.experiment_info.report_progress.print_to_console
        if self.report_progress:
            self.report_interval = self.experiment_info.report_progress.interval
        self.logging = self.experiment_info.report_progress.log_file is not None

        # Set cooling schedule type and parameters
        self.cool_sched_name = self.experiment_info.cooling_schedule
        if experiment_info.tune:
            self.cooling_schedule = OmegaConf.load(
                os.path.join(
                    experiment_info.config_path,
                    "cooling_schedule",
                    f"{self.cool_sched_name}.yaml",
                )
            )
        else:
            self.cooling_schedule = OmegaConf.load(
                os.path.join(
                    Path("src/optimizers/DD_PUSA/configs/cooling_schedule").resolve(),
                    f"{self.cool_sched_name}.yaml",
                )
            )
        self.t_damp = self.cooling_schedule.t_damp.value
        self.t_0 = self.cooling_schedule.t_0

    def reset(self):
        self.cummulative_samples = 0
        self.tested_allocations = []
        self.temp = self.t_0
        self.current_iteration = 0

        if (
            "test_bench" in list(self.sim_dict.keys())[0]
            or "testbench" in list(self.sim_dict.keys())[0]
        ):
            self.sim_size = self.sim_dict[list(self.sim_dict.keys())[0]]["size"]
        else:
            self.sim_size = "ed_to_ip"
        self.create_workers()
        self.pareto_set = pareto_set(
            self.stoch_optim, set=[], obj_fns=self.obj_fns, workers=self.worker_pool
        )

    def create_workers(self):
        self.worker_pool = worker_pool(
            num_workers=self.num_workers,
            sim_info={"size": self.sim_size, "obj_fns": self.obj_fns},
        )

    def generate_candidate(self):
        def fix_cell(cell, lim: float = 1):
            if cell < (-1 * lim):
                cell = -lim + abs(cell + lim)
            elif cell > lim:
                cell = (lim - cell) + lim
            return cell

        counter = 0
        tweak_lim = self.tweak_limit
        acceptance_prob = p_accept(
            t_0=self.t_0,
            t_damp=self.t_damp,
            schedule=self.cool_sched_name,
            it=self.current_iteration,
        )

        while True:
            # Loop candidate generation until a valid solution is generated or the counter ends
            if self.pareto_set.best is not None:
                changes = np.random.uniform(-tweak_lim, tweak_lim, self.num_variables)
                changes = np.array(
                    [
                        0 if np.random.uniform(0, 1, 1) > acceptance_prob else i
                        for i in changes
                    ]
                )
                new_sol = np.array(
                    [
                        fix_cell(i, lim=self.tweak_limit * 3)
                        for i in (self.pareto_set.best.solution + changes)
                    ]
                )
            else:
                new_sol = np.random.uniform(0, 1, self.num_variables)

            allocation = decode(new_sol, self.sim_dict)
            if not any(np.array_equal(allocation, i) for i in self.tested_allocations):
                self.tested_allocations.append(allocation)
                return mo_ocba_solution(
                    solution=new_sol,
                    alpha=self.alpha,
                    allocation=allocation,
                    init_reps=self.init_reps,
                    obj_fns=self.obj_fns,
                )
            else:
                counter += 1

            if counter > 40:
                break

    def mo_ocba(self, sol_set):
        self.mo_ocba_reps = 0

        replication_limit = self.replications_per_iter * self.sols_per_iter

        k_val = ceil(self.sols_per_iter / 3)
        if sol_set.length < k_val:
            k_val = sol_set.length

        sol_set.procedure_I(delta=self.replications_per_iter)
        sP = candidate_set(
            candidates=[
                sol_set.set[i]
                for i in np.argsort(np.array(sol_set.get_attribute("psi")[:k_val]))
            ],
            workers=self.worker_pool,
            obj_fns=self.obj_fns,
        )
        reps_used = 0
        while min(sP.get_attribute("psi")) > self.psi_target and sP.length > 1:
            sP.procedure_II(sol_set=sol_set, K=k_val, delta=self.replications_per_iter)
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
                delta=min(self.replications_per_iter, replication_limit),
                K=k_test,
                delta_ref=(psi_ref * self.replications_per_iter)
                / np.sum(np.abs(np.array(sP.get_attribute("delta_psi_d")[:k_test]))),
                psi_ref=psi_ref,
            )
            sP.generate_samples()

            self.mo_ocba_reps += reps_used
            replication_limit -= reps_used

            if replication_limit <= 0:
                break

            sol_set.procedure_I(delta=self.replications_per_iter)
        return sP.set, list(set(sol_set.set).difference(set(sP.set)))

    def cool_temp(self):
        self.temp = getattr(schedules, self.cool_sched_name)(
            t_0=self.t_0, t_damp=self.t_damp, current_iteration=self.current_iteration
        )

    def update_history(self, return_val: bool = False):
        self.sols_full_data_df = pd.concat(
            [i.data.mean(axis=0) for i in self.pareto_set.set], axis=1
        ).T
        if return_val:
            return self.history[-1]
        else:
            self.history.append(
                {
                    "Iteration": self.current_iteration,
                    "Iteration Replications": self.sample_counter,
                    "Total Replications": self.cummulative_samples,
                    "Temperature": self.temp,
                    "Pareto Set Length": self.pareto_set.length,
                    "Estimated Pareto Front": self.sols_full_data_df,
                }
            )

    def print(self, message):
        if self.logging:
            logging.info(message)
        if self.report_progress:
            print(message)

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
        if self.use_moocba:
            messages.append(
                f"M.O.O.C.B.A allocated {self.mo_ocba_reps} more replications."
            )

        if termination_message:
            messages.append("------------------------------")
            messages.append("OPTIMIZATION JOB HAS CONCLUDED")
            messages.append("------------------------------")

        for m in messages:
            self.print(m)

    def execute_iteration(self):
        self.current_iteration += 1

        if self.pareto_set.length > 0:  # Find the
            old_pset_ids = self.pareto_set.get_attribute("id")
        else:
            old_pset_ids = None

        candidate_solutions = candidate_set(
            candidates=[self.generate_candidate() for _ in range(self.sols_per_iter)],
            workers=self.worker_pool,
            obj_fns=self.obj_fns,
        )
        candidate_solutions.generate_samples()
        self.sample_counter = np.sum(
            np.array([sol.replications for sol in candidate_solutions.set])
        )
        if len(candidate_solutions.set) > 0:
            if self.use_moocba:
                candidate_solutions, _ = self.mo_ocba(candidate_solutions)
                self.sample_counter += self.mo_ocba_reps
            for sol in candidate_solutions:
                self.pareto_set.add_solution(sol)
            if self.use_moocba & self.current_iteration % 10 == 0:
                updated_pSet, _ = self.mo_ocba(self.pareto_set)
                for sol in updated_pSet:
                    self.pareto_set.add_solution(sol)
                self.sample_counter += self.mo_ocba_reps
            self.pareto_set.update(prev_set=old_pset_ids)
            self.pareto_set.find_best()
        else:
            self.pareto_set.counter += 1
        self.update_history()
        if self.report_progress:
            if self.current_iteration % self.report_interval == 0:
                self.results_to_console()
        self.cummulative_samples += self.sample_counter
        self.sample_counter = 0
        if old_pset_ids is None or self.pareto_set.get_attribute("id") != old_pset_ids:
            self.cool_temp()
        self.tested_allocations[: -min(self.tabu_limit, len(self.tested_allocations))]

    def terminate_experiment(self, checkpoint=True):
        self.results_to_console(termination_message=True)
        self.update_history()
        self.worker_pool.kill_client()
        if checkpoint and self.checkpoint:
            self.create_checkpoint()

        return self.pareto_set

    def __reduce__(self):
        state = self.__dict__.copy()
        del state["worker_pool"]
        return (
            self.__class__,
            (
                self.experiment_info,
                self.hyper_params,
                self.cooling_schedule,
                self.moocba,
            ),
            state,
        )

    def p_accept(self):
        return 1 - (exp(self.t_0 - self.temp) / self.temp)
