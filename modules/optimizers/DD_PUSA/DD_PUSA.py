import numpy as np 
import logging
import os

from omegaconf import DictConfig
from utils.utils import decode
from utils.r_utils.r_communication import worker_pool
from math import exp, ceil
from sets.DD_PUSA import pareto_set, candidate_set
from solution.DD_PUSA import mo_ocba_solution
from ..base import popMultiObjOptim
from ..DD_PUSA import cooling_schedules as schedules

        
class dd_pusa(popMultiObjOptim):
    
    def __init__(self, 
                 experiment_info  : DictConfig,
                 hyper_params: DictConfig,
                 cooling_schedule: DictConfig,
                 moocba: DictConfig = None):
        super().__init__(config=experiment_info)
        self.cool_sched = experiment_info.cooling_schedule
        self.t_0 = cooling_schedule.t_0
        self.t_damp = cooling_schedule.t_damp.value
        self.use_moocba = experiment_info.use_moocba
        self.stoch_optim = experiment_info.stoch_optim
        self.sim_dict = experiment_info.capacity_dictionary
        self.num_workers = experiment_info.num_workers
        self.obj_fns = experiment_info.obj_fns
        if self.stoch_optim:
            self.alpha = hyper_params.solution_comparison_alpha
        else:
            self.alpha = None
        self.pareto_limit = hyper_params.pareto_limit.value
        self.tabu_limit = hyper_params.tabu_limit.value
        self.tweak_limit = hyper_params.max_tweak.value
        if self.use_moocba:
            self.psi_target = moocba.psi_target.value
            self.replications_per_iter = moocba.replications_per_sol_x_iter.value
            self.init_reps = moocba.initial_reps_per_sol.value
        self.history = []
        self.report_progress = experiment_info.report_progress.print_to_console
        if self.report_progress:
            self.report_interval = experiment_info.report_progress.interval
            self.logging = not experiment_info.report_progress.log_file is None
            if self.logging:
                logging.basicConfig(filename=os.path.join(experiment_info.job_dir,
                                                          experiment_info.report_progress.log_file),
                                    level=logging.INFO,
                                    format='%(asctime)s')
        
    def reset(self):
        self.pareto_set = pareto_set(self.stoch_optim)
        self.cummulative_samples = 0
        self.tested_allocations = []
        self.temp = self.t_0
        self.current_iteration = 0
        if 'test_bench' in list(self.sim_dict.keys())[0]:
            self.sim_size = self.sim_dict[list(self.sim_dict.keys())[0]]['size']
        else:
            self.sim_size = 'ed_to_ip'
        self.worker_pool = worker_pool(num_workers=self.num_workers,sim_info={'size': self.sim_size,'obj_fns': self.obj_fns})
        
    def terminate_experiment(self):
        term_states = []
        for criteria in self.term_crit:
            crit = criteria['criteria']
            crit_val = criteria['value']
            test = criteria['condition']
            
            if "." in crit:
                term_states.append(eval(f"self.{crit} {test} {crit_val}")) 
            else:
                term_states.append(eval(f"getattr(self,'{crit}') {test} {crit_val}")) 
                
        return any(term_states) 

    
    def generate_candidate(self):

        def fix_cell(cell):
            if cell < 0:
                cell = abs(cell)
            elif cell > 1:
                cell = (1 - cell) + 1
            return cell
           
        counter = 0
        tweak_lim = self.tweak_limit * (self.temp / self.t_0)
        acceptance_prob = exp(self.t_0 - self.temp)/exp(self.t_0)
                
        while True:
            # Loop candidate generation until a valid solution is generated or the counter ends    
            if not self.pareto_set.best is None:
                changes = np.random.uniform(-tweak_lim, tweak_lim, self.num_variables)
                changes = np.array([0 if np.random.uniform(0,1,1) < acceptance_prob else i for i in changes])
                new_sol = np.array([fix_cell(i) for i in (self.pareto_set.best.solution + changes)])
            else:
                new_sol = np.random.uniform(0,1,self.num_variables)

            allocation = decode(new_sol,self.sim_dict)
            if not any(np.array_equal(allocation,i) for i in self.tested_allocations):
                self.tested_allocations.append(allocation)
                return mo_ocba_solution(solution=new_sol, 
                                        alpha=self.alpha,
                                        allocation=allocation,
                                        init_reps=self.init_reps,
                                        obj_fns = self.obj_fns)
            else:
                counter += 1
                
            if counter > 20:
                break
    
    def mo_ocba(self,sol_set):
        replication_limit = (self.replications_per_iter * sol_set.length) - sol_set.total_replications()
        k_val = ceil(self.sols_per_iter/3)
        if sol_set.length < k_val:
            k_val = sol_set.length
        
        sol_set.procedure_I(delta = self.replications_per_iter)
        sP = candidate_set(candidates = [sol_set.set[i] for i in np.argsort(np.array(sol_set.get_attribute('psi')[:k_val]))],
                           workers = self.worker_pool)
        reps_used = 0
        while min(sP.get_attribute('psi')) > self.psi_target:
            
            sP.procedure_II(sol_set=sol_set,K = k_val, delta=self.replications_per_iter)
            sP.reorder('delta_psi_d',decreasing=True)
            if any(i == 0 for i in sP.get_attribute('delta_psi_d')):
                k_test = max([0,min([index for index, value in enumerate(sP.get_attribute('delta_psi_d')) if value == 0])])
            else: 
                k_test = sP.length-1
                
            psi_ref = sP.set[k_test].delta_psi_d
            reps_used += sP.allocate_replications(delta=min(self.replications_per_iter,replication_limit),
                                                          K=k_test, 
                                                          delta_ref=(psi_ref * self.replications_per_iter)/np.sum(np.abs(np.array(sP.get_attribute('delta_psi_d')[:k_test]))),
                                                          psi_ref=psi_ref)
            sP.generate_samples()
            
            
            self.mo_ocba_reps += reps_used
            replication_limit -= reps_used
            
            if replication_limit <= 0:
                break
            
            sol_set.procedure_I(delta = self.replications_per_iter)
        return sP.set, list(set(sol_set.set).difference(set(sP.set)))
    
    def cool_temp(self):
            self.temp = getattr(schedules,self.cool_sched)(t0 = self.t_0, t_damp = self.t_damp, current_iteration = self.current_iteration)
            
    def update_history(self):
        self.history.append({
            'Iteration': self.current_iteration,
            'Iteration Replications': self.sample_counter,
            'Temperature': self.temp,
            'Pareto Set Length': self.pareto_set.length,
            'Rejected Allocations': [i.allocation for i in self.rejects],
            'Estimated Pareto Front:': self.pareto_set.get_attribute('mean_response')})
        
        
    def print(self,message):
        if self.logging:
            logging.info(message)
        else:
            print(message)
        
    
    def results_to_console(self):
        messages = [f'Iteration {self.current_iteration} required {self.sample_counter} replications.',
        f"Current pareto set is unchanged for {self.pareto_set.counter} iterations.",
        f'There are {self.pareto_set.length} estimated solutions in the pareto set.',
        'Current Pareto Front:\n {}'.format(self.pareto_set.get_attribute("mean_response").sort_values(by = list(self.pareto_set.set[0].data.columns)).to_string())]
        if self.use_moocba:
            messages.append(f'M.O.O.C.B.A allocated {self.mo_ocba_reps} more replications.')
        for m in messages:
            self.print(m)
        
    
    def execute_iteration(self):
        self.current_iteration += 1
        self.candidate_solutions = candidate_set(candidates = [self.generate_candidate() for _ in range(self.sols_per_iter)],workers=self.worker_pool)
        self.candidate_solutions.generate_samples()
        self.sample_counter = np.sum(np.array([sol.replications for sol in self.candidate_solutions.set]))
        if len(self.candidate_solutions.set) > 0:
            if self.use_moocba:
                self.mo_ocba_reps = 0
                self.candidate_solutions, self.rejects = self.mo_ocba(self.candidate_solutions)
                self.sample_counter += self.mo_ocba_reps
            for sol in self.candidate_solutions:
                self.pareto_set.add_solution(sol)
            self.pareto_set.update()
            self.pareto_set.find_best()
            self.update_history()
            if self.report_progress:
                if self.report_interval % self.current_iteration == 0: 
                    self.results_to_console()
        self.sample_counter = 
        self.cool_temp()

       
        