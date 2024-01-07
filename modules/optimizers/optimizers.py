import numpy as np 
import pandas as pd 
import pickle as pkl
import os
import math
import ray
from omegaconf import OmegaConf
import math

from ray.util.actor_pool import ActorPool
from statistics import NormalDist
from .solution_sets import solution_set, solution
from utils.utils import smart_round, decode
from utils.r_utils.r_functions.stats import bhattacharyya
from utils.r_utils.r_communication import r_sim_client
from abc import ABC, abstractmethod


class popMultiObjOptim():
    
    # Base class for population based multi-objective simulation optimizers
    
    def __init__(self, config: OmegaConf.DictConfig):
        self.term_crit = config.termination_criteria
        self.num_variables = config.num_variables
        self.sols_per_iter = config.n_sols
        self.optim_dirs = config.optim_dirs
        self.iteration = 0
        if config.checkpoint:
            self.create_checkpoints = self.checkpoint
            self.checkpoint_num = 0
            self.checkpoints_dir = config.checkpoint.directory
            self.checkpoint_interval = config.checkpoint.interval
            if not os.path.exists(self.checkpoints_dir):
                os.makedirs(self.checkpoints_dir)
    
    @abstractmethod 
    def reset(self):
        pass
    

    def create_checkpoint(self):
        
        self.checkpoint_num += 1
        self.update_checkpoint_metrics()
        with open(os.path.join(self.checkpoints_dir,f"checkpoint_{self.checkpoint_num}"),'wb') as f:
            pkl.dump(self.checkpoint_metrics, f)
    
    @abstractmethod
    def update_checkpoint_metrics(self):
        pass
    
    
    @abstractmethod
    def execute_iteration(self):
        pass
    
    
    def optimize(self):
        self.reset()
        while self.terminate_experiment():
            self.execute_iteration()
            if self.iteration % self.checkpoint == 0 and self.checkpoint:
                self.create_checkpoint()

    
    @abstractmethod
    def terminate_experiment(self):
       pass
        
class DD_PUSA(popMultiObjOptim):
    
    def __init__(self, 
                 experiment_info  : OmegaConf.DictConfig,
                 hyper_params: OmegaConf.DictConfig):
        super.__init__(config=experiment_info)
        self.t_0 = hyper_params.cooling_schedule.t_0
        self.t_damp = hyper_params.cooling_schedule.t_damp
        self.pareto_limit = hyper_params.pareto_limit
        self.tabu_limit = hyper_params.tabu_limit
        self.tweak_limit = hyper_params.max_tweaks
        self.use_mocba = experiment_info.use_mocba
        self.stoch_optim = experiment_info.stoch_optim
        self.sim_dict = experiment_info.capacity_dictionary
        if self.stoch_optim:
            self.alpha = hyper_params.solution_comparison_alpha
        else:
            self.alpha = None
            
        if self.use_mocba:
            self.psi_target = hyper_params.mocba.psi_target
            self.replications_per_iter = hyper_params.mocba.replications_per_sol_x_iter 
            self.num_ocba_best_sols = max([self.num_sols_per_iteration, hyper_params.mocba.set_size_lim])
        self.history = []
        
    
    def reset(self):
        self.pareto_counter = 0
        self.pareto_set = pareto_set(self.stoch_optim)
        self.replication_counter = 0
        self.tested_allocations = []
        self.temp = self.t_0
        self.current_iteration = 0
        
        
    def terminate_experiment(self):
        term_states = []
        for criteria in self.termination_criteria:
            if 'iteration' in criteria.keys():
                term_states.append(self.iteration < criteria['iteration'])
            elif 't_min' in criteria.keys():
                term_states.append(self.temp > criteria['t_min'])        

    
    def generate_candidate(self,input_sol):
        counter = 0
        tweak_lim = self.tweak_limit * (self.temp / self.t_0)
        
        def fix_cell(cell):
            if cell < 0:
                cell = math.abs(cell)
            elif cell > 1:
                cell = (1 - cell) + 1
            
                
        while True:
            # Loop candidate generation until a valid solution is generated or the counter ends    
            if not self.pareto_set.best is None:
                changes = np.random.uniform(-tweak_lim, tweak_lim, self.num_variable)
                changes = np.array([0 for i in changes if np.random.uniform(0,1,1) < self.acceptance_prob])
                new_sol = np.array([fix_cell(i) for i in (self.pareto_set.best.solution + changes)])
            else:
                new_sol = np.random.uniform(self.num_variables)

            if not decode(new_sol,self.total_capacity) in self.tested_allocations:
                return mo_ocba_solution(OmegaConf.create({'solution': new_sol, 'alpha': self.alpha}))
                
            else:
                counter += 1
                
            if counter > 20:
                break
        
    
    
    def mo_ocba(self,sol_set):
        replication_limit = self.replications_per_iter * sol_set.length
        k_val = math.ceil(self.n_sols/3)
        if sol_set.length < k_val:
            k_val = sol_set.length
        
        sol_set.procedure_I(delta = self.replications_per_iter)
        while replication_limit - sol_set.total_replications() > 0 and min(sol_set.get_attribute('psi') > self.psi_target):
            sP = [sol_set[i] for i in np.argsort(np.array(sol_set.get_attribute('psi')[:k_val]))]
            sP = candidate_set(sP,
                               num_workers = self.num_workers,
                               sim_info = self.sim_dict['size'])
            sP.procedure_II(sol_set=sol_set,K = k_val)
            sP.reorder('delta_psi_d',decreasing=True)
            if any(i == 0 for i in sP.get_attribute('delta_psi_d')):
                k_test = max([0,min([index for index, value in enumerate(sP.get_attribute('delta_psi_d')) if value == 0])])
            psi_ref = sP.set[k_test].delta_psi_d
            reps = sP.allocate_replications(delta=self.replications_per_iter,K=k_test,
                                            delta_ref=(psi_ref * self.replications_per_iter)/np.sum(np.abs(np.array(sP.get_attribute('delta_psi_d')[:k_test]))),psi_ref=psi_ref)
    
    def update_history(self):
        self.replication_counter += np.sum(np.array([sol.replications for sol in self.candidate_solutions]))
        self.history.append({
            'Iteration': self.current_iteration,
            'Temperature': self.temp,
            'Best Solution Objectives': self.pareto_set.best.data.mean(),
            'Solutions in Pareto Set': len(self.candidate_solutions.set)})
    
    def results_to_console(self):
        print(f'Iteration {self.history[self.current_iteration]["Iteration"]} required {self.replication_counter} \n')
        print('Current best has existed for {self.pareto_set.best.counter} iterations \n')
        print(pd.concat([sol.data.mean() for sol in pareto_set]))
    
    def execute_iteration(self):
        self.acceptance_prob = math.exp(self.t_0 - self.temp)/math.exp(self.t_0)
        self.candidate_solutions = candidate_set(sol_set = [self.generate_candidate(self.pareto_set.best.solution) for _ in range(self.n_sols)],
                                                 num_workers = self.num_workers,
                                                 sim_info=self.sim_info['size'])
        _ = [self.tested_allocations.append(sol.allocation) for sol in self.candidate_solutions]
        if len(self.candidate_solutions) > 0:
            if self.use_mocba:
                self.candidate_solutions = self.mocba(self.candidate_solutions)
            self.pareto_set.update(self.candidate_solutions)
            self.pareto_set.find_best()
            self.update_history()
            if self.report_progress:
                self.results_to_console()
            
               
class pareto_set(solution_set):
    def __init__(self,stoch_optim = True):
        super.__init__()
        self.previous_sols = []
        self.stoch_optim = stoch_optim
        self.best = None
        
    def update(self):
        front_dict = self.nondominated_sorting(noisy = self.stoch_optim)
        for i in len(self.set):
            if not i in front_dict[1]:
                self.previous_sols.append(self.remove_solution(i))
        
    def find_g_ideal(self):
        ideal_data = pd.concat([self.set[i].data.assign(candidate = i) for i in range(len(self.set))]).groupby(['candidate']).mean()
        ideal_sols = []
        for i in range(ideal_data.shape[1]):
            if self.pareto_set.best.is_maximize(i):
                ideal_sols.append(ideal_data[ideal_data.columns[i]].idmax())
            else:
                ideal_sols.append(ideal_data[ideal_data.columns[i]].idmin())
        self.g_ideal = pd.concat([self.set[ideal_sols[i]].data[ideal_data.columns[i]] for i in range(ideal_data.shape[1])])
        
    def find_best(self):
        self.find_g_ideal()
        distances = np.array([solution.calc_distance(ideal_dat=self.g_ideal) for solution in self.set])
        self.best = self.set[distances.argmin()]

class candidate_set(solution_set):
    def __init__(self,num_workers: int,sim_info: str = 'Small'):
        super().__init__()
        self.workers = ActorPool([r_sim_client.remote(sim_info) for _ in range(num_workers)])    
        
    def kill_client(self):
        ray.kill(self.r_client_handle)
        
    def total_replications(self):
        return np.sum(np.array(self.get_attribute('replications')))
        
    def procedure_I(self,delta):
        for sol in self.set:
            sol.procedure_I(delta)
            
    def procedure_II(self,sol_set,K):
        for sol in sol.set:
            sol.procedure_II(sol_set,K)
            
    def allocate_replications(self,delta,K,delta_ref,psi_ref):
        alloc_list = []
        for i in self.set:
            if self.set.index(i) == K:
                alloc_list.append(delta_ref)
            elif self.set.index(i) < K:
                alloc_list.append(i.delta_psi_d/psi_ref * delta_ref)
            else:
                alloc_list.append(0)
        alloc_list = np.array(alloc_list)
        alloc_list = smart_round(delta * (alloc_list/np.sum(alloc_list)))
        for i in self.set:    
            if alloc_list[self.set.index(i)] > 0:
                i.pending_samples = alloc_list[self.set.index(i)]
                
        return np.sum(alloc_list)
    
    def generate_samples(self):
        job_list = []
        for sol in self.set:
            for _ in range(sol.pending_samples):
                job_list.append({'index': self.set.index(sol),'allocation': sol.allocation})
            sol.replications += sol.pending_samples
            sol.pending_samples = 0
        results = self.workers.map(lambda worker, allocation: worker.generate_samples(allocation), [job['allocation'] for job in job_list])
        for idx in range(len(results)):
            self.set[job_list[idx]['index']].update_data(new_data=results[idx])
            
class mo_ocba_solution(solution):
    
    def __init__(self,cfg):
        super.__init__()
        self.alpha = cfg.alpha
        self.solution = cfg.solution
        self.allocation = self.decode(cfg.solution)
        
    def procedure_I(self,sol_set,delta):
        self.psi = 0
        for sol in sol_set:
            if not self is sol:
                self.psi += self.get_p_val(sol,delta)
                
    def procedure_II(self,sol_set,delta,K,include_all=True):
        psi_id = []
        
        if include_all:
            for sol in sol_set:
                if self is sol:
                    psi_id.append(self.get_p_val(compare_sol=sol,delta = delta) - self.get_p_val(compare_sol=sol,hat=True))
                else:
                    sv1 = 0
                    sv2 = 0
                    for sol2 in sol_set:
                        sv1 += self.get_p_val(self,compare_sol=sol2)
                        sv2 += self.get_p_val(self,compare_sol=sol2,self_hat=True)
                    psi_id.append(sv1-sv2)
        else:
            pass
        self.delta_psi_d = np.sum(np.array(psi_id[:K]))
            
                           
    def get_p_val(self,compare_sol,delta,hat=False,self_hat=False):
        x_bar = np.array(self.data.mean(axis = 1) - compare_sol.data.mean(axis = 1))
        sigma_estimates = self.est_sigma(compare=compare_sol,delta=delta,hat=hat,self_hat=self_hat)
        z_vals = np.array([])
        for i in self.n_obj:
            z = NormalDist(mu=0,sigma = sigma_estimates[i]).zscore(x_bar[i])
            if self.is_maximize(i):
                z_vals.insert(1-z)
            else:
                z_vals.insert(z)
        return np.prod(z_vals)
        
    def est_sigma(self,compare,delta,hat,self_hat):
        if self is compare:
            radicand = (self.data.std(axis=1).to_numpy() + (self.replications + delta)) + (compare.data.std(axis=1).to_numpy()/compare.replications)
        elif hat:
            radicand = (self.data.std(axis=1).to_numpy() + self.replications ) + (compare.data.std(axis=1).to_numpy()/compare.replications)
        else:
            radicand = (self.data.std(axis=1).to_numpy() + self.replications ) + (compare.data.std(axis=1).to_numpy()/(compare.replications + delta))
        return np.sqrt(radicand)
    
    def calc_distance(self,ideal_data: pd.DataFrame):
        return bhattacharyya(self.data,ideal_data)

       
        