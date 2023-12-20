import numpy as np 
import pandas as pd 
import pickle as pkl
import os

from OmegaConf import DictConfig
from .optimizers.solution_sets import solution_set, pareto_set

class popMultiObjOptim():
    
    # Base class for population based multi-objective simulation optimizers
    
    def __init__(self, config: DictConfig):
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
        
    def reset(self):
        pass
    

    def create_checkpoint(self):
        
        self.checkpoint_num += 1
        self.update_checkpoint_metrics()
        with open(os.path.join(self.checkpoints_dir,f"cehckpoint_{self.checkpoint_num}"),'wb') as f:
            pkl.dump(self.checkpoint_metrics, f)
    

    def update_checkpoint_metrics(self):
        pass
    
    
    def execute_iteration(self):
        pass
    
    
    def optimize(self):
        self.reset()
        while self.terminate_experiment():
            self.execute_iteration()
            if self.iteration % self.checkpoint == 0 and self.checkpoint:
                self.create_checkpoint()

    
    def terminate_experiment(self):
       pass
        
class DD_PUSA(popMultiObjOptim):
    
    def __init__(self, 
                 experiment_info  : DictConfig,
                 hyper_params: DictConfig):
        super.__init__(config=experiment_info)
        self.t_0 = hyper_params.cooling_schedule.t_0
        self.t_damp = hyper_params.cooling_schedule.t_damp
        self.pareto_limit = hyper_params.pareto_limit
        self.tabu_limit = hyper_params.tabu_limit
        self.use_mocba = experiment_info.use_mocba
        self.history = []
        
    
    def reset(self):
        self.pareto_counter = 0
        self.pareto_set = solution_set()
          
        
    def terminate_experiment(self):
        term_states = []
        for criteria in self.termination_criteria:
            if 'iteration' in criteria.keys():
                term_states.append(self.iteration < criteria['iteration'])
            elif 't_min' in criteria.keys():
                term_states.append(self.temp > criteria['t_min'])
                
    
    def generate_candidates(self):
        pass
    
    
    def mocba(self):
        pass
    
    
    def find_best(self):
        pass
    
    
    def update_history(self):
        pass
    
    
    def results_to_console(self):
        pass
    
    def execute_iteration(self):
        candidate_solutions = candidate_set()
        if len(candidate_solutions) > 0:
            if self.use_mocba:
                candidate_solutions = self.mocba(candidate_solutions)
            self.pareto_set.update(candidate_solutions)
            self.find_best()
            self.update_history()
            if self.report_progress:
                self.results_to_console()
            
               
class pareto_set(solution_set):
    def __init__(self):
        super.__init__()
        
    def reorder(self):
        pass
    
    def update(self):
        
        # Input code for updating the solution set 
        self.reorder()
        

class candidate_set(solution_set):
    def __init__(self):
        super().__init__()
            