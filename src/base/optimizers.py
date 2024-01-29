import os
import pickle as pkl

from omegaconf import DictConfig
from abc import abstractmethod

class popMultiObjOptim():
    
    # Base class for population based multi-objective simulation optimizers
    
    def __init__(self, config: DictConfig):
        self.term_crit = config.termination_criteria
        if 'num_variables' in config.keys():
            self.num_variables = config.num_variables
        self.sols_per_iter = config.n_sols
        self.obj_fns = config.obj_fns
        self.iteration = 0
        self.checkpoint =config.checkpoint.create_checkpoints
        self.checkpoint_num = 0
        if self.checkpoint:
            self.checkpoint_num = 0
            self.checkpoints_dir = os.path.join(config.job_dir,'outputs','checkpoints')
            self.checkpoint_interval = config.checkpoint.interval
            if not os.path.exists(self.checkpoints_dir):
                os.makedirs(self.checkpoints_dir)
    
    @abstractmethod 
    def reset(self):
        pass
    
    def check_termination_condition(self):
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

    def create_checkpoint(self, check_dir: str = None):
        
        if check_dir is None:
            self.checkpoint_num += 1
            check_dir = os.path.join(self.checkpoints_dir,f"checkpoint_{self.checkpoint_num}")
        # self.update_checkpoint_metrics()
        with open(f"{check_dir}.pkl",'wb') as f:
            pkl.dump(self, f)
    
    @abstractmethod
    def update_checkpoint_metrics(self):
        pass
    
    
    @abstractmethod
    def execute_iteration(self):
        pass
    
    
    def optimize(self,reset = True):
        if reset:
            self.reset()
            
        while not self.check_termination_condition():
            self.execute_iteration()
            if self.checkpoint:
                if self.iteration % self.checkpoint_interval == 0:
                    self.create_checkpoint()
        self.terminate_experiment()
    
    @abstractmethod
    def terminate_experiment(self):
       pass

    
        