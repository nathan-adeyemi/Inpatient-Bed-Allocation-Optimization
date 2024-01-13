import os
import pickle as pkl

from omegaconf import DictConfig
from abc import abstractmethod

class popMultiObjOptim():
    
    # Base class for population based multi-objective simulation optimizers
    
    def __init__(self, config: DictConfig):
        self.term_crit = config.termination_criteria
        self.num_variables = config.num_variables
        self.sols_per_iter = config.n_sols
        self.obj_fns = config.obj_fns
        self.iteration = 0
        self.checkpoint =config.checkpoint.create_checkpoints
        if self.checkpoint:
            self.checkpoint_num = 0
            self.checkpoints_dir = os.path.join(config.job_dir,'outputs','checkpoints')
            self.checkpoint_interval = config.checkpoint.interval
            if not os.path.exists(self.checkpoints_dir):
                os.makedirs(self.checkpoints_dir)
    
    @abstractmethod 
    def reset(self):
        pass

    def create_checkpoint(self):
        
        self.checkpoint_num += 1
        # self.update_checkpoint_metrics()
        with open(os.path.join(self.checkpoints_dir,f"checkpoint_{self.checkpoint_num}"),'wb') as f:
            pkl.dump(self, f)
    
    @abstractmethod
    def update_checkpoint_metrics(self):
        pass
    
    
    @abstractmethod
    def execute_iteration(self):
        pass
    
    
    def optimize(self):
        self.reset()
        while not self.terminate_experiment():
            self.execute_iteration()
            if self.checkpoint:
                if self.iteration % self.checkpoint_interval == 0:
                    self.create_checkpoint()

    
    @abstractmethod
    def terminate_experiment(self):
       pass

    
        