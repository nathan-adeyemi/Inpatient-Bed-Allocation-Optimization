import numpy as np
import pandas as pd
import logging
import os

from omegaconf import OmegaConf, DictConfig
from utils.utils import decode, p_accept
from utils.r_communication import worker_pool
from ..base import popMultiObjOptim

class nsga_ii(popMultiObjOptim):
    def __init__(self,
              experiment_info: DictConfig,
              params: DictConfig):
        super().__init__(config=experiment_info)
        
        self.experiment_info = experiment_info
        self.params = params
        
        # Set the hyper parameters
        self.crossover_rate = self.params.crossover_rate.value
        self.mutation_rate = self.params.mutation_rate.value
        self.solution_comparison_alpha = self.params.solution_comparison_alpha.value
        
        def reset(self):
            self.cummulative_samples = 0
            self.current_iteration = 0
            self.create_workers
            pass
        
        def create_workers(self):
            self.worker_pool = worker_pool(
                num_workers=self.num_workers,
                sim_info={"size": self.sim_size, "obj_fns": self.obj_fns},
            )
            
        def execute_iteration(self):