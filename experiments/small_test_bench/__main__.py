import sys
import os
sys.path.append('modules')

import hydra
import ray
from omegaconf import OmegaConf
from optimizers.DD_PUSA import DD_PUSA

@hydra.main(config_path = os.path.join(os.path.dirname(os.path.abspath(__file__))), 
            config_name='config')

def run_optim(cfg):
    
    if not ('num_processors' in globals() or 'num_processors' in locals()):
            num_workers = 8
        
    # Reserve one processor for the master/driver
    num_workers -= 1
    OmegaConf.update(cfg.experiment_info,"num_workers",num_workers,force_add=True) 
    optim_job = DD_PUSA(**cfg)
    optim_job.optimize()
        
if __name__ == '__main__':
    ray.init(runtime_env={'working_dir':"modules",'pip': ['pandas']})
    result = run_optim()
    ray.shutdown()