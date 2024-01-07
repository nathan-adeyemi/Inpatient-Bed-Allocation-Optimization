import sys
import os
sys.path.append('modules')

from omegaconf import OmegaConf
from optimizers import DD_PUSA

if __file__ == '__main__':
    
    if not ('num_processors' in globals() or 'num_processors' in locals()):
        num_workers = 8
        
    # Reserve one processor for the master/driver
    num_workers -= 1
    
    cfg_path = os.path.join(os.path.dirname(os.abspath(__file__)),'config.yaml')
    cfg = OmegaConf.load(cfg_path)
    cfg.experiment_info.num_workers = num_workers
    optim_job = DD_PUSA(cfg)
    while not optim_job.terminate_experiment():
        optim_job.execute_iteration()