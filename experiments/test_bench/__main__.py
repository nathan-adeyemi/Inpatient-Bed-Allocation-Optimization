import sys
import os
import pickle as pkl
import ray
sys.path.append('src')

from optimizers.DD_PUSA import DD_PUSA
from omegaconf import OmegaConf
from hydra.experimental import initialize, compose


# @hydra.main(config_path = os.path.join(os.path.dirname(os.path.abspath(__file__))), 
#             config_name='config')

def run_optim(check_dir=None):
    
    if check_dir is None:
        with initialize(config_path = '.', job_name = 'multi_obj_combo'):
            cfg = compose(config_name='config')
            
        if not ('num_processors' in globals() or 'num_processors' in locals()):
                num_workers = 8
            
        # Reserve one processor for the master/driver
        num_workers -= 1
        OmegaConf.update(cfg.experiment_info,"num_workers",num_workers,force_add=True) 
        optim_job = DD_PUSA.dd_pusa(**cfg)
    else:
        with open(check_dir, 'rb') as f:
            optim_job = pkl.load(f)
        optim_job.create_workers()
            
    optim_job.optimize(reset = False)
        
if __name__ == '__main__':
    ray.init(runtime_env={'working_dir':"src",'pip': ['pandas']})
    result = run_optim()
    ray.shutdown()