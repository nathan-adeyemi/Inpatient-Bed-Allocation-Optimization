import sys
import os
import pickle as pkl
import ray
import logging
sys.path.append('src')

from optimizers.DD_PUSA import DD_PUSA
from omegaconf import OmegaConf
from hydra.experimental import initialize, compose
from utils.utils import get_cpu_count
from pathlib import Path


def run_optim(check_dir=None,num_workers: int = 8):
    
    if check_dir is None:
        with initialize(config_path = '.', job_name = 'multi_obj_combo'):
            cfg = compose(config_name='config')
            
            
        
        OmegaConf.update(cfg.experiment_info,"num_workers",num_workers-1, force_add=True) 
        
        if cfg.experiment_info.report_progress.log_file is not None:
            log_path = Path(os.path.join(cfg.experiment_info.job_dir,'outputs',cfg.experiment_info.report_progress.log_file)).resolve()
            print(f'Output log: {log_path}')
            logging.basicConfig(filename=log_path,
                                encoding = 'utf-8',
                                level=logging.INFO,
                                format='%(asctime)s') 
        
        
        optim_job = DD_PUSA.dd_pusa(**cfg)
    else:
        with open(check_dir, 'rb') as f:
            optim_job = pkl.load(f)
        optim_job.create_workers()
            
    optim_job.optimize(reset = (check_dir is None))
        
if __name__ == '__main__':
    num_workers = get_cpu_count()
    
    ray.shutdown()
    ray.init(runtime_env={'working_dir':"src",'pip': ['pandas']},
             ignore_reinit_error=True,
             num_cpus=8)
    result = run_optim()
    ray.shutdown()