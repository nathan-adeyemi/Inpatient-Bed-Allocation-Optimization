import sys
sys.append('MH_Simulation/Inpatient_Bed_Allocation_Optimization')

import hydra
import ray
import OmegaConf
import os
from optimizers import DD_PUSA

if __file__ == '__main__':
    cfg_path = os.path.join(os.path.dirname(os.abspath(__file__)),'config.yaml')
    cfg = OmegaConf.load(cfg_path)
    optim_job = DD_PUSA(cfg)
    while not optim_job.terminate_experiment():
        optim_job.execute_iteration()