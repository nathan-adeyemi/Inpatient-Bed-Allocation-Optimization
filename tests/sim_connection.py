import os
import sys
sys.path.append("Inpatient_Bed_Allocation_Optimization/modules")
os.chdir('../..')


import utils.r_utils.r_communication as r_client
from omegaconf import OmegaConf

x = r_client.r_sim_client(sim_info=OmegaConf.create({'size':'Small','sh_path': 'Inpatient_Bed_Allocation_Optimization/modules/Simulations/testbeds/simulation_trigger.sh'}))

test_result = x.generate_samples(allocation=str([3,5,1]))
print(test_result)
x.terminate_client()