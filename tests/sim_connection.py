import sys
sys.path.append('/home/adeyemi.n/MH_Simulation/Inpatient_Bed_Allocation_Optimization/modules')

import utils.r_utils.r_communication as r_client
from omegaconf import OmegaConf

x = r_client.r_sim_client(initial_info=OmegaConf.create({'size':'Small','sh_path': '/home/adeyemi.n/MH_Simulation/Inpatient_Bed_Allocation_Optimization/Simulations/testbeds/simulation_trigger.sh'}))

test_result = x.generate_samples(allocation=str([3,5,1]))
print(test_result)