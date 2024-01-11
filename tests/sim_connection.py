import sys
import ray
import pandas as pd
import numpy as np
sys.path.append("modules")

from ray.util.actor_pool import ActorPool
from ray.runtime_env import RuntimeEnv
from utils.r_utils.r_communication import r_sim_client
from omegaconf import OmegaConf

if __name__ == '__main__':
    ray.init(runtime_env={'working_dir': "modules",
                        #   'conda': "modules/Simulations/sim_conda_env.yml"})
                        'pip': ['pandas']})

    # ray.init(runtime_env=RuntimeEnv(working_dir=".", pip = ['pandas']))
    n_workers = 6
    workers = ActorPool([r_sim_client.remote(OmegaConf.create({'size': 'Small', 'sh_path': 'Simulations/testbeds/simulation_trigger.sh'})) for _ in range(n_workers)])  
    
    # Initial test simulation replications
    result = workers.map(lambda worker, data: worker.generate_samples.remote(data), [np.array([4,4,4]) for _ in range(n_workers)])
    test_df = list(result)
    test_df = pd.concat(test_df)
    print(test_df)
    
    # Run another round of simulation replications to ensure the R session never dies
    result = workers.map(lambda worker, data: worker.generate_samples.remote(data), [[4,4,4] for _ in range(n_workers)])
    test_df = pd.concat(list(result))
    print(test_df)
    _ = workers.map(lambda worker, kill: worker.terminate_client.remote(kill), [None,None])
    ray.shutdown()