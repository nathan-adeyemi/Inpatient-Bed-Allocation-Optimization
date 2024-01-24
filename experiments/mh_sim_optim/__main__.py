import sys
import ray
import os
import math
import pickle as pkl
sys.path.append('src')

from ray import tune, train
from ray.tune.search.basic_variant import BasicVariantGenerator
from hydra.experimental import initialize, compose
from omegaconf import OmegaConf
from itertools import combinations
from pathlib import Path

from optimizers.DD_PUSA import DD_PUSA
from utils.r_utils.r_communication import worker_pool
from utils.utils import get_cpu_count
from utils.stats import column_ranges


class mh_sim_trainable(tune.Trainable):
        
    def setup(self, cfg):
        self.cfg = OmegaConf.create(cfg)
        self.optim_job = DD_PUSA.dd_pusa(**self.cfg)
        self.optim_job.reset()
        
    def step(self):
        if not self.optim_job.check_termination_condition():
            self.optim_job.execute_iteration()
        
        optim_dict = self.optim_job.update_history(return_val=True)
            
        res_dict = {'total_simulation_replications': optim_dict['Total Replications'],
                'pareto_set_size': optim_dict['Pareto Set Length'],
                'pareto_counter': optim_dict['Pareto Set Counter'],
                'temp' : optim_dict['Temperature']
                }
                
                # Call the metrics for stopping conditions
        res_dict.update(column_ranges(optim_dict['Estimated Pareto Front']))
        
        return res_dict
    
    def save_checkpoint(self, directory):
        self.optim_job.create_checkpoint(check_dir = directory)
        return directory
        
    def load_checkpoint(self,directory):
        with open(f"{directory}.pkl", 'rb') as f:
            self.optim_job = pkl.load(f)
        self.optim_job.create_workers()
            
    def cleanup(self):
        self.optim_job.terminate_experiment(checkpoint = False)
            
    
def flatten_list(nested_list):
    flattened = []
    for item in nested_list:
        if isinstance(item, list):
            flattened.extend(flatten_list(item))
        else:
            flattened.append(item)
    return flattened
    

def execute_tune_job(trainer, path: str = None, num_workers = None, concurrent_trials= None):
    
    def stop_fn(trial_id: str, results: dict):
        
        ret_val = [
            results['training_iteration'] > cfg.experiment_info.termination_criteria[0]['value'],
            results['temp'] < cfg.experiment_info.termination_criteria[1]['value'],
            results['pareto_counter'] > cfg.experiment_info.termination_criteria[2]['value']
        ]
        
        return any(ret_val)
    
    if path is None:
            #obj_fn_combos = ['mh_wait_quantile','mh_wait_sigma','mh_distance_range','mh_total_throughput']
        obj_fn_combos = [{'sample_statistic': 'TB_obj_1', 'direction': '+'},
                        {'sample_statistic': 'TB_obj_2', 'direction': '-'},
                        {'sample_statistic': 'TB_obj_3', 'direction': '-'}]
        obj_fn_combos = flatten_list([list(combinations(obj_fn_combos,i)) for i in range(2,len(obj_fn_combos)+1)])
        
        
        with initialize(config_path = '.',
                        job_name = 'multi_obj_combo'):
            cfg = compose(config_name='config')
            check_int = cfg.experiment_info.checkpoint.interval
            OmegaConf.update(cfg,'experiment_info.num_workers',num_workers,force_add=True)
            OmegaConf.update(cfg,'experiment_info.obj_fns',tune.grid_search(obj_fn_combos),merge = False)
            cfg = OmegaConf.to_object(cfg)
            
        bundle_list = [{'CPU': math.floor(num_workers/len(obj_fn_combos))}] # Reserves 1 CPU for the Tune job
            
        trainable_w_resources = tune.with_resources(trainable=trainer, 
                                                resources=tune.PlacementGroupFactory(bundles=bundle_list,
                                                strategy='PACK'))
    
        tuner = tune.Tuner(trainable_w_resources,
                        tune_config=tune.TuneConfig(num_samples=1),
                        param_space=cfg,
                        run_config=train.RunConfig(stop=stop_fn,
                                                    storage_path=Path('experiments/mh_sim_optim').resolve(),
                                                    name = 'test_obj_combos',
                                                    checkpoint_config=train.CheckpointConfig(checkpoint_frequency=check_int)))
        results = tuner.fit()
    else: 
        results = tune.Tuner.restore(path = path, trainable = trainer)
        
    return results
        
if __name__ == '__main__':
    ray.init(runtime_env={'working_dir':"src",'pip': ['pandas']})
    execute_tune_job(trainer=mh_sim_trainable, num_workers = get_cpu_count() - 1)
    ray.shutdown()