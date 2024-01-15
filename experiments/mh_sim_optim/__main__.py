import sys
import ray
import math
sys.path.append('modules')

from ray import tune
from ray import train
from ray.tune.search.basic_variant import BasicVariantGenerator
from hydra.experimental import initialize, compose
from optimizers.DD_PUSA import DD_PUSA
from omegaconf import OmegaConf
from itertools import combinations

def run_optim(cfg):
    cfg = OmegaConf.create(cfg)
    optim_job = DD_PUSA.dd_pusa(**cfg)
    optim_job.reset()
    while not optim_job.check_termination_condition():
        optim_job.execute_iteration()
        if optim_job.checkpoint:
            if optim_job.iteration % optim_job.checkpoint_interval == 0:
                optim_job.create_checkpoint()
        train.report(metrics = {'sample_stats_per_sol':optim_job.update_history(return_val=True)})
    optim_job.terminate_experiment()
    train.report(metrics = {'sample_stats_per_sol':optim_job.update_history(return_val=True)})
    
def flatten_list(nested_list):
    flattened = []
    for item in nested_list:
        if isinstance(item, list):
            flattened.extend(flatten_list(item))
        else:
            flattened.append(item)
    return flattened
        
if __name__ == '__main__':
    
    #obj_fn_combos = ['mh_wait_quantile','mh_wait_sigma','mh_distance_range','mh_total_throughput']
    obj_fn_combos = [{'sample_statistic': 'TB_obj_1', 'direction': '+'},
                     {'sample_statistic': 'TB_obj_2', 'direction': '-'},
                     {'sample_statistic': 'TB_obj_3', 'direction': '-'}]
    obj_fn_combos = flatten_list([list(combinations(obj_fn_combos,i)) for i in range(2,len(obj_fn_combos)+1)])
    
    
    # Setup the number of total processors and processors per job
    if not ('num_processors' in globals() or 'num_processors' in locals()):
        total_workers = 8
    else:
        total_workers = globals()['num_processors']
        
    num_workers = max(1,math.floor(total_workers/len(obj_fn_combos))) # Number of remote actors (per obj_fn_combo) to execute simulation replications
    
    with initialize(config_path = '.',
                    job_name = 'multi_obj_combo'):
        cfg = compose(config_name='config')
        OmegaConf.update(cfg,'experiment_info.num_workers',num_workers,force_add=True)
        OmegaConf.update(cfg,'experiment_info.obj_fns',tune.grid_search(obj_fn_combos),merge = False)
        cfg = OmegaConf.to_object(cfg)

    ray.init(runtime_env={'working_dir':"modules",'pip': ['pandas']})
    trainable_w_resources = tune.with_resources(trainable=run_optim, 
                                                resources=tune.PlacementGroupFactory(bundles=[{'CPU':1} ,
                                                                                              {'CPU': num_workers}],
                                                strategy='PACK'))
    
    tuner = tune.Tuner(trainable_w_resources,
                       tune_config=tune.TuneConfig(num_samples=1),
                       param_space=cfg,
                       run_config=train.RunConfig(stop={'training_iteration': 20}))
    results = tuner.fit()
    ray.shutdown()