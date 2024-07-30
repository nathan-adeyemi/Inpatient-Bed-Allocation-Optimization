import sys
import argparse
import json


from pathlib import Path
from os import PathLike

sys.path.append('src')
from r_communication import r_client
from DD_PUSA.DD_PUSA import dd_pusa
from utils import read_yaml, update_config
from processing import build_cluster, kill_cluster

def eval_fn(params):
    
    params.update({
        "cfg_path": Path(cfg.get('fn_cfg_path')),
        "cfg_name": cfg.get('fn_cfg_name'),
        "obj_fns": list(cfg.get("obj_fns").keys()),
    })
    
    return r_client(
                    input = params,
                    trigger_path="src/Simulations/r-trigger.sh")

def run_optim(
    worker_pool, 
    exp_cfg: dict = None, 
    mo_ocba_cfg: dict = None,
    hyperparam_cfg: dict = None,
    evaluation_fn = None, 
    checkpoint: str|PathLike = None):
    
    if not checkpoint:
        optim_job = dd_pusa(config = exp_cfg,
                            mo_ocba_cfg = mo_ocba_cfg,
                            hyperparam_cfg = hyperparam_cfg,
                            evaluation_fn = evaluation_fn,
                            worker_pool = worker_pool,
                            create_checkpoint = True)
    else:
        optim_job = dd_pusa.from_checkpoint(exp_dir = checkpoint,
                                            evaluation_fn = evaluation_fn,
                                            workers = worker_pool)
            
    optim_job.optimize(reset = (checkpoint is  None))

if __name__ == '__main__':    
    
    with open("src/Simulations/sim_info.json", 'r') as f:
        sim_info = json.load(f)
    
    parser = argparse.ArgumentParser(
        description="Execute the ED arrival scaling paramter tuning"
    )
    parser.add_argument('--backend',
                        '-b',
                        choices=list(read_yaml("configs/cluster.yaml").keys()).append('single'),
                        action='store',
                        default='multiprocessing'
    )
    
    parser.add_argument('--exp-name',
                        "-e",
                        default='debug',
                        action='store',
                        )
    
    parser.add_argument(
        "--cluster-config",
        "-c",
        default='debug',
        action='store'
    )
    parser.add_argument("--num-workers",
                        "-nw",
                        default=1,
                        action='store')
    
    parser.add_argument(
        "--checkpoint-path",
        action='store',
        default=None
    )
    
    parser.add_argument(
        "--yaml-path",
        action='store',
        default="/home/adeyemi.n/MH_Simulation/Inpatient_Bed_Allocation_Optimization/configs/experiments.yaml"
    )
    
    args = parser.parse_args()
    
    exp_cfg = read_yaml(args.yaml_path).get(args.exp_name)
    mo_ocba_cfg = read_yaml("src/DD_PUSA/configs/moocba.yaml").get(exp_cfg.get("mo_ocba"))
    hyperparam_cfg = read_yaml("src/DD_PUSA/configs/hyper_params.yaml").get(exp_cfg.get('hyper_params'))
    
    cfg = update_config(exp_cfg = exp_cfg, 
                        exp_name=args.exp_name)
    
    workers = build_cluster(cli_args=args)
    if args.checkpoint_path:
        result = run_optim(
            worker_pool=workers,
            checkpoint=args.checkpoint_path,
            evaluation_fn=eval_fn
            )
    else:
        result = run_optim(exp_cfg=cfg,
                        mo_ocba_cfg=mo_ocba_cfg,
                        hyperparam_cfg=hyperparam_cfg,
                        evaluation_fn = eval_fn,
                        worker_pool = workers)
        
    kill_cluster(worker = workers)
