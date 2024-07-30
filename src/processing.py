import ray.util.multiprocessing as ray_mp
import utils
import os
import ray
import time
import multiprocessing as mp

from pathlib import Path
from dask_jobqueue import SLURMCluster
from dask.distributed import Client

def build_cluster(cli_args):    
    client = None
    
    if not cli_args.backend == "single":
        cluster_cfg = (
            utils.read_yaml(Path("configs/cluster.yaml").resolve())
            .get(cli_args.backend)
            .get(cli_args.cluster_config)
        )
        if cluster_cfg.get('log_directory'):
            cluster_cfg["log_directory"] = os.path.join(
                cluster_cfg.get("log_directory"), cli_args.exp_name, cli_args.cluster_config
            )

    if cli_args.backend == 'ray':
        ray.init(**cluster_cfg)
        client = ray_mp.Pool()
        
    if cli_args.backend == "dask":
        num_workers = int(cli_args.num_workers)
        # Set up Dask distr cluster and client
        cluster = SLURMCluster(**cluster_cfg)
        cluster.scale(num_workers)

        client = Client(cluster, timeout="600s")
        wait_for_cluster(
            client=client,
            expected_workers=num_workers,
            timeout=7200,
            check_interval=15,
        )
    elif cli_args.backend == 'multiprocessing':
        client = mp.Pool()
    return client

def wait_for_cluster(client, expected_workers, 
                    timeout=600, 
                    check_interval=10):
    start_time = time.time()
    try:
        while True:
            # Get the number of currently connected workers
            n_workers = len(client.scheduler_info()["workers"])

            if n_workers >= expected_workers:
                print(f"Cluster is ready with {n_workers} workers.")
                break

            # Check for timeout
            elapsed_time = time.time() - start_time
            if elapsed_time > timeout:
                if n_workers > 0:
                    print(f"Timeout waiting for Dask cluster to be ready.\n Running experiment with {n_workers} workers")
                else:
                    raise TimeoutError("Timeout waiting for Dask cluster to be ready. No workers available")
            time.sleep(check_interval)
        
        if (time.time() % (check_interval * 5) == 0):
            print(
                f"Waiting for cluster to be ready... Currently {n_workers} workers connected."
            )
    except TimeoutError as e:
        print(e)
            # Wait before checking again
            
def kill_cluster(worker, args):
    if args.backend == 'ray':
        ray.shutdown()
    elif args.backend == 'dask':
        worker.close()
    elif args.backend == 'multiproccesing':
        worker.terminate()
    else:
        pass