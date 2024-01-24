import math
import pickle as pkl
import sys
import ray
import os

sys.path.append("src")

from pathlib import Path
from hydra.experimental import compose, initialize
from omegaconf import OmegaConf  # noqa: E402
from optimizers.DD_PUSA import DD_PUSA  # noqa: E402
from ray import train, tune
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

        res_dict = {
            "total_simulation_replications": optim_dict["Total Replications"],
            "pareto_set_size": optim_dict["Pareto Set Length"],
        }
        res_dict.update(column_ranges(optim_dict["Estimated Pareto Front"]))

        return res_dict

    def save_checkpoint(self, directory):
        self.optim_job.create_checkpoint(check_dir=directory)
        return directory

    def load_checkpoint(self, directory):
        with open(f"{directory}.pkl", "rb") as f:
            self.optim_job = pkl.load(f)
        self.optim_job.create_workers()

    def cleanup(self):
        self.optim_job.terminate_experiment(checkpoint=False)


def flatten_list(nested_list):
    flattened = []
    for item in nested_list:
        if isinstance(item, list):
            flattened.extend(flatten_list(item))
        else:
            flattened.append(item)
    return flattened


def update_and_delete_fields(current_dict: dict = None, tune_fn=None, keys_path=None):
    # Navigate to the specified nested key path
    for key in keys_path:
        current_dict = current_dict[key]

    # Update the value
    if "min" in list(current_dict.keys()):
        current_dict["value"] = tune_fn(
            lower=current_dict["min"],
            upper=current_dict["max"],
            q=current_dict["interval"],
        )
        keys_to_delete = ["min", "max", "interval"]

    elif "grid" in list(current_dict.keys()):
        keys_to_delete = "grid"
        current_dict["value"] = tune_fn(current_dict["grid"])

    # Delete specified keys
    for key in keys_to_delete:
        if key in current_dict:
            del current_dict[key]


def execute_tune_job(trainer, path: str = None, num_workers=1):
    if path is None:
        with initialize(config_path=".", job_name="parameter_tuning"):
            cfg = compose(config_name="config")
            check_int = cfg.experiment_info.checkpoint.interval
            OmegaConf.update(
                cfg, "experiment_info.num_workers", num_workers, force_add=True
            )
            cfg = OmegaConf.to_object(cfg)

            param_list = [
                (("hyper_params", "n_sols"), tune.qrandint),
                (("hyper_params", "tabu_limit"), tune.qrandint),
                (("hyper_params", "max_tweak"), tune.quniform),
                (("moocba", "initial_reps_per_sol"), tune.qrandint),
                (("moocba", "replications_per_sol_x_iter"), tune.qrandint),
                (("moocba", "psi_target"), tune.grid_search),
            ]

            for key_set, tune_func in param_list:
                update_and_delete_fields(
                    current_dict=cfg, tune_fn=tune_func, keys_path=key_set
                )

            cfg["experiment_info"]["cooling_schedule"] = tune.grid_search(
                ["exponential", "quadratic", "linear", "logarithmic"]
            )

            cfg["experiment_info"]["config_path"] = Path(
                "src/optimizers/DD_PUSA/configs"
            ).resolve()

        bundle_list = [
            {"CPU": 1},
            {"CPU": num_workers},
        ]  # Reserve a single CPU for the Tune job execution

        trainable_w_resources = tune.with_resources(
            trainable=trainer,
            resources=tune.PlacementGroupFactory(bundles=bundle_list, strategy="PACK"),
        )

        tuner = tune.Tuner(
            trainable_w_resources,
            tune_config=tune.TuneConfig(
                metric="total_simulation_replications", mode="min", num_samples=5
            ),
            param_space=cfg,
            run_config=train.RunConfig(
                stop={"training_iteration": 5},
                storage_path=Path("experiments/parameter_tuning").resolve(),
                name="results",
                checkpoint_config=train.CheckpointConfig(
                    num_to_keep=1,
                    checkpoint_frequency=check_int,
                    checkpoint_at_end=True,
                ),
            ),
        )
        results = tuner.fit()
    else:
        results = tune.Tuner.restore(path=path, trainable=trainer)

    with open(
        os.path.join(
            Path("experiments/parameter_tuning/").resolve(), 
            "final_tune_results.pkl"
        ),
        "wb",
    ) as f:
        pkl.dump(results, f)


if __name__ == "__main__":
    if not ("num_processors" in globals() or "num_processors" in locals()):
        total_workers = get_cpu_count()

    num_workers_per_trial = 10

    ray.init(runtime_env={"working_dir": "src", "pip": ["pandas"]})
    results = execute_tune_job(
        trainer=mh_sim_trainable, num_workers=num_workers_per_trial
    )
    ray.shutdown()
