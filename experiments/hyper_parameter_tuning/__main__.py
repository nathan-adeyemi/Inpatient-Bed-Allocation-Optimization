import math
import pickle as pkl
import sys

import ray

sys.path.append("src")

from pathlib import Path
from hydra.experimental import compose, initialize
from omegaconf import OmegaConf  # noqa: E402
from optimizers.DD_PUSA import DD_PUSA  # noqa: E402
from ray import train, tune


class mh_sim_trainable(tune.Trainable):
    def setup(self, cfg):
        self.cfg = OmegaConf.create(cfg)
        self.optim_job = DD_PUSA.dd_pusa(**self.cfg)
        self.optim_job.reset()

    def step(self):
        if not self.optim_job.check_termination_condition():
            self.optim_job.execute_iteration()

        return {"sample_stats_per_sol": self.optim_job.update_history(return_val=True)}

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


def execute_tune_job(
    trainer, path: str = None, num_workers=1, concurrent_trials: int = 2
):
    if path is None:

        with initialize(config_path=".", job_name="multi_obj_combo"):
            cfg = compose(config_name="config")
            check_int = cfg.experiment_info.checkpoint.interval
            OmegaConf.update(
                cfg, "experiment_info.num_workers", num_workers, force_add=True
            )
            cfg = OmegaConf.to_object(cfg)

            # Update the config with search spaces for the tuneable paramters
            cfg["hyper_params"]["n_sols"]["value"] = tune.qrandint(
                lower=cfg["hyper_params"]["n_sols"]["min"],
                upper=cfg["hyper_params"]["n_sols"]["max"],
                q=cfg["hyper_params"]["n_sols"]["interval"],
            )

            cfg["hyper_params"]["tabu_limit"]["value"] = tune.qrandint(
                lower=cfg["hyper_params"]["tabu_limit"]["min"],
                upper=cfg["hyper_params"]["tabu_limit"]["max"],
                q=cfg["hyper_params"]["tabu_limit"]["interval"],
            )

            cfg["hyper_params"]["pareto_limit"]["value"] = tune.qrandint(
                lower=cfg["hyper_params"]["pareto_limit"]["min"],
                upper=cfg["hyper_params"]["pareto_limit"]["max"],
                q=cfg["hyper_params"]["pareto_limit"]["interval"],
            )

            cfg["hyper_params"]["max_tweak"]["value"] = tune.quniform(
                lower=cfg["hyper_params"]["max_tweak"]["min"],
                upper=cfg["hyper_params"]["max_tweak"]["max"],
                q=cfg["hyper_params"]["max_tweak"]["interval"],
            )

            cfg["moocba"]["initial_reps_per_sol"]["value"] = tune.qrandint(
                lower=cfg["moocba"]["initial_reps_per_sol"]["min"],
                upper=cfg["moocba"]["initial_reps_per_sol"]["max"],
                q=cfg["moocba"]["initial_reps_per_sol"]["interval"],
            )

            cfg["moocba"]["replications_per_sol_x_iter"]["value"] = tune.qrandint(
                lower=cfg["moocba"]["replications_per_sol_x_iter"]["min"],
                upper=cfg["moocba"]["replications_per_sol_x_iter"]["max"],
                q=cfg["moocba"]["replications_per_sol_x_iter"]["interval"],
            )

            cfg["hyper_params"]["solution_comparison_alpha"][
                "value"
            ] = tune.grid_search(
                cfg["hyper_params"]["solution_comparison_alpha"]["grid"]
            )

            cfg["moocba"]["psi_target"]["value"] = tune.grid_search(
                cfg["moocba"]["psi_target"]["grid"]
            )

            cfg["experiment_info"]["cooling_schedule"] = tune.grid_search(
                ["exponential", "quadratic", "linear", "logarithmic"]
            )

            cfg["experiment_info"]["config_path"] = Path(
                "src/optimizers/DD_PUSA/configs"
            ).resolve()

        bundle_list = [{"CPU": 1}]  # Reserve a single CPU for the Tue job execution
        for _ in range(concurrent_trials):
            bundle_list.append({"CPU": num_workers})

        trainable_w_resources = tune.with_resources(
            trainable=trainer,
            resources=tune.PlacementGroupFactory(bundles=bundle_list, strategy="PACK"),
        )

        tuner = tune.Tuner(
            trainable_w_resources,
            tune_config=tune.TuneConfig(num_samples=1),
            param_space=cfg,
            run_config=train.RunConfig(
                stop={"training_iteration": 5},
                storage_path=Path("experiments/hyper_parameter_tuning").resolve(),
                name="test_obj_combos",
                checkpoint_config=train.CheckpointConfig(
                    checkpoint_frequency=check_int
                ),
            ),
        )
        results = tuner.fit()
    else:
        results = tune.Tuner.restore(path=path, trainable=trainer)

    return results


if __name__ == "__main__":
    if not ("num_processors" in globals() or "num_processors" in locals()):
        total_workers = 7

    concurrent_trials = 4
    num_workers = math.floor(total_workers / concurrent_trials)

    ray.init(runtime_env={"working_dir": "src", "pip": ["pandas"]})
    execute_tune_job(
        trainer=mh_sim_trainable,
        num_workers=num_workers,
        concurrent_trials=concurrent_trials,
    )
    ray.shutdown()
