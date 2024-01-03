#!/bin/bash
#
#SBATCH --array=0-1
#SBATCH --job-name=Small_hyperparameter_search
#SBATCH --output=sbatch_out_files/slurm-%A_%a.out 
#SBATCH --error=sbatch_err_files/slurm-%A_%a.err
#SBATCH --time=03:00:00
#SBATCH --mem=16G
#SBATCH --cpus-per-task=30

module load singularity/3.5.3
# singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript "/home/adeyemi.n/MH_Simulation/Inpatient Bed Allocation Optimization/.Rprofile"
singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript "_rslurm_Small_hyperparameter_search/slurm_run.R"
