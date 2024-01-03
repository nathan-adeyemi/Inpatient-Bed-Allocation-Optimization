#!/bin/bash
#SBATCH --partition=long
#SBATCH --time=5-00:00:00
#SBATCH --array=0-2

# SBATCH --time=24:00:00
# SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --mem=64GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=126
#SBATCH --output=sbatch_full_sim/slurm-%A_%a.out 
#SBATCH --error=sbatch_full_sim/slurm-%A_%a.err

# Parameter lists to be passed to each node in the sbatch array
obj_function_list=("mh_distance_range,mh_wait_quantile,mh_wait_sigma" "mh_distance_range,mh_wait_quantile,mh_total_throughput" "mh_distance_range,mh_wait_quantile,mh_wait_sigma,mh_total_throughput")
optim_type=("min,min,min" "min,min,max" "min,min,min,max")

# obj_function_list="mh_distance_range,mh_wait_quantile,mh_wait_sigma"
# optim_type="min,min,min"

# Logical indicating to continue a previous algorithm run of begin a new run. Only one should be commmented out
# continue_previous="TRUE"
# continue_previous="FALSE"
continue_previous=("TRUE" "TRUE" "FALSE")

cd /home/adeyemi.n/MH_Simulation/Inpatient\ Bed\ Allocation\ Optimization/
module load singularity/3.5.3
singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript "Code/Full_Sim_Optimization.r" ${obj_function_list[$SLURM_ARRAY_TASK_ID]} ${optim_type[$SLURM_ARRAY_TASK_ID]} ${continue_previous[$SLURM_ARRAY_TASK_ID]}
# singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript "Code/Full_Sim_Optimization.r" $obj_function_list $optim_type $continue_previous