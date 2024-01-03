#!/bin/bash
#SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --time=24:00:00
#SBATCH --mem=64GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=126
#SBATCH --output=sbatch_out_files/slurm-%A_%a.out 
#SBATCH --error=sbatch_err_files/slurm-%A_%a.err

obj_fun="mh_distance_range"
cd /home/adeyemi.n/MH_Simulation/Inpatient\ Bed\ Allocation\ Optimization/
module load singularity/3.5.3
singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript "Code/Single_Obj_SA.R" $obj_fun