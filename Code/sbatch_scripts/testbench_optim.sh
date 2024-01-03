#!/bin/bash
#SBATCH --partition=long
#SBATCH --time=2-00:00:00
#SBATCH --mem=64GB
#SBATCH --ntasks=1
# SBATCH --array=0-2
#SBATCH --cpus-per-task=85
#SBATCH --output=sbatch_dd_pusa/slurm-%j_%A.out 
#SBATCH --error=sbatch_dd_pusa/slurm-%j_%A.err

cd /home/adeyemi.n/MH_Simulation/Inpatient\ Bed\ Allocation\ Optimization/
# size=('Small' 'Medium' 'Large')
size="Large"

# Run DD-PUSA on testbench problem with 2 objectives
obj_function_list="TB_obj_1,TB_obj_2"
optim_type="max","min"

module load singularity/3.5.3
singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript "/home/adeyemi.n/MH_Simulation/Inpatient Bed Allocation Optimization/Code/Test_Bed_Optimization.r" ${size[$SLURM_ARRAY_TASK_ID]} $obj_function_list $optim_type
