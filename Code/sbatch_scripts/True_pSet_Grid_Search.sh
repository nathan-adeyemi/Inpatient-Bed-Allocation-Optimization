#!/bin/bash
#SBATCH --partition=long
#SBATCH --nodes=1
#SBATCH --time=4-00:00:00
#SBATCH --mem=256GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=126
#SBATCH --output=sbatch_grid_search/slurm-%A.out 
#SBATCH --error=sbatch_grid_search/slurm-%A.err

module load singularity/3.5.3
cd /home/adeyemi.n/MH_Simulation/Inpatient\ Bed\ Allocation\ Optimization/

# size="Small"
# size="Medium"
size="Large"

optim_type="max,min"
obj_function_list="TB_obj_1,TB_obj_2"
# obj_function_list="TB_obj_1,TB_obj_2,TB_obj_3"

singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript "Code/Inv_V_Grid_Search.R" $size $obj_function_list $optim_type 

