#!/bin/bash
#SBATCH --partition=long
#SBATCH --time=1-12:00:00
#SBATCH --array=0-3
#SBATCH --mem=128GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=120
#SBATCH --output=sbatch_out_files/parameter_tuning/slurm-%A_%a.out 
#SBATCH --error=sbatch_out_files/parameter_tuning/slurm-%A_%a.err

module load anaconda3/2022.05
cd /home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access
conda activate bed_allocation_proj

