#!/bin/bash
#SBATCH --partition=short
#SBATCH --time=)2:00:00
#SBATCH --nodes=1
#SBATCH --mem=128GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=40
#SBATCH --output=sbatch_out_files/testbench_optim/slurm-%A_%a.out 
#SBATCH --error=sbatch_out_files/testbench_optim/slurm-%A_%a.err

module load anaconda3/2022.05
cd /home/adeyemi.n/MH_Simulation/Inpatient_Bed_Allocation_Optimization
conda activate bed_allocation_proj

/home/adeyemi.n/.conda/envs/bed_allocation_proj/bin/python3.11 experiments/test_bench/__main__.py