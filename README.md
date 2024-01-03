# Inpatient-Bed-Allocation-Optimization

This repository holds code for Distance-Driven Pareto-Updating Simulated Annealing, a novel multi-objective simulation-optimization algorithm, proposed our working paper titled *Balancing Equity and Efficiency: A Multi-Objective Simulation-Optimization Approach for Redistribution of Psychiatric Inpatient Beds* and sample queueing networks to test the algorithm performance.

## Files/Folders

`Code/Test_Bed_Optimization.R`: executes the DD-PUSA algorithm to solve bi-objective simulation optimization problem on a selected test network size.

`Code/Full_Sim_Optimization.R`: executes the DD-PUSA algorithm to solve the inpatient bed redistribution simulation optimization problem with the full Minn. psychiatric patient transfer network DES.

`Code/Test_Bed_NSGA_II.R`: executes the a lightly modified version of the NSGA-II algorithm (Deb et. al 2002)

`Code/Test_Bed_Setup.R`: configures the bi-objective optimization problem solved with `Code/Test_Bed_Optimization.r`

`Code/Jackson Network Test Bench.R`: configures the inverted-V Open-Jackson queueing network simulation for the chosen test scenario size.

`Data/Test Inverted V Networks/`: directory holding dataframes detailing each of the test scenario queueing networks used during numerical experiments (Section 5.1)

`Functions/`: directory holding all custom functions called in scripts

* `Functions/DD_PUSA.R`: implementation of the DD-PUSA algorithm

* `Functions/mcnsga_2.R`: modified parallel implementation of the NSGA-II algorithm

`Simulations/`: directory holding code for the Minn. psychiatric patient transfer network DES. and accompanying input parameters
