import numpy as np

from omegaconf import DictConfig
from utils.utils import decode
from utils.r_communication import worker_pool
from ..base import popMultiObjOptim
from parent_soln import parent_solution
from sets import parent_soln_set, children_set


class nsga_ii(popMultiObjOptim):
    def __init__(self, experiment_info: DictConfig, params: DictConfig):
        super().__init__(config=experiment_info)

        self.experiment_info = experiment_info
        self.params = params

        # Set the hyper parameters
        self.population_size = self.params.pop_size.value
        self.crossover_prob = self.params.crossover_prob.value
        self.mutation_rate = self.params.mutation_rate.value
        self.solution_comparison_alpha = self.params.solution_comparison_alpha.value
        self.tournament_size = self.params.tournament_size.value

    def reset(self):
        self.cummulative_samples = 0
        self.current_iteration = 0
        self.create_workers()
        self.solution_population = parent_soln_set(obj_fns = self.experiment_info.obj_fns,
                                                    worker_pool = self.worker_pool)
        
        # Create solutions for the first iteration
        for _ in range(self.population_size):
            sol = self.generate_candidate()
            self.solution_population.add_solution(sol)
            
        # Begin iteration 0
        self.solution_population.generate_samples()
        self.solution_population.nondominated_sorting()
        self.solution_population.calc_crowding_distance()
        

    def create_workers(self):
        self.worker_pool = worker_pool(
            num_workers=self.num_workers,
            sim_info={"size": self.sim_size, "obj_fns": self.obj_fns},
        )

    def generate_candidate(self):
        counter = 0
        while True:
            new_sol = np.random.uniform(0, 1, self.num_variables)
            allocation = decode(new_sol, self.sim_dict)

            if not any(
                np.array_equal(allocation, i) for i in self.tested_allocations
            ):
                self.tested_allocations.append(allocation)
                return parent_solution(
                    solution=new_sol,
                    obj_fns=self.obj_fns,
                    mutation_rate=self.mutation_rate
                )
            else:
                counter += 1

            if counter > 40:
                break            

    def execute_iteration(self):
        self.current_iteration += 1
        mating_pool = self.tournament_selection()
        mating_pool.crossover(crossover_prob = self.crossover_prob)
        mating_pool.mutate_children() # Also generates samples
        for sol in mating_pool.set:
            self.solution_population.add_solution(sol)
        self.solution_population.nondominated_sorting()
        self.solution_population.calc_crowding_distance()
        self.solution_population.choose_new_parents()
        self.print()
        
    def tournament_selection(self) -> children_set():
        pass
    
    def print(self):
        pass
    