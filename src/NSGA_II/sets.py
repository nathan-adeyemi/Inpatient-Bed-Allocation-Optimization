import numpy as np
import pandas as pd


from ..base.sets import solution_set
from parent_soln import parent_solution

class children_set(solution_set):
    def __init__(self,
                 soln_set: list = [],
                 obj_fns: list = [],
                 workers: worker_pool = None):
        super.__init__(sol_set = soln_set,
                       obj_fns = obj_fns)
        
            
    def total_replications(self):
        return np.sum(np.array(self.get_attribute("replications")))
        
    
    def generate_samples(self):
        job_list = []
        for sol in self.set:
            for _ in range(sol.pending_samples):
                job_list.append(
                    {"index": self.set.index(sol), "allocation": sol.allocation}
                )
            sol.replications += sol.pending_samples
            sol.pending_samples = 0
        results = self.worker_pool.workers.map(
            lambda worker, allocation: worker.generate_samples.remote(allocation),
            [job["allocation"].tolist() for job in job_list],
        )
        results = list(results)
        for idx in range(len(results)):
            self.set[job_list[idx]["index"]].update_data(new_data=results[idx])
        
        
    def crossover(self,crossover_prob: float = 0.5):
        parent_index_1 = parent_index_2 = 1
        # Rewrite to perform all cross over on the set itself
        x = self.parent_solution_set[parent_index_1]
        y = self.parent_solution_set[parent_index_2]
        
        for i in range(len(x)):
            if np.random.random() < self.crossover_prob:
                old_y = y[i]
                x[i] = y[i]
                x[i] = old_y
                
        for sol in (x,y):
            self.children_set.add_solution(
                parent_solution(sol_vector=sol,
                                obj_fns = self.obj_fns,
                                mutation_rate=self.mutation_rate)
                )
            
    def mutate_children(self):
        for sol in self.set:
            sol.mutate_solution()
        self.generate_samples()
            
class parent_soln_set(children_set):
    def __init__(self,
                 soln_set: list = [],
                 obj_fns: list = [],
                 workers: worker_pool = None):
        super.__init__(soln_set = soln_set,
                 obj_fns = obj_fns,
                 workers = workers)
        
        self.obj_fns_names = [i["sample_statistic"] for i in self.obj_fns]
        self.worker_pool = workers
        
    
    def calc_crowding_distance(self):
        
        # Need to correct to adjust for stochastic
        
        for sol in self.set:
            sol.crowding_distance = 0
            
        for m in range(self.obj_fns_names):
            m_range = np.ptp(self.get_attribute(m))
            self.reorder(m)
            self.set[0].crowdind_distance = np.inf
            self.set[self.length].crowdind_distance = np.inf
            
            for i in range(1,self.length - 1):
                self.set[i].crowding_distance = self.set[i].crowding_distance + (self.set[i+1].crowding_distance + self.set[i-1].crowding_distance)/m_range
                
    def choose_new_parents(self):
        pass
                
                