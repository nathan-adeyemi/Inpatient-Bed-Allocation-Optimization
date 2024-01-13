import numpy as np 
import pandas as pd 

from utils.utils import smart_round
from utils.r_utils.r_communication import worker_pool
from scipy.special import softmax
from math import lcm
from .base import solution_set
from copy import deepcopy

class pareto_set(solution_set):
    def __init__(self,stoch_optim = True):
        super().__init__()
        self.stoch_optim = stoch_optim
        self.best = None
        self.counter = 0
        
    def update(self):
        old_p_set = set(deepcopy(self.set))
        self.nondominated_sorting(noisy = self.stoch_optim)
        for i in reversed(range(self.length)):
            if not i in self.fronts[0]:
                _ = self.remove_solution(i)
        if set(self.set) == old_p_set:
            self.counter +=1
        
    def find_g_ideal(self):
        ideal_data = pd.concat([self.set[i].data.assign(candidate = i) for i in range(len(self.set))]).groupby(['candidate']).mean()
        n_samp = []
        idxs = []
        for i in range(ideal_data.shape[1]):
            if self.set[0].is_maximize(i):
                idx = ideal_data.iloc[:,i].argmax()
            else:
                idx = ideal_data.iloc[:,i].argmin()
            n_samp.append(self.set[idx].replications)
            idxs.append(idx)
            
        reps = lcm(*n_samp)/np.array(n_samp)    
        self.g_ideal = pd.concat([pd.concat([self.set[idxs[i]].data.iloc[:,i] for _ in range(int(reps[i]))]) for i in range(ideal_data.shape[1])],axis=1)
        
    def find_best(self):
        if self.length > 1:
            self.find_g_ideal()
            distances = np.array([solution.calc_distance(compare_data=self.g_ideal) for solution in self.set])
            probs = softmax(distances)
            best = self.set[np.random.choice(self.length,1,probs.tolist())[0]]
            if best is self.best:
                self.best_counter += 1
            else:
                self.best = best
                self.best_counter = 0
        else:
            self.best = self.set[0]
            self.best_counter = 0


class candidate_set(solution_set):
    def __init__(self, workers: worker_pool,candidates: list = []):
        super().__init__(sol_set=candidates)
        self.worker_pool = workers
        
    def total_replications(self):
        return np.sum(np.array(self.get_attribute('replications')))
        
    def procedure_I(self,delta):
        for sol in self.set:
            sol.procedure_I(sol_set=self.set,delta=delta)
            
    def procedure_II(self,sol_set,K,delta):
        for sol in sol_set.set:
            sol.procedure_II(sol_set=sol_set.set,delta=delta,K = K)
            
    def allocate_replications(self,delta,K,delta_ref,psi_ref):
        alloc_list = []
        for i in self.set:
            if self.set.index(i) == K:
                alloc_list.append(delta_ref)
            elif self.set.index(i) < K:
                alloc_list.append(i.delta_psi_d/psi_ref * delta_ref)
            else:
                alloc_list.append(0)
        alloc_list = np.array(alloc_list)
        alloc_list = smart_round(delta * (alloc_list/np.sum(alloc_list)))
        for i in self.set:    
            if alloc_list[self.set.index(i)] > 0:
                i.pending_samples = alloc_list[self.set.index(i)]
                
        return np.sum(alloc_list)
    
    def generate_samples(self):
        job_list = []
        for sol in self.set:
            for _ in range(sol.pending_samples):
                job_list.append({'index': self.set.index(sol),'allocation': sol.allocation})
            sol.replications += sol.pending_samples
            sol.pending_samples = 0
        results = self.worker_pool.workers.map(lambda worker, allocation: worker.generate_samples.remote(allocation), [job['allocation'].tolist() for job in job_list])
        results = list(results)
        for idx in range(len(results)):
            self.set[job_list[idx]['index']].update_data(new_data=results[idx])