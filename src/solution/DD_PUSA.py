import numpy as np 
import pandas as pd 

from statistics import NormalDist
from utils.stats import bhattacharyya_dist
from .base import solution

class mo_ocba_solution(solution):
    
    def __init__(self,alpha: float, solution: np.ndarray, allocation: np.ndarray, init_reps: int, obj_fns: list):
        super().__init__(obj_fns=obj_fns)
        self.alpha = alpha
        self.solution = solution
        self.allocation = allocation
        self.pending_samples = init_reps
        self.replications = 0
        self.obj_fns = obj_fns
        self.obj_fns_names = [i['sample_statistic'] for i in self.obj_fns]

        
    def procedure_I(self,sol_set,delta):
        self.psi = 0
        for sol in sol_set:
            if not self is sol:
                self.psi += self.get_p_val(sol,delta)
                
    def procedure_II(self,sol_set,delta,K,include_all=True):
        psi_id = []
        
        if include_all:
            for sol in sol_set:
                if not self is sol:
                    psi_id.append(self.get_p_val(compare_sol=sol,delta=delta) - self.get_p_val(compare_sol=sol,delta=delta,hat=True))
                else:
                    sv1 = 0
                    sv2 = 0
                    for sol2 in sol_set:
                        sv1 += self.get_p_val(compare_sol=sol2,delta=delta)
                        sv2 += self.get_p_val(compare_sol=sol2,delta=delta,self_hat=True)
                    psi_id.append(sv1-sv2)
        else:
            pass
        self.delta_psi_d = np.sum(np.array(psi_id[:K]))
            
                           
    def get_p_val(self,compare_sol,delta,hat=False,self_hat=False):
        x_bar = np.array(self.mean_response - compare_sol.mean_response)
        sigma_estimates = self.est_sigma(compare=compare_sol,delta=delta,hat=hat,self_hat=self_hat)
        z_vals = []
        for i in self.obj_fns_names:
            z = NormalDist(mu=0,sigma = sigma_estimates.loc[i]).cdf(x_bar[self.obj_fns_names.index(i)])
            if self.is_maximize(col = i):
                z_vals.append(1-z)
            else:
                z_vals.append(z)
        return np.prod(np.array(z_vals))
        
    def est_sigma(self,compare,delta,hat,self_hat):
        
        if hat:
            radicand = (self.var/ self.replications ) + (compare.var/(compare.replications + delta))
        elif self_hat:
            radicand = (self.var/self.replications + delta) + (compare.var/compare.replications)
        else:
            radicand = (self.var/self.replications) + (compare.var/compare.replications)
        return np.sqrt(radicand)
    
    def calc_distance(self,compare_data: pd.DataFrame):
        return bhattacharyya_dist(self.data.loc[:,self.obj_fns_names],
                                  compare_data.loc[:,self.obj_fns_names])