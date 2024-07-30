import numpy as np 
import pandas as pd
import json 

from statistics import NormalDist
from stats import bhattacharyya_dist
from base.solution import solution
from .utils import decode
from copy import deepcopy

class mo_ocba_solution(solution):
    
    def __init__(self,
                 solution: np.ndarray = None, 
                 init_reps: int = None,
                 obj_fns: list = None,
                 decode_sols: bool = True):
        super().__init__(obj_fns=obj_fns, solution = solution)
        
        self.pending_samples = init_reps
        self.replications = 0
        self.sol_rep = solution
        if decode_sols and solution is not None:
            self.sol_rep = deepcopy(self.solution)
            self.solution = decode(self.sol_rep, decode_sols)
        
    def procedure_I(self,sol_set,delta):
        self.psi = 0
        for sol in sol_set:
            if self is not sol:
                self.psi += self.get_p_val(sol,delta)
                
    def procedure_II(self,sol_set,delta,K,include_all=True):
        psi_id = []
        
        if include_all:
            for sol in sol_set:
                if self is not sol:
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
    
    def _to_dict(self):
        self_dict = super()._to_dict()
        self_dict.update({
            'pending_samples': self.pending_samples,
            'replications': self.replications,
            'psi': self.psi
        })
        if self.sol_rep is not None:
            self_dict.update(
                {'sol_rep': self.sol_rep }
            )
        return self_dict