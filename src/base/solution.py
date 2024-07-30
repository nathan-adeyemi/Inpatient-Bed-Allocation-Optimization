import numpy as np
import pandas as pd

from scipy import stats as stats 
from utils import generate_random_identifier
from .base_class import BaseClass
        
class solution(BaseClass):
    def __init__(self,
                 solution =  None,
                 obj_fns: dict = None):
        
        if obj_fns is not None:
            self.obj_fns = obj_fns
            self.obj_fns_names = list(self.obj_fns.keys())
            self.n_obj = len(self.obj_fns)
            
        if solution is not None:
            self.solution = solution
            self.data = None
            self.id = generate_random_identifier(length=10)
        
    def is_maximize(self,col = None):
        
        return '+' in self.obj_fns.get(col).get('direction') or 'max' in self.obj_fns.get(col).get('direction')
    
    def update_data(self,new_data: pd.DataFrame = None):
        
        if isinstance(self.data, dict):
            self.data = pd.DataFrame(self.data)
        
        if self.data is None:
            self.data = new_data
        elif new_data is not None:
            self.data = pd.concat([self.data,new_data]).reset_index(drop=True)
        
        self.mean_response = self.data.loc[:,self.obj_fns_names].mean(axis=0)
        self.var = (self.data.loc[:,self.obj_fns_names].std(axis=0)**2)
    
    def soln_comparison(self,compare_sol,stat=False, alpha: float = None):
    
        # Returns true if the compare_sol >> self
    
        res_list = []
        for i in self.obj_fns_names:
            if stat:
                alt = 'greater' if self.is_maximize(i) else 'less'
                _, crit_1 = stats.ttest_ind(a = self.data.loc[:,i], b = compare_sol.data.loc[:,i])
                _, crit_2 = stats.ttest_ind(a = self.data.loc[:,i], b = compare_sol.data.loc[:,i],
                                            alternative = alt)
                crit_1 = crit_1 > alpha
                crit_2 = crit_2 < alpha
                res_list.append(crit_1 is False and crit_2 is True)   
            else:
                if self.is_maximize(i):
                    res_list.append(self.data[i] < compare_sol.data[i]) 
                else:
                    res_list.append(self.data[i] > compare_sol.data[i]) 
        return np.prod(np.array(res_list))
    
    
    def _to_dict(self):
        attributes_dict = {
            "obj_fns":self.obj_fns,
            "obj_fns_names": self.obj_fns_names ,
            'n_obj': self.n_obj ,
            "data": self.data.reset_index(drop = True).to_dict(),
            "id": self.id ,
            "solution": self.solution,
        }
        return attributes_dict    
    
    @classmethod
    def _from_dict(cls,data):
        instance = super()._from_dict(data = data)
        instance.obj_fns_names = list(instance.obj_fns.keys())
        instance.data = instance.update_data()
        return instance