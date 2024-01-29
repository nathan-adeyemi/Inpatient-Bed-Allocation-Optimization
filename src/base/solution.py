import numpy as np
import pandas as pd

from scipy import stats as stats 
from utils.utils import generate_random_identifier
        
class solution():
    def __init__(self,sol_vector,obj_fns: list = ['+']):
        self.obj_fns = obj_fns
        self.obj_fns_names = [i['sample_statistic'] for i in self.obj_fns]
        self.n_obj = len(self.obj_fns)
        self.data = None
        self.id = generate_random_identifier(length=10)
        self.solution = sol_vector
        
    def is_maximize(self,col = None):
        
        if isinstance(col, str):
            col = self.obj_fns_names.index(col)
        
        return '+' in self.obj_fns[col]['direction'] or 'max' in self.obj_fns[col]['direction']
    
    def update_data(self,new_data: pd.DataFrame):
        if self.data is None:
            self.data = new_data
        else: 
            self.data = pd.concat([self.data,new_data])
        self.mean_response = self.data.loc[:,self.obj_fns_names].mean(axis=0)
        self.var = (self.data.loc[:,self.obj_fns_names].std(axis=0)**2)
    
    def soln_comparison(self,compare_sol,stat=False):
    
        # Returns true if the compare_sol >> self
    
        res_list = []
        for i in self.obj_fns_names:
            if stat:
                alt = 'greater' if self.is_maximize(i) else 'less'
                _, crit_1 = stats.ttest_ind(a = self.data.loc[:,i], b = compare_sol.data.loc[:,i])
                _, crit_2 = stats.ttest_ind(a = self.data.loc[:,i], b = compare_sol.data.loc[:,i],
                                            alternative = alt)
                crit_1 = crit_1 > self.alpha
                crit_2 = crit_2 < self.alpha
                res_list.append(crit_1 is False and crit_2 is True)   
            else:
                if self.is_maximize(i):
                    res_list.append(self.data[i] < compare_sol.data[i]) 
                else:
                    res_list.append(self.data[i] > compare_sol.data[i]) 
        return np.prod(np.array(res_list))