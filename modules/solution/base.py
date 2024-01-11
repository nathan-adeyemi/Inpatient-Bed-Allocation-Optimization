import numpy as np
import pandas as pd

from scipy import stats as stats 
        
class solution():
    def __init__(self,optim_dirs: list = ['+']):
        self.optim_dirs = optim_dirs
        self.n_obj = len(self.optim_dirs)
        self.data = None
        
    def is_maximize(self,index):
        return '+' in self.optim_dirs[index] or 'max' in self.optim_dirs[index]
    
    def update_data(self,new_data: pd.DataFrame):
        if self.data is None:
            self.data = new_data
        else: 
            self.data = pd.concat([self.data,new_data])
        self.mean_response = self.data.mean(axis=0)
        self.var = (self.data.std(axis=0)**2)
    
    def soln_comparison(self,compare_sol,stat=False):
    
        # Returns true if the compare_sol >> self
    
        res_list = []
        for i in range(self.n_obj):
            if stat:
                alt = 'greater' if self.is_maximize(i) else 'less'
                _, crit_1 = stats.ttest_ind(a = self.data.iloc[:,i], b = compare_sol.data.iloc[:,i])
                _, crit_2 = stats.ttest_ind(a = self.data.iloc[:,i], b = compare_sol.data.iloc[:,i],
                                            alternative = alt)
                crit_1 = crit_1 > self.alpha
                crit_2 = crit_2 < self.alpha
                res_list.append(crit_1 == False and crit_2 == True)   
            else:
                if self.is_maximize(i):
                    res_list.append(self.data[i] < compare_sol.data[i]) 
                else:
                    res_list.append(self.data[i] > compare_sol.data[i]) 
        return np.prod(np.array(res_list))