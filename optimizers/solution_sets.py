import numpy as np
import pandas as pd
from operator import itemgetter, attrgetter
from scipy import stats as stats 

class solution_set():
    def __init__(self,sol_set: list = []):
        self.set = sol_set
        self.length = len(self.set)
        
    def get_attribute(self, attr: str):
        attr_list = [getattr(i,attr) for i in self.set]
        if isinstance(attr_list[0], pd.DataFrame) or isinstance(attr_list,np.ndarray):
            attr_list = pd.concat(attr_list)    
            
    def update_length(self):
        self.length = len(self.set)
        
    def add_solution(self,sol):
        self.set.append(sol)
        self.update_length()
        
    def remove_solution(self,index: int = None):
        sol = self.set.pop(index)
        self.update_length()
        return sol
    
    def reorder(self,attr:str,decreasing=False):
        self.set = sorted(self.set,getattr(attr))
        if decreasing:
            self.set = self.set.reverse()
    
    def nondominated_sorting(self,noisy = True ):
        dominators = {}
        dominatees = {}
        
        for i in range(self.set[:-1]):
            for j in range(len(self.set)):
                for k in [i,j]:
                        if not k in dominators.keys:
                            dominators.update({k:[]})
                            
                        if not k in dominatees.keys:
                            dominatees.update({k:[]})
                            
                if not self.set[i] is self.set[j]:
                    crit_1 = self.set[i].soln_comparison(compare_sol = self.set[j], stat=noisy)
                    crit_2 = self.set[j].soln_comparison(compare_sol = self.set[i], stat=noisy)
                    if crit_1 and not crit_2:
                        dominators[j].append(i)
                        dominatees[i].append(j)
                    elif crit_2 and not crit_1:
                        dominators[i].append(j)
                        dominatees[j].append(i)
                        
        no_doms = {key: len(value) for key, value in dominators.items()}
        rank_list = []
        sol_assigned=[]
        rank_list.append(key for key, value in dominators.items if len(values) == 0)
        sol_assigned.append(len(rank_list[-1]))
        
        while sum([item for sublist in sol_assigned for item in sublist]) < len(self.set):
            q=[]
            no_sol_in_curr_front = sol_assigned[-1]
            if no_sol_in_curr_front > 0:
                for i in range(no_sol_in_curr_front):
                    sol_idx = rank_list[-1][i]
                    his_dominatess = dominatees[sol_idx]
                    for j in his_dominatess:
                        no_doms[i] -= 1
                        if no_doms[i] == 0:
                            q.append(i)
            rank_list.append(sorted(q))
            sol_assigned.append(len(q))

        
class solution():
    def __init__(self,optim_dirs: list = ['+']):
        self.replications = 0
        self.optim_dirs = optim_dirs
        self.n_obj = len(self.optim_dirs)
        self.data = None
        
    def is_maximize(self,index):
        return '+' in self.optim_dirs[index] or 'max' in self.optim_dirs[index]
    
    def update_data(self,new_data: pd.DataFrame):
        if self.data is None:
            self.data = new_data
        else: 
            self.data = self.data.append(new_data)
    
    def soln_comparison(self,compare_sol,stat=False):
    
        # Returns true if the compare_sol >> self
    
        res_list = np.array([])
        for i in range(self.n_obj):
            if stat:
                alt = 'greater' if self.is_maximize(i) else 'less'
                _, crit_1 = stats.ttest_ind(a = self.data.loc[:,i], b = compare_sol.data.loc[:,i])
                _, crit_2 = stats.ttest_ind(a = self.data.loc[:,i], b = compare_sol.data.loc[:,i],alternative = alt)
                crit_1 = crit_1 > self.alpha
                crit_2 = crit_2 < self.alpha
                res_list.append(crit_1 == False and crit_2 == True)   
            else:
                if self.is_maximize(i):
                    res_list.append(self.data[i] < compare_sol.data[i]) 
                else:
                    res_list.append(self.data[i] > compare_sol.data[i]) 
        return np.prod(res_list)
    
        