import numpy as np
import pandas as pd

from .solution import solution
from .base_class import BaseClass 

class solution_set(BaseClass):
    def __init__(self, 
                 sol_set: list = None,
                 obj_fns: list = None):
        self.set = [sol for sol in sol_set if sol is not None] if sol_set else []
        self.length = len(self.set)
        self.obj_fns_names = list(obj_fns.keys()) if obj_fns else []

    def get_attribute(self, attr: str):
        attr_list = [getattr(i, attr) for i in self.set]
        if attr in self.obj_fns_names:
            attr_list = np.array([i.mean_response.loc[attr] for i in self.set])
        elif isinstance(attr_list[0], pd.DataFrame):
            attr_list = pd.concat(attr_list)
        elif isinstance(attr_list, np.ndarray) or isinstance(attr_list[0], pd.Series):
            attr_list = pd.concat(attr_list, axis=1).T
        return attr_list

    def update_length(self):
        self.length = len(self.set)

    def add_solution(self, sol):
        self.set.append(sol)
        self.update_length()

    def remove_solution(self, index: int = None):
        sol = self.set.pop(index)
        self.update_length()
        return sol

    def reorder(self, attr: str, decreasing=False,):
        attrs = [float(i) for i in self.get_attribute(attr)]
        pairs = zip(self.set, attrs)
        pairs = sorted(pairs, key=lambda x: x[1])
        self.set = [i[0] for i in pairs]
        if decreasing:
            self.set.reverse()

    def nondominated_sorting(self, noisy=True, alpha: float = None):
        dominators = {}
        dominatees = {}

        for i in range(len(self.set[:-1])):
            for j in range(len(self.set)):
                for k in [i, j]:
                    if k not in dominators.keys():
                        dominators.update({k: []})

                    if k not in dominatees.keys():
                        dominatees.update({k: []})

                if  self.set[i] is not self.set[j]:
                    crit_1 = self.set[i].soln_comparison(
                        compare_sol=self.set[j], stat=noisy, alpha = alpha
                    )
                    crit_2 = self.set[j].soln_comparison(
                        compare_sol=self.set[i], stat=noisy, alpha = alpha
                    )
                    if crit_1 and not crit_2:
                        # If i dominates j
                        dominators[j].append(i)
                        dominatees[i].append(j)
                    elif crit_2 and not crit_1:
                        # If j dominate i
                        dominators[i].append(j)
                        dominatees[j].append(i)

        no_doms = {key: len(value) for key, value in dominators.items()}
        rank_list = [[key for key, value in dominators.items() if len(value) == 0]]
        sol_assigned = [len(rank_list[-1])]

        while np.array([sol_assigned]).sum() < len(self.set):
            q = []
            num_sol_in_curr_front = sol_assigned[-1]
            if num_sol_in_curr_front > 0:
                for i in range(num_sol_in_curr_front):
                    sol_idx = rank_list[-1][i]
                    his_dominatees = dominatees[sol_idx]
                    for j in his_dominatees:
                        no_doms[j] -= 1
                        if no_doms[j] == 0:
                            q.append(j)
            rank_list.append(sorted(q))
            sol_assigned.append(len(q))
        self.fronts = rank_list
        
        front_num = 0
        for front in self.fronts:
            front_num += 1
            for sol_ind in front:
                self.set[sol_ind].ranking = front_num

    def _to_dict(self):
        self_dict = {
            "set": [i._to_dict() for i in self.set],
            "length": self.length,
            "obj_fns_names": self.obj_fns_names
        }
        return self_dict
    
    @classmethod
    def _from_dict(cls,data, sol_class = solution):
        instance = super()._from_dict(data = data)
        instance.set = [sol_class._from_dict(data = i) for i in instance.set]
        return instance