import numpy as np
import pandas as pd
import json

from scipy.special import softmax
from math import lcm

from .utils import smart_round
from .mo_ocba_soln import mo_ocba_solution

from base.sets import solution_set
from base.solution import solution

class candidate_set(solution_set):
    def __init__(self, obj_fns: dict, candidates: list = [] ):
        super().__init__(sol_set=candidates, obj_fns=obj_fns)

    def total_replications(self):
        return np.sum(np.array(self.get_attribute("replications")))

    def procedure_I(self, delta):
        for sol in self.set:
            sol.procedure_I(sol_set=self.set, delta=delta)

    def procedure_II(self, sol_set, K, delta):
        for sol in sol_set.set:
            sol.procedure_II(sol_set=sol_set.set, delta=delta, K=K)

    def allocate_replications(self, delta, K, delta_ref, psi_ref):
        alloc_list = []
        for i in self.set:
            if self.set.index(i) == K:
                alloc_list.append(delta_ref)
            elif self.set.index(i) < K:
                alloc_list.append(i.delta_psi_d / psi_ref * delta_ref)
            else:
                alloc_list.append(0)
        alloc_list = np.array(alloc_list)
        alloc_list = smart_round(delta * (alloc_list / np.sum(alloc_list)))
        for i in self.set:
            if alloc_list[self.set.index(i)] > 0:
                i.pending_samples = alloc_list[self.set.index(i)]

        return np.sum(alloc_list)
    
    @classmethod
    def _from_dict(cls,data, sol_class = mo_ocba_solution):
        instance = super()._from_dict(data=data, sol_class = sol_class)
        return instance


class pareto_set(candidate_set):
    def __init__(
        self,
        set: list = [],
        obj_fns: list = None,
    ):
        super().__init__(candidates=set, obj_fns=obj_fns)
        self.best = None
        self.counter = 0

    def update(self,stochastic: bool = True, prev_set: list = None, alpha: float = None):
        self.nondominated_sorting(noisy=stochastic, alpha = alpha)
        for i in reversed(range(self.length)):
            if i not in self.fronts[0]:
                _ = self.remove_solution(i)
        if self.get_attribute("id") == prev_set:
            self.counter += 1
        else:
            self.counter = 0

    def find_g_ideal(self):
        ideal_data = (
            pd.concat(
                [self.set[i].data.assign(candidate=i) for i in range(len(self.set))]
            )
            .groupby(["candidate"])
            .mean()
        )
        n_samp = []
        idxs = []
        for i in self.obj_fns_names:
            if self.set[0].is_maximize(i):
                idx = ideal_data.loc[:, i].argmax()
            else:
                idx = ideal_data.loc[:, i].argmin()
            n_samp.append(self.set[idx].replications)
            idxs.append(idx)

        reps = lcm(*n_samp) / np.array(n_samp)
        self.g_ideal = pd.concat(
            [
                pd.concat(
                    [
                        self.set[idxs[self.obj_fns_names.index(col)]].data.loc[:, col]
                        for _ in range(int(reps[self.obj_fns_names.index(col)]))
                    ]
                ).reset_index(drop = True)
                for col in self.obj_fns_names
            ],
            axis=1,
        )

    def find_best(self):
        if self.length > 1:
            self.find_g_ideal()
            distances = np.array(
                [
                    solution.calc_distance(compare_data=self.g_ideal)
                    for solution in self.set
                ]
            )
            probs = softmax(distances)
            best = self.set[np.random.choice(self.length, 1, probs.tolist())[0]]
            if best is self.best:
                self.best_counter += 1
            else:
                self.best = best
                self.best_counter = 0
        else:
            self.best = self.set[0]
            self.best_counter = 0
            
    def _to_dict(self):
        self_dict = super()._to_dict()
        self_dict.update({
            'best': self.best.id,
            'length': self.length,
            'best_counter': self.best_counter,
            'counter': self.counter,
            'g_ideal': self.g_ideal.reset_index(drop = True).to_dict(),
            
        })
        return self_dict    
    
    @classmethod
    def _from_dict(cls,data, sol_class = mo_ocba_solution):
        instance = super()._from_dict(data=data, sol_class = sol_class)
        instance.best = instance.set[[i.id for i in instance.set].index(instance.best)]
        return instance