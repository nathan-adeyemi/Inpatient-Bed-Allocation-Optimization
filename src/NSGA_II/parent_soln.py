import pandas as pd
import numpy as np

from base.solution import solution

class parent_solution(solution):
    def __init__(self, obj_fns: list = [],mutation_rate: float = 0.5):
        super.__init__(obj_fns = obj_fns)
        self.mutation_rate = mutation_rate
        
    def mutate_solutition(self):
        for i in range(len(self.solution)):
            if np.random.random() < self.mutation_rate:
                self.solution[i] = fix_cell(self.solution[i] + np.random.random())
    
    
def fix_cell(cell, lim: float = 1):
    if cell < 0:
        cell = abs(cell + lim)
    elif cell > lim:
        cell = (lim - cell) + lim
    return cell

        