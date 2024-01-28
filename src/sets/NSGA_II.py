import numpy as np
import pandas as pd

from utils.utils import smart_round
from utils.r_communication import worker_pool

from math import lcm
from .base import solution_set


class parent_solns(solution_set):
    def __init__(self,
                 soln_set: list = [],
                 obj_fns: list = [],
                 workers: worker_pool = None):
        super.__init__(sol_set = soln_set,
                       obj_fns = obj_fns)
        
    def crossover_fn(self):
        pass