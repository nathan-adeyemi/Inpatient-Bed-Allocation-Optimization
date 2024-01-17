import math

def exponential(t_0: float, t_damp: float, current_iteration: float):
    if t_damp > 0:    
        t_damp *= -1
    return t_0 * current_iteration ** t_damp

def quadratic(t_0: float, t_damp: float, current_iteration: float):
    return t_0 / (1 + t_damp * current_iteration ** 2)

def logarithmic(t_0: float, t_damp: float, current_iteration: float):
    return t_0 / (1 + t_damp * math.log(1+ current_iteration))

def linear(t_0: float, t_damp: float, current_iteration: float):
    return t_0 / (1 + t_damp * current_iteration)