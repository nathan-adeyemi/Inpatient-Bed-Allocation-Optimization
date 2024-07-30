# import matplotlib.pyplot as plt
import numpy as np

from math import exp
from . import cooling_schedules as schedules

def smart_round(input_vector):
    if not isinstance(input_vector, np.ndarray):
        input_vector = np.array(input_vector)
    rounded_vector = np.round(input_vector)
    rounding_error = int(np.ceil(np.sum(rounded_vector) - np.sum(input_vector)))

    # Adjust rounding error by adding or subtracting 1 from the smallest absolute rounding error
    if rounding_error > 0:
        indices_to_adjust = np.argpartition(
            np.abs(input_vector - rounded_vector), -rounding_error
        )[:rounding_error]
        rounded_vector[indices_to_adjust] -= 1
    elif rounding_error < 0:
        indices_to_adjust = np.argpartition(
            np.abs(input_vector - rounded_vector), rounding_error
        )[: abs(rounding_error)]
        rounded_vector[indices_to_adjust] += 1
        
    assert np.isclose(a = int(np.sum(rounded_vector)),
                      b = int(np.sum(input_vector)),
                      atol=1e-5)

    return rounded_vector.astype(int)


def decode(sol: np.ndarray, capacities: dict = None):
    
    def sub_fun(x, y):
        
        ret_val = smart_round(x / np.sum(x) * y)

        return ret_val

    p0 = 0
    allocation = np.array([])
    for _, sub_dict in capacities.items():
        p1 = p0 + sub_dict.get("nVar")
        allocation = np.append(
            allocation, sub_fun(x=sol[p0:p1], y=sub_dict.get("constraint_b"))
        )
        p0 = p1

    return np.array(allocation)


def get_temp(sched, t_0, t_damp, it):
    return getattr(schedules, sched)(t_0=t_0, t_damp=t_damp, current_iteration=it)


def p_accept(t_0, t_damp, it, schedule):
    return 1 - exp(t_0 - get_temp(schedule, t_0, t_damp, it)) / exp(t_0)


# def plot_probs(sched, t_0, t_damp, plot=True):
#     probs = [
#         p_accept(t_0=t_0, t_damp=t_damp, schedule=sched, it=i + 1) for i in range(100)
#     ]
#     if plot:
#         plt.plot(probs)
#         plt.show()
#         plt.close()
#     else:
#         return probs
    
# def plot_temps(sched, t_0, t_damp, plot=True):
#     temps = [
#         get_temp(t_0=t_0, t_damp=t_damp, sched=sched, it=i + 1) for i in range(100)
#     ]
#     if plot:
#         plt.plot(temps)
#         plt.show()
#         plt.close()
#     else:
#         return temps