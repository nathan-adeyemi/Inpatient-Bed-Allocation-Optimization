import numpy as np
import subprocess
import multiprocessing
import os
import string
import secrets
from math import exp
import matplotlib.pyplot as plt
from optimizers.DD_PUSA import cooling_schedules as schedules


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
        
    assert int(np.sum(rounded_vector)) == int(np.sum(input_vector))

    return rounded_vector.astype(int)


def decode(sol: np.ndarray, capacities: dict = None):
    
    def sub_fun(x, y):
        
        ret_val = smart_round(x / np.sum(x) * y)

        return ret_val

    p0 = 0
    allocation = np.array([])
    for _, sub_dict in capacities.items():
        p1 = p0 + sub_dict["num_pools"]
        allocation = np.append(
            allocation, sub_fun(x=sol[p0:p1], y=sub_dict["total_capacity"])
        )
        p0 = p1

    return np.array(allocation)


# Function to execute the shell command
def execute_command(command):
    try:
        result = subprocess.run(command, check=True, capture_output=True, text=True)
        return result.stdout
    except subprocess.CalledProcessError as e:
        return f'Error executing the command: {e}\nOutput:\n{e.stdout.decode("utf-8")}'


def get_cpu_count():
    try:
        # Using os.cpu_count() to get the number of logical CPUs
        cpu_count = os.cpu_count()
        if cpu_count is not None:
            return cpu_count
        else:
            # If os.cpu_count() returns None, use multiprocessing.cpu_count()
            return multiprocessing.cpu_count()
    except Exception as e:
        print(f"Error getting CPU count: {e}")
        return None


def generate_random_identifier(length=8):
    characters = string.ascii_letters + string.digits
    identifier = "".join(secrets.choice(characters) for _ in range(length))
    return identifier


def get_temp(sched, t_0, t_damp, it):
    return getattr(schedules, sched)(t_0=t_0, t_damp=t_damp, current_iteration=it)


def p_accept(t_0, t_damp, it, schedule):
    return 1 - exp(t_0 - get_temp(schedule, t_0, t_damp, it)) / exp(t_0)


def plot_probs(sched, t_0, t_damp, plot=True):
    probs = [
        p_accept(t_0=t_0, t_damp=t_damp, schedule=sched, it=i + 1) for i in range(100)
    ]
    if plot:
        plt.plot(probs)
        plt.show()
        plt.close()
    else:
        return probs
    
def plot_temps(sched, t_0, t_damp, plot=True):
    temps = [
        get_temp(t_0=t_0, t_damp=t_damp, sched=sched, it=i + 1) for i in range(100)
    ]
    if plot:
        plt.plot(temps)
        plt.show()
        plt.close()
    else:
        return temps
