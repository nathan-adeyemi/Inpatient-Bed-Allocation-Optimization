import numpy as np
import socket
import pandas as pd

def smart_round(input_vector):
    if not isinstance(input_vector,np.ndarray):
        input_vector = np.array(input_vector)
    rounded_vector = np.round(input_vector)
    rounding_error = int(np.sum(rounded_vector) - np.sum(input_vector))

    # Adjust rounding error by adding or subtracting 1 from the smallest absolute rounding error
    if rounding_error > 0:
        indices_to_adjust = np.argpartition(np.abs(input_vector - rounded_vector), -rounding_error)[:rounding_error]
        rounded_vector[indices_to_adjust] -= 1
    elif rounding_error < 0:
        indices_to_adjust = np.argpartition(np.abs(input_vector - rounded_vector), rounding_error)[:abs(rounding_error)]
        rounded_vector[indices_to_adjust] += 1

    return rounded_vector.astype(int)


def decode(sol: np.ndarray, capacities: dict = None):
    
    def sub_fun(x,y):
        return smart_round(x/np.sum(x) * y)
    
    p0 = 0
    allocation = []
    for _ ,sub_dict in capacities.items():
        p1 = p0 + sub_dict['num_pools']
        allocation.append(sub_fun(x = sol[p0:p1],y = sub_dict['total_capacity']))
        p0 = p1
        
    return np.array(allocation)

# Function to find an available port
def find_available_port(print_port = False):
    # Create a socket
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Bind to a random port
    s.bind(('localhost', 0))

    # Get the actual port number
    _, port = s.getsockname()

    # Close the socket
    s.close()
    
    if(print_port):
        print(port)

    return port