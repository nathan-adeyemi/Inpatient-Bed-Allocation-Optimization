import socket
import json
import pandas as pd
import subprocess
import os
import ray

from omegaconf import DictConfig
from ray.util.actor_pool import ActorPool

class worker_pool():
    def __init__(self, sim_info: str, num_workers: int):
        self.num_workers = num_workers
        # Point to the correct shell script for initiating R
        if not 'ed_to_ip' in sim_info: 
            # Path is relative to the /modules folder
            shell_path = 'Simulations/testbeds/simulation_trigger.sh'
        else:
            shell_path = 'Simulations/ed_mh_simulation/simulation_trigger.sh'
        sim_info = DictConfig({'size': sim_info, 'sh_path': shell_path})
        self.workers = ActorPool([r_sim_client.remote(sim_info) for _ in range(self.num_workers)])  
          
        
    def kill_client(self):
        self.workers.map(lambda worker, kill: worker.terminate_client(kill), [None for _ in range(self.num_workers)])

@ray.remote(num_cpus = 1)    
class r_sim_client():
    def __init__(self, sim_info: DictConfig):
        self.initial_info = sim_info.size
        self.sh_path = sim_info.sh_path
        
        self.port = find_available_port()
        
        # Create a socket
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        # Bind the socket to a specific address and port
        server_address = ('localhost', self.port)
        self.server.bind(server_address)
        
        # Listen for incoming connections
        self.server.listen(1)
        
        # Begin the R simulation subprocess 
        subprocess_env = os.environ.copy()
        subprocess_env['port'] = str(self.port)
        subprocess_env['size'] = str(self.initial_info)

        try:
            self.process = subprocess.Popen(['bash', self.sh_path], env=subprocess_env)
        except Exception as e:
            print(f'Error starting the shell script: {e}')        
        
        # Accept the client connection
        self.client, _ = self.server.accept()

        self.data = None
        
    def _receive_client_data(self, json_format: bool = False, conv_float: bool = False):
        info = self.client.recv(1024).decode('utf-8')

        if json_format:
            # Parse the received JSON data
            try:
                info = pd.DataFrame(json.loads(info))
            except json.JSONDecodeError as e:
                info = f"Error decoding JSON: {e}"
        elif conv_float:
            info = float(info)
            
        return info
    
    def _send_client_data(self,data):
        if not isinstance(data,bytes):
            data = data.encode()
        self.client.sendall(data)
        
    def generate_samples(self,allocation):
        sim_info = allocation
        if not isinstance(sim_info,str):
            sim_info = str(sim_info)
        self._send_client_data(sim_info)
        return self._receive_client_data(json_format = True)
    
    def terminate_client(self, value: None):
        self.process.terminate()
        

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