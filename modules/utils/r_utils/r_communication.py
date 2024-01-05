import socket
import json
import pandas as pd
import subprocess
import os
from omegaconf import DictConfig

from utils.utils import find_available_port, execute_command

# @ray.remote(num_cpus = 1)    
class r_sim_client():
    def __init__(self, sim_info: DictConfig):
        self.initial_info = sim_info.size
        self.sh_path = sim_info.sh_path
        
        self.port = find_available_port(print_port = True)
        
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
            print(f'Shell script started asynchronously.')
        except Exception as e:
            print(f'Error starting the shell script: {e}')        
        
        # Accept the client connection
        self.client, _ = self.server.accept()
    
        # Set up server side port connection to transmit actions to the R simulation

        # self.client.sendall(self.size.encode())
        # self.n_queues = self._receive_client_data(conv_float = True)
        # self.n_queues = int(self.n_queues)
        self.data = None
        
    def _receive_client_data(self, json_format: bool = False, conv_float: bool = False):
        info = self.client.recv(1024).decode('utf-8')

        if json_format:
            # Parse the received JSON data
            try:
                info = pd.DataFrame(json.loads(info))
            except json.JSONDecodeError as e:
                print(f"Error decoding JSON: {e}")
        elif conv_float:
            info = float(info)
            
        return info
    
    def _send_client_data(self,data):
        if not isinstance(data,bytes):
            data = data.encode()
        self.client.sendall(data)
        
    def generate_samples(self,allocation):
        sim_info = allocation
        self._send_client_data(sim_info.encode())
        return self._receive_client_data(json_format = True)
    
    def terminate_client(self):
        self.process.terminate()