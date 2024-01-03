# import ray
import socket
import json
import pandas as pd

from utils.utils import find_available_port

# @ray.remote(num_cpus = 1)    
class r_sim_client():
    def __init__(self,initial_info, port = None):
        self.initial_info = initial_info
        if port is None:
            port = find_available_port(print_port = True)
        
        # Create a socket
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        # Bind the socket to a specific address and port
        server_address = ('localhost', port)
        self.server.bind(server_address)
        
        # Listen for incoming connections
        self.server.listen(1)
        
        # Accept the client connection
        self.client, _ = self.server.accept()
    
        # Set up server side port connection to transmit actions to the R simulation

        self.client.sendall(self.initial_info.encode())
        self.n_queues = self._receive_client_data(conv_float = True)
        self.n_queues = int(self.n_queues)
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