import socket
import json
import pandas as pd
import numpy as np
import subprocess
import os
import time


def r_client(input: dict, trigger_path: str) -> dict:
    
    def clean_input(item):
        if isinstance(item, dict):
            # Recursively clean nested dictionaries
            item = {k: clean_input(v) for k, v in item.items()}
        elif isinstance(item, pd.DataFrame):
            # Convert DataFrame to a dictionary of lists
            item = item.to_dict(orient='records')
        elif isinstance(item, np.ndarray):
            # Convert numpy array to a list
            item = item.tolist()
        elif isinstance(item, (list, tuple, set)):
            # Recursively clean list elements
            item = [clean_input(i) for i in item]
        elif  isinstance(item, os.PathLike):
            item = item.__fspath__()
        elif not (isinstance(item, (str, float, int, bool)) or item is None):
            # For any other type, attempt to convert to string
            item = str(item)
            
        return item
    
    server, port = _find_and_connect()
    # Begin the R simulation subprocess
    
    # Clean up the input information so its readable in the shell
    add_sub = {"port": str(port)}
    for k,v in input.items():
        v = clean_input(v)
        v = json.dumps(v) if not isinstance(v, str) else v 
        add_sub.update({k:v})
    
    subprocess_env = os.environ.copy()
    subprocess_env.update(add_sub)
    try:
        subprocess.Popen(["bash",trigger_path], env=subprocess_env)
    except Exception as e:
        print(f"Error starting the shell script: {e}")

    # Accept the client connection
    client, _ = server.accept()
    results = _receive_client_data(client = client, var_class = 'dataframe')
    
    return results

def _receive_client_data(client, var_class: str = 'int'):
    type_mapping = {# Add more types if needed
                            "int": int,
                            "float": float,
                            "str": str,
                            "bool": bool,
                            "dataframe": pd.DataFrame,
                            "array": np.ndarray
                            }
    info = client.recv(1024).decode("utf-8")
    try:
        info = json.loads(info)    
    except json.JSONDecodeError as e:
        print(f"Error decoding JSON: {e}")
    info = type_mapping[var_class](info)

    return info


def _send_client_data(client, data):
    if not isinstance(data, bytes):
        data = data.encode()
    client.sendall(data)


def _find_available_port(print_port=False):
    # Function to find an available port

    # Create a socket
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Bind to a random port
    s.bind(("localhost", 0))

    # Get the actual port number
    _, port = s.getsockname()

    # Close the socket
    s.close()

    if print_port:
        print(port)

    return port


def _find_and_connect(server_wait_tol: int = 1) -> socket.socket:
    cont = True
    while cont:
        try:
            port = _find_available_port()

            # Create a socket
            server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

            # Bind the socket to a specific address and port
            server_address = ("localhost", port)
            server.bind(server_address)
            cont = False
        except OSError as e:
            if e.errno == 98:
                print(
                    f"Port {port} is already in use. Retrying in {server_wait_tol} seconds..."
                )
                time.sleep(server_wait_tol)
            else:
                cont = False  # Exit the loop and raise the exception
                print('Unknown Error')
                raise 

    # Listen for incoming connections
    server.listen(1)

    return server, port
