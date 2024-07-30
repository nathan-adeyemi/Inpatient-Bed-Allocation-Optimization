import zipfile
import subprocess
import re
import os
import string
import secrets
import yaml 
import numpy as np
import pandas as pd
from itertools import combinations
from copy import deepcopy

# Function to execute the shell command
def execute_command(command):
    try:
        result = subprocess.run(command, check=True, capture_output=True, text=True)
        return result.stdout
    except subprocess.CalledProcessError as e:
        return f'Error executing the command: {e}\nOutput:\n{e.stdout.decode("utf-8")}'


def get_cpu_count():
    return len(os.sched_getaffinity(0))


def generate_random_identifier(length=8):
    characters = string.ascii_letters + string.digits
    identifier = "".join(secrets.choice(characters) for _ in range(length))
    return identifier

    
def read_yaml(filename):
    with open(filename, "r") as file:
        data = yaml.safe_load(file)
    return data

def flatten_list(nested_list):
    flattened = []
    for item in nested_list:
        if isinstance(item, list):
            flattened.extend(flatten_list(item))
        else:
            flattened.append(item)
    return flattened

def zip_dir(directory, zip_filename):
    # Get the absolute path of the directory
    directory = os.path.abspath(directory)
    
    # Initialize the zipfile object
    with zipfile.ZipFile(zip_filename, 'w', zipfile.ZIP_DEFLATED) as zipf:
        # Walk through all the files and subdirectories in the directory
        for root, _, files in os.walk(directory):
            for file in files:
                # Calculate the relative path of the file to the directory
                relative_path = os.path.relpath(os.path.join(root, file), directory)
                # Write the file to the zipfile
                zipf.write(os.path.join(root, file), arcname=relative_path)
    
def convert_to_serializable(obj):
    if isinstance(obj, np.integer):
        return int(obj)
    elif isinstance(obj, np.floating):
        return float(obj)
    elif isinstance(obj, (np.bool_, bool)):
        return bool(obj)
    elif isinstance(obj, np.str_):
        return str(obj)
    elif hasattr(obj,"tolist"):
        return obj.tolist()
    elif isinstance(obj,pd.DataFrame):
        return obj.reset_index().drop(columns = 'index').to_dict(orient = 'records')
    else:
        return 
    
def key_from_tuple(tuple_of_dicts):
    keys = set()  # Using a set to collect unique keys
    for d in tuple_of_dicts:
        keys.update(d.keys())
    return list(keys)  
    
def expand_configs(exp_cfg: dict, 
                  exp_name: str):
    
    if "ed_ip_sim" in exp_name:
        obj_fn_combos = [{'mh_wait_quantile': {'direction': "-"}},
                            {'mh_wait_sigma': {'direction': "-"}},
                            {'mh_distance_range': {'direction': "-"}},
                            {'mh_total_throughput': {'direction': "+"}}]
    else:
        obj_fn_combos = [{'TB_obj_1': {'direction': '+'}},
                        {'TB_obj_2': { 'direction': '-'}},
                        {'TB_obj_3':{ 'direction': '-'}}]
    obj_fn_combos = flatten_list([list(combinations(obj_fn_combos,i)) for i in range(2,len(obj_fn_combos)+1)])
    cfg_list = []
    job_dir = exp_cfg.get("job_dir")
    for objs in obj_fn_combos:
        tmp_nme = "-".join(key_from_tuple(objs))
        tmp = deepcopy(exp_cfg)
        tmp_dir =  os.path.join(job_dir,tmp_nme)
        os.makedirs(tmp_dir, exist_ok=True)
        tmp.update(
            {'obj_fns': {k:v for o in objs for k, v in o.items()},
            'job_dir':tmp_dir})
        cfg_list.append(tmp)
    return cfg_list
    
def update_config(exp_cfg: dict, 
                  exp_name: str):
    sim_info_path =  "src/Simulations/sim_info.json"
    units_dict = read_yaml(sim_info_path).get(re.split(r'/', exp_name, 1)[0]).get('units_dict')
    
    exp_cfg.update({'var_info': {'num_variables': np.sum([int(i.get('nVar')) for _, i in units_dict.items()]),
                                "constraint_info": units_dict},
                    'fn_cfg_path': sim_info_path,
                    'fn_cfg_name': exp_name})
    return exp_cfg
    

def dict_to_filename(config):
        # Create key-value pairs and replace periods with underscores
        items = [f"{key}-{str(value).replace('.', '_')}" for key, value in config.items()]
        # Join the items with an underscore
        filename = '_'.join(items)
        # Sanitize the filename (this step ensures that the filename is valid)
        sanitized_filename = re.sub(r'[^a-zA-Z0-9_-]', '_', filename)
        return sanitized_filename

def dict_to_path(config):
    # Extract the first two keys for the folder structure
    folder_keys = [key for key in list(config.keys())]
    # Create the folder path
    folder_path = []
    for key in folder_keys:
        value = config[key]
        folder_path.append(f"{key}-{str(value).replace('.', '_')}")
    
    # Join folder path components with OS-specific separator
    folder_path = os.path.join(*folder_path)

    # Sanitize the full path (optional, can be skipped if not needed)
    sanitized_path = re.sub(r'[^a-zA-Z0-9_/.-]', '_', folder_path)
    
    return sanitized_path
    