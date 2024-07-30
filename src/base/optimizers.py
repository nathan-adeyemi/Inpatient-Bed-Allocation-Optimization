import os
import json
import re

from abc import abstractmethod
from pathlib import Path
from .base_class import BaseClass
from utils import  generate_random_identifier, convert_to_serializable

def wrapper(args):
    instance, inp = args
    return instance._evaluate(inp)

class popMultiObjOptim(BaseClass):
    
    # Base class for population based multi-objective simulation optimizers
    
    def __init__(self, config: dict = None, create_checkpoint = False):
        if config is not None:
            self.config = config
            self.term_crit = self.config.get("termination_criteria")
            self.sols_per_iter = self.config.get("n_sols")
            self.obj_fns = self.config.get("obj_fns")
            self.current_iteration = 0
            self.job_id = generate_random_identifier(10)
            self.config['job_dir'] = Path(os.path.join(self.config.get("job_dir"), self.job_id))
            if self.config.get("checkpoint-interval") and create_checkpoint:
                self.checkpoints_dir = os.path.join(self.config.get("job_dir"),'checkpoints',self.job_id)
                self.checkpoint_interval = self.config.get("checkpoint-interval")
                if not os.path.exists(self.checkpoints_dir):
                    os.makedirs(self.checkpoints_dir)
                    
            with open(os.path.join(self.config.get('job_dir'),"optim-config.json"),'w') as f:
                json.dump(obj = self.config,
                        fp = f,
                        default=convert_to_serializable,
                        indent = 4)
    
    def check_termination_condition(self):
        if self.current_iteration == 0:
            return False
        else:
            term_states = []
            for key in list(self.term_crit.keys()):
                criteria = self.term_crit.get(key)
                if "." in criteria.get('name'):
                    term_states.append(eval(f"self.{criteria.get('name')} {criteria.get('condition')} {criteria.get('value')}")) 
                else:
                    term_states.append(eval(f"getattr(self,'{criteria.get('name')}')  {criteria.get('condition')}  {criteria.get('value')}")) 
                    
            return any(term_states) 
        
    def optimize(self,reset = True):
        if reset:
            self.reset()
            print(f"~~~~~~~~~~Beginning Optimization Job {self.job_id} ~~~~~~~~~~~~~~~")
            
        while not self.check_termination_condition():
            self.execute_iteration()
            if self.config.get('checkpoint-interval'):
                self.create_checkpoint()
        self.terminate_experiment()

    def _to_dict(self):
        self_dict = {
            "job_id": self.job_id,
            "current_iteration": self.current_iteration,
            "obj_fns": self.obj_fns,
            "term_crit":self.term_crit,
            "sols_per_iter": self.sols_per_iter,
        }
        
        config_dict = {
            "config": self.config,   
        }
        
        if self.config.get('checkpoint-interval'):
            config_dict.update({
                'checkpoints_dir': self.checkpoints_dir,
                'checkpoint_interval': self.checkpoint_interval
            })
        return self_dict, config_dict
    
    def create_checkpoint(self):
        self.update_checkpoint_name()
        with open(f"{os.path.join(self.checkpoints_dir,self.checkpoint_name)}.json",'w') as f:
            json.dump(obj = self._to_dict(),
                      fp = f,
                      default=convert_to_serializable,
                      indent = 4)
            
        if hasattr(self,'history'):
            with open(f"{os.path.join(self.checkpoints_dir,'history')}.json", "w") as f:
                json.dump(obj=self.history,
                          fp = f,
                          default=convert_to_serializable,
                          indent=4)
    
    def find_last_checkpoint(self, exp_dir: str|os.PathLike = None):
        if exp_dir is None:
            exp_dir = self.checkpoints_dir
        checkpoint_files = [fname for fname in os.listdir(exp_dir) if fname.startswith('checkpoint-')]
        if not checkpoint_files:
            return "checkpoint-0"
        max_checkpoint = max([int(re.sub(".json","",fname.split('-')[-1])) for fname in checkpoint_files])
        return f"checkpoint-{max_checkpoint}"
    
    def update_checkpoint_name(self, exp_dir: str|os.PathLike = None):
        last_checkpoint = self.find_last_checkpoint(exp_dir)
        last_checkpoint_number = int(last_checkpoint.split('-')[-1])
        next_checkpoint_number = last_checkpoint_number + 1
        self.checkpoint_name = f"checkpoint-{next_checkpoint_number}"
        
    @classmethod
    def _from_checkpoint(cls,
                         exp_dir: str):
        instance = object.__new__(cls)
        with open(os.path.join(exp_dir,f"{instance.find_last_checkpoint(exp_dir=exp_dir)}.json"),"r") as f:
            data = json.load(f)
        instance.__dict__.update(data)
        return instance, data
                    
    @abstractmethod 
    def reset(self):
        pass
    
    @abstractmethod
    def terminate_experiment(self):
       pass
         
    @abstractmethod
    def execute_iteration(self):
        pass