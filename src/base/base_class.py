import json
from abc import abstractmethod

class BaseClass():
    def __init__(self):
        pass
    
    def _to_json(self):
        json_string = json.dumps(self._to_dict(), indent=4)
        return json_string
    
    @classmethod
    def _from_dict(cls, data):
        """Create an instance from a dictionary."""
        instance = cls()
        instance.__dict__.update(data)
        return instance

    @classmethod
    def _from_json(cls, json_string):
        """Create an instance from a JSON string."""
        data = json.loads(json_string)
        return cls._from_dict(data)
    
    @abstractmethod
    def _to_dict(self):
        pass