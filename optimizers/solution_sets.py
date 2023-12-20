class solution_set():
    def __init__(self):
        self.set = []
        self.ranks = None
        
    def get_attribute(self, attr: str):
        attr_list = [getattr(i,attr) for i in self.set]
        if isinstance(attr_list[0], pd.DataFrame) or isinstance(attr_list,np.ndarray):
            attr_list = np.concat(attr_list)
    
        
class pareto_set(solution_set):
    def __init__(self):
        super.__init__()
        
    def reorder(self):
        pass
    
    def update(self):
        
        # Input code for updating the solution set 
        self.reorder()
        

        
class solution():
    def __init__():
        self.replications = 0
        
    
    def