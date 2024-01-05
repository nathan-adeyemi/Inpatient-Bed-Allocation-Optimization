import rpy2.robjects as ro
from rpy2.robjects import pandas2ri

def bhattacharyya(df1,df2):
    dist_Fn = ro.globalenv['bhat_from_DFs']
    with (ro.default_converter + pandas2ri.converter).context():
      return dist_Fn(df1,df2)