import os 
os.environ['R_HOME'] = '/usr/local/bin/R'
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
import rpy2.robjects.packages as rpackages

# import R's utility package
utils = rpackages.importr('utils')
utils.chooseCRANmirror(ind=1)
if not rpackages.isinstalled('fpc'):
    utils.install_packages('fpc')

fpc = importr('fpc')
ro.r('''
    bhattacharyya_from_DFs <- function(df1,df2) {
        fps::bhattacharyya.dist(
        mu1 = apply(df1,2,mean),
        mu2 = apply(df2,2,mean),
        Sigma1 = cov(df1),
        Sigma2 = cov(df2)
        )
    }
        ''')