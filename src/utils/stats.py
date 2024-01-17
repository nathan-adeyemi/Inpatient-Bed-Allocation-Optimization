import numpy as np
import pandas as pd

from scipy import stats
from scipy.spatial.distance import mahalanobis

def bhattacharyya_dist(df1: pd.DataFrame, df2: pd.DataFrame):
    
    # Followd the CRAN fpc::bhattacharyya.dist() implementation 
    # See: https://cran.r-project.org/web/packages/fpc/fpc.pdf
    
    mu_1 = df1.to_numpy().mean(axis=0)
    mu_2 = df2.to_numpy().mean(axis=0)
    
    sigma_1 = np.cov(df1.to_numpy(),rowvar=False)
    sigma_2 = np.cov(df2.to_numpy(),rowvar=False)
    
    sig = (sigma_1 + sigma_2)/2
    
    _, det_ovr = np.linalg.slogdet(sig)
    _, det_1 = np.linalg.slogdet(sigma_1)
    _, det_2 = np.linalg.slogdet(sigma_2)
    
    dist1 = (mahalanobis(mu_1, mu_2, np.linalg.inv(sig))**2)/8
    dist2 = 0.5 * det_ovr - 0.25 * det_1 -0.25 * det_2

    return dist1 + dist2
