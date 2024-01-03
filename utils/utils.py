import numpy as np
import math
import pandas as pd
import rpy2
import rpy2.robjects as ro

from scipy.spatial.distance import mahalanobis
from scipy.linalg import cholesky
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri

def smart_round(input_vector):
    if not isinstance(input_vector,np.ndarray):
        input_vector = np.array(input_vector)
    rounded_vector = np.round(input_vector)
    rounding_error = int(np.sum(rounded_vector) - np.sum(input_vector))

    # Adjust rounding error by adding or subtracting 1 from the smallest absolute rounding error
    if rounding_error > 0:
        indices_to_adjust = np.argpartition(np.abs(input_vector - rounded_vector), -rounding_error)[:rounding_error]
        rounded_vector[indices_to_adjust] -= 1
    elif rounding_error < 0:
        indices_to_adjust = np.argpartition(np.abs(input_vector - rounded_vector), rounding_error)[:abs(rounding_error)]
        rounded_vector[indices_to_adjust] += 1

    return rounded_vector.astype(int)


def decode(sol: np.ndarray, capacities: dict = None):
    
    def sub_fun(x,y):
        return smart_round(x/np.sum(x) * y)
    
    p0 = 0
    allocation = []
    for _ ,sub_dict in capacities.items():
        p1 = p0 + sub_dict['num_pools']
        allocation.append(sub_fun(x = sol[p0:p1],y = sub_dict['total_capacity']))
        p0 = p1
        
    return np.array(allocation)

def bhattacharyya_distance(df1, df2):
    
    
    """
    Calculate Bhattacharyya distance for multivariate data between two DataFrames.

    Parameters:
    - df1: First DataFrame
    - df2: Second DataFrame

    Returns:
    - bhattacharyya_distance: Bhattacharyya distance value
    """

    # Calculate the covariance matrix for each group
    cov1 = np.cov(df1, rowvar=False)
    cov2 = np.cov(df2, rowvar=False)

    # Calculate the average covariance matrix using the arithmetic mean
    avg_cov = (cov1 + cov2) / 2.0

    # Invert the average covariance matrix
    avg_cov_inv = np.linalg.inv(avg_cov)

    # Calculate the Mahalanobis distances for each sample in both groups
    mahalanobis_dist1 = [mahalanobis(x, df1.mean().values, avg_cov_inv) for x in df1.values]
    mahalanobis_dist2 = [mahalanobis(x, df2.mean().values, avg_cov_inv) for x in df2.values]

    # Calculate the Bhattacharyya distance
    bhat_distance = np.mean(np.log(np.sqrt(np.linalg.det(avg_cov) / np.sqrt(np.linalg.det(cov1) * np.linalg.det(cov2)))) +
                            0.5 * (np.mean(mahalanobis_dist1) + np.mean(mahalanobis_dist2)))

    return bhat_distance

def bhattacharyya(df1,df2):
    dist_Fn = ro.globalenv['bhat_from_DFs']
    with (ro.default_converter + pandas2ri.converter).context():
      return dist_Fn(df1,df2)