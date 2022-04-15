from sympy import *
import numpy as np
import scipy.stats as stats
from scipy.stats import truncnorm
from scipy.stats import norm
import numpy.random as rnd
import math
import matplotlib.pyplot as plt


# Set random seed to obtain reproducible results
rnd.seed(123)

for i in range(1000):
    v = np.random.uniform(50, 150)
    mu = np.random.uniform(0.000001, 100)

    ### a1 ###
    a1 = np.random.uniform(0.9, 0.05)

    ### a3 ####
    a3_1 = np.random.uniform(0, 1)  # Firm1
    a3_2 = np.random.uniform(0, 1)  # Firm2

    ### a2 ###

    # shape parameters
    sigma = mu ** (-2)
    m_1 = (a3_1 + 1) / 2  # Firm1
    m_2 = (a3_2 + 1) / 2  # Firm2

    # Bounds
    upper = 1
    lower_1 = a3_1  # Firm 1
    lower_2 = a3_2  # Firm 2

    # instantiate an object a2 using the above four parameters,
    a2_1 = stats.truncnorm.rvs(lower_1, upper, loc=m_1, scale=sigma)
    a2_2 = stats.truncnorm.rvs(lower_2, upper, loc=m_2, scale=sigma)

    if i == 0:
        a4 = np.random.uniform(0, 1)
        s = np.random.uniform(-a4 * (a1 * v), a4 * (a1 * v))

        k1 = [a1 * v + a2_1 * s]
        k2 = [a1 * v + a2_2 * s]

    else:
        a4 = np.random.uniform(0, 1)
        min_c = min(k1[-1],k2[-1])
        s = np.random.uniform(-a4 * min_c, a4 * min_c)
        print(s)

        k1.append(k1[-1] + a2_1 * s)
        k2.append(k2[-1] + a2_2 * s)

plt.plot(k1, alpha = 0.4);
plt.plot(k2, alpha = 0.4)

