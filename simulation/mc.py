from sympy import *
from sympy import diff
from sympy.utilities.lambdify import lambdify
import numpy as np
import scipy.io
import scipy.stats as stats
from scipy.stats import truncnorm
from scipy.stats import norm
import numpy.random as rnd
import math
import random
import matplotlib.pyplot as plt

mu=0.1
theta = mu

# ------------- Asymmetric Shock --------------
# m2 (defined in loop) : tats.truncnorm((lower - m4) / sigma, (upper - m4) / sigma,
# loc=m4, scale=sigma).rvs()
lower, upper = 0, 0.01
m2_1 = np.random.uniform(0, 0.01)  # mean
m2_2 = np.random.uniform(0, 0.01)  # mean
m2_3 = np.random.uniform(0, 0.01)  # mean
m2_4 = np.random.uniform(0, 0.01)  # mean

sigma1 = (mu * 100) ** (-2)
sigma2 = (theta * 100) ** (-2)

for i in range(1000):

    # ------------- Asymmetric Shock --------------
    # truncated normal distributed
    # Market 1
    m1_1 = stats.truncnorm(
        (lower - m2_1) / sigma1, (upper - m2_1) / sigma1, loc=m2_1, scale=sigma1).rvs()
    m1_2 = stats.truncnorm(
        (lower - m2_2) / sigma1, (upper - m2_2) / sigma1, loc=m2_2, scale=sigma1).rvs()

    # Market 2
    m1_3 = stats.truncnorm(
        (lower - m2_3) / sigma2, (upper - m2_3) / sigma2, loc=m2_3, scale=sigma2).rvs()
    m1_4 = stats.truncnorm(
        (lower - m2_4) / sigma2, (upper - m2_4) / sigma2, loc=m2_4, scale=sigma2).rvs()

    # shock
    l1 = np.random.uniform(0, 0.01)  # Market 1
    l2 = np.random.uniform(0, 0.01)  # Market 2

    # First period
    if i == 0:
        # Market 1
        c1_l = [np.random.uniform(0, 0.01)]
        c2_l = [np.random.uniform(0, 0.01)]

        # Market 2
        f1_l = [np.random.uniform(0, 0.01)]
        f2_l = [np.random.uniform(0, 0.01)]

    # Remaining periods: MC follow a random walk.
    else:
        # Market 1
        c1_l.append(c1_l[-1] * 0.3 + m1_1 + l1)
        c2_l.append(c2_l[-1] * 0.3 + m1_2 + l1)

        # Market 2
        f1_l.append(f1_l[-1] * 0.3 + m1_3 + l2)
        f2_l.append(f2_l[-1] * 0.3 + m1_4 + l2)


for i in range(1000):
    # Modelling Marginal Costs
    # ----------------Market1 -------------------------
    # Symmetric Shock on Market 1
    shock_c1 = np.random.uniform(0,0.01)

    # Asymmetric Shock on Market 1
    shock_c11 = np.random.uniform(0,0.01)  # Firm1
    shock_c21 = np.random.uniform(0,0.01)  # Firm2

    #  ----------------Market2 -------------------------
    # Symmetric Shock on Market 2
    shock_c2 = np.random.uniform(0,0.01)

    # Asymmetric Shock on Market 1
    shock_c12 = np.random.uniform(0,0.01) # Firm 1
    shock_c22 = np.random.uniform(0,0.01)  # Firm 2

# First period
    if i == 0:
        # Market 1
        c1_l1 = [shock_c1 + shock_c11]  # MC Firm1 / Market1
        c2_l1 = [shock_c1 + shock_c21]  # MC Firm2 / Market1

        # Market 2
        f1_l1 = [shock_c2 + shock_c12]  # MC Firm1 / Market1
        f2_l1 = [shock_c2 + shock_c22]  # MC Firm2 / Market1

    # Remaining periods: MC follow a random walk.
    else:
        # Market 1
        c1_l1.append(c1_l1[-1] * 0.3 + shock_c11 + shock_c1)
        c2_l1.append(c2_l1[-1] * 0.3 + shock_c21 + shock_c1)

        # Market 2
        f1_l1.append(f1_l1[-1] * 0.3 + shock_c12 + shock_c2)
        f2_l1.append(f2_l1[-1] * 0.3 + shock_c12 + shock_c2)


t = np.arange(0, 1000, 1)

plt.figure(1)
plt.subplot(211)
plt.plot(t, c1_l)
plt.subplot(212)
plt.plot(t, c1_l1)

shock_c1 = np.random.uniform(0.001,0.01, size=1000)
test1=(np.random.randint(1, 10, size=(1,1000))) / 1000
plt.hist(test1, alpha=0.3)
plt.hist(shock_c1, alpha = 0.3)
