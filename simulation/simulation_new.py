from sympy import *
from sympy import diff
from sympy.utilities.lambdify import lambdify
import numpy as np
import scipy.stats as stats
from scipy.stats import truncnorm
import scipy.io

np.random.seed(2017)
# Two-sided-Market Oligopoly Model with 2 Symmetric Firms.

# Assign symbolic expressions:
# Prices
p1, r1, p2, r2 = symbols('p1 r1 p2 r2')

# Quantities
s1, s2, q1, q2 = symbols('s1 s2 q1 q2')

# INE Parameter
d, g = symbols('d g')

# Product Homogeneity Parameter
theta, mu = symbols('theta mu')

# Cost Variables
c1, c2, f1, f2 = symbols('c1 c2 f1 f2')

# Demand Functions: Each Firms faces two inverse Demand Functions
# ----------------------------Firm 1--------------------------------------
p1 = 1-q1-theta*q2+d*s1       # Market 1
r1 = 1-s1-mu*s2+g*q1    # Market 2
# ----------------------------Firm 2--------------------------------------
p2 = 1-q2-theta*q1+d*s2       # Market 1
r2 = 1-s2-mu*s1+g*q2    # Market 2

# Cost Functions: Both firms face a common and an individual cost-shock on both Markets.

# Profit Functions
# ------------------------------Firm1-------------------------------------
pi1 = (p1-c1)*q1+(r1-f1)*s1
# ------------------------------Firm2-------------------------------------
pi2 = (p2-c2)*q2+(r2-f2)*s2

# Take Derivatives
eqn1 = diff(pi1, q1)
eqn2 = diff(pi1, s1)
eqn3 = diff(pi2, q2)
eqn4 = diff(pi2, s2)

equations = [eqn1, eqn2, eqn3, eqn4]

# solve for q1,s1,q2,s2
sol = solve(equations, (q1, q2, s1, s2))

# Convert symbolic expression to function handle for q1,q2,s1,s2
q1 = lambdify((c1, c2, f1, f2, d, g, theta, mu), sol[q1])
q2 = lambdify((c1, c2, f1, f2, d, g, theta, mu), sol[q2])
s1 = lambdify((c1, c2, f1, f2, d, g, theta, mu), sol[s1])
s2 = lambdify((c1, c2, f1, f2, d, g, theta, mu), sol[s2])


def my_range(start, end, step):
    while start <= end:
        yield start
        start += step

Q1 = np.zeros((11,1000,11,11))
Q2 = np.zeros((11,1000,11,11))
#S1 = np.zeros((11,100,11,11))
#S2 = np.zeros((11,100,11,11))


# ------------- Asymmetric Shock --------------
# m2 (defined in loop) : tats.truncnorm((lower - m4) / sigma, (upper - m4) / sigma,
# loc=m4, scale=sigma).rvs()
lower, upper = 0, 0.1
m2_1 = np.random.uniform(0, 0.1)  # mean
m2_2 = np.random.uniform(0, 0.1)  # mean
m2_3 = np.random.uniform(0, 0.1)  # mean
m2_4 = np.random.uniform(0, 0.1)  # mean

theta_counter = 0
for theta in my_range(0, 1, .1):

    d_counter = 0
    for d in my_range(0, 0.5, .05):

        g_counter = 0
        for g in my_range(0, 0.5, .05):

            mu = theta

            if theta == 0:
                sigma1 = 1
                sigma2 = 1

            else:
                sigma1 = (theta * 10) ** (-2)
                sigma2 = (mu * 10) ** (-2)

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
                    c1_l = [np.random.uniform(0,0.1)]
                    c2_l = [np.random.uniform(0,0.1)]

                    # Market 2
                    f1_l = [np.random.uniform(0,0.1)]
                    f2_l = [np.random.uniform(0,0.1)]

                # Remaining periods: MC follow a random walk.
                else:
                    # Market 1
                    # min_c = min(c1_l[-1], c2_l[-1])
                    # l1 = np.random.uniform(-m3_1 * min_c, m3_1 * min_c)  # shock

                    c1_l.append(c1_l[-1] * 0.3 + m1_1 + l1)
                    c2_l.append(c2_l[-1] * 0.3 + m1_2 + l1)

                    # Market 2
                    # min_k = min(f1_l[-1], f2_l[-1])
                    # l2 = np.random.uniform(-m3_2 * min_c, m3_2 * min_c)  # shock

                    f1_l.append(f1_l[-1] * 0.3 + m1_3 + l2)
                    f2_l.append(f2_l[-1] * 0.3 + m1_4 + l2)

            # convert list to numpy array
            c1 = np.array(c1_l)
            c2 = np.array(c2_l)
            f1 = np.array(f1_l)
            f2 = np.array(f2_l)

            with np.errstate(divide='ignore'):  # Ignore Error divided by Zero
                Q1[g_counter, :, d_counter, theta_counter] = q1(c1, c2, f1, f2, d, g, theta, mu)
                Q2[g_counter, :, d_counter, theta_counter] = q2(c1, c2, f1, f2, d, g, theta, mu)
                #S1[g_counter, :, d_counter, mu_counter] = s1(c1, c2, f1, f2, d, g, theta, mu)
                #S2[g_counter, :, d_counter, mu_counter] = s2(c1, c2, f1, f2, d, g, theta, mu)

            print(theta_counter)

            g_counter = g_counter + 1
        d_counter = d_counter + 1
    theta_counter = theta_counter + 1

# Check correlation between and within Market 1 & 2
# pre-allocation to store the coefficients
QQ = np.zeros((11, 11, 11))
SS = np.zeros((11, 11, 11))
#QS1 = np.zeros((21, 21, 10))
#QS2 = np.zeros((21, 21, 10))


for mu in range(11):

    for d in range(11):

        for g in range(11):

            # Market 1
            tempQQ = np.corrcoef(Q1[g, :, d, mu], Q2[g, :, d, mu])
            QQ[g, d, mu] = tempQQ[0, 1]

            # Market 2
            #tempSS = np.corrcoef(S1[g, :, d, mu], S2[g, :, d, mu])
            #SS[g, d, mu] = tempSS[0, 1]

                # Between Market 1
            #tempQS1 = np.corrcoef(Q1[g, :, d, mu], S1[g, :, d, mu])
            #QS1[g, d, mu] = tempQS1[0, 1]

                # Between Market 2
            #tempQS2 = np.corrcoef(Q2[g, :, d, mu], S2[g, :, d, mu])
            #QS2[g, d, mu] = tempQS2[0, 1]


# Get the Diagonals to analyse how the sum of d+g depends on mu, theta
QQdiag = np.zeros((11,11))
QS1diag = np.zeros((11,11))


for i in range(11):

    tempQQ_diag= QQ[:,:,i].diagonal()
    QQdiag[:,i]=tempQQ_diag

    #tempQS1_diag=QS1[:,:,i].diagonal()
    #QS1diag[:,i]=tempQS1_diag


# Save to matlab file
scipy.io.savemat('qq.mat', mdict={'qq': QQ})
#scipy.io.savemat('qs1.mat', mdict={'qs1': QS1})

scipy.io.savemat('qqdiag.mat', mdict={'qqdiag': QQdiag})
#scipy.io.savemat('qsdiag.mat', mdict={'qsdiag': QS1diag})
