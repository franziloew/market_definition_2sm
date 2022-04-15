from sympy import *
from sympy import diff
from sympy.utilities.lambdify import lambdify
import numpy as np
import scipy.io
import random

# Two-sided-Market Oligopoly Model with 2 Symmetric Firms.

# Assign symbolic expressions:
# Prices
p, r = symbols('p r')

# Quantities
s, q = symbols('s q')

# INE Parameter
d, g = symbols('d g')

#  other Parameter
t = symbols('t')


# Demand Functions
p = 1-q+(t+d)*s       #Market 1
r = 1-s            #Market 2


# Cost Functions
# K(q,s) = (1/2)*d^2*s,    with F=0

# Profit Function
pi = (p)*q+(r)*s-((d**2)*s)/2

# Take Derivatives
eqn1 = diff(pi, q)
eqn2 = diff(pi, s)
eqn3 = diff(pi, d)

equations = [eqn1, eqn2, eqn3]

# solve for q1,s1,q2,s2
sol = solve(equations, (q, s, d))

# Convert symbolic expression to function handle for q1,q2,s1,s2
q1 = lambdify((c1, c2, k1, k2, d, g, theta, mu), sol[q1])
q2 = lambdify((c1, c2, k1, k2, d, g, theta, mu), sol[q2])
s1 = lambdify((c1, c2, k1, k2, d, g, theta, mu), sol[s1])
s2 = lambdify((c1, c2, k1, k2, d, g, theta, mu), sol[s2])


def my_range(start, end, step):
    while start <= end:
        yield start
        start += step

Q1 = np.zeros((11,1000,11,11))
Q2 = np.zeros((11,1000,11,11))
S1 = np.zeros((11,1000,11,11))
S2 = np.zeros((11,1000,11,11))

# Set random seed to obtain reproducible results
random.seed(123)

mu_counter = 0
for mu in my_range(0, 1, .1):

    d_counter = 0
    for d in my_range(0, 1, .1):

        g_counter = 0
        for g in my_range(0, 1, .1):

            theta = mu
            # Modelling Marginal Costs
            # ----------------Market1 -------------------------
            # Symmetric Shock on Market 1
            shock_c1 = (np.random.randint(1, 10, size=(1,1000)))/1000
            # creates a 1 x100 Vector with uniformly distrib.# random numbers between 0 and 0.01

            # Asymmetric Shock on Market 1
            shock_c11 = (np.random.randint(1, 10, size=(1,1000)))/1000  # Firm1
            shock_c21 = (np.random.randint(1, 10, size=(1,1000)))/1000  # Firm2

            c1 = shock_c1 + shock_c11  # MC Firm1 / Market1
            #print('c1:{}'.format(c1))

            c2 = shock_c1 + shock_c21  # MC Firm2 / Market1
            #print('c2:{}'.format(c2))
            # ----------------Market2 -------------------------
            # Symmetric Shock on Market 2
            shock_c2 = (np.random.randint(1, 10, size=(1,1000)))/1000

            # Asymmetric Shock on Market 1
            shock_c12 = (np.random.randint(1, 10, size=(1,1000)))/1000  # Firm 1
            shock_c22 = (np.random.randint(1, 10, size=(1,1000)))/1000  # Firm 2

            k1 = shock_c2 + shock_c12  # MC Firm1 / Market2
            #print('k1:{}'.format(k1))

            k2 = shock_c2 + shock_c22  # MC Firm2 / Market2
            #print('k2:{}'.format(k2))

            with np.errstate(divide='ignore'):  # Ignore Error divided by Zero
                Q1[g_counter, :, d_counter, mu_counter] = q1(c1, c2, k1, k2, d, g, theta, mu)
                Q2[g_counter, :, d_counter, mu_counter] = q2(c1, c2, k1, k2, d, g, theta, mu)
                S1[g_counter, :, d_counter, mu_counter] = s1(c1, c2, k1, k2, d, g, theta, mu)
                S2[g_counter, :, d_counter, mu_counter] = s2(c1, c2, k1, k2, d, g, theta, mu)

            g_counter = g_counter + 1
        d_counter = d_counter + 1
    mu_counter = mu_counter + 1

# Check correlation between and within Market 1 & 2
# pre-allocation to store the coefficients
QQ = np.zeros((11, 11, 11))
SS = np.zeros((11, 11, 11))
QS1 = np.zeros((11, 11, 11))
QS2 = np.zeros((11, 11, 11))


for mu in range(11):
    print("mu:{}".format(mu))

    for d in range(11):
        print("d:{}".format(d))

        for g in range(11):
            print("g:{}".format(g))
               # Market 1
            tempQQ = np.corrcoef(Q1[g, :, d, mu], Q2[g, :, d, mu])
            QQ[g, d, mu] = tempQQ[0, 1]
                #print(QQ[g, d, mu])
                # Market 2
            tempSS = np.corrcoef(S1[g, :, d, mu], S2[g, :, d, mu])
            SS[g, d, mu] = tempSS[0, 1]

                # Between Market 1
            tempQS1 = np.corrcoef(Q1[g, :, d, mu], S1[g, :, d, mu])
            QS1[g, d, mu] = tempQS1[0, 1]

                # Between Market 2
            tempQS2 = np.corrcoef(Q2[g, :, d, mu], S2[g, :, d, mu])
            QS2[g, d, mu] = tempQS2[0, 1]


# Get the Diagonals to analyse how the sum of d+g depends on mu, theta
QQdiag = np.zeros((11,11))
QS1diag = np.zeros((11,11))


for i in range(11):

    tempQQ_diag= QQ[:,:,i].diagonal()
    QQdiag[:,i]=tempQQ_diag

    tempQS1_diag=QS1[:,:,i].diagonal()
    QS1diag[:,i]=tempQS1_diag


# Save to matlab file
scipy.io.savemat('qq.mat', mdict={'qq': QQ})
scipy.io.savemat('qs1.mat', mdict={'qs1': QS1})

scipy.io.savemat('qqdiag.mat', mdict={'qqdiag': QQdiag})
scipy.io.savemat('qsdiag.mat', mdict={'qsdiag': QS1diag})



