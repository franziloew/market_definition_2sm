from sympy import *
from sympy import diff
from sympy.utilities.lambdify import lambdify
import numpy as np
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
c1, c2, k1, k2 = symbols('c1 c2 k1 k2')

# Demand Functions: Each Firms faces two inverse Demand Functions
# ----------------------------Firm 1--------------------------------------
p1 = 1-q1-mu*q2+d*s1       #Market 1
r1 = 1-s1-theta*s2+g*q1          #Market 2
# ----------------------------Firm 2--------------------------------------
p2 = 1-q2-mu*q1+d*s2       #Market 1
r2 = 1-s2-theta*s1+g*q2          #Market 2

# Cost Functions: Both firms face a common and an individual cost-shock on both Markets.

# Profit Functions
# ------------------------------Firm1-------------------------------------
pi1 = (p1-c1)*q1+(r1-k1)*s1
# ------------------------------Firm2-------------------------------------
pi2 = (p2-c2)*q2+(r2-k2)*s2

# Take Derivatives
eqn1 = diff(pi1, q1)
eqn2 = diff(pi1, s1)
eqn3 = diff(pi2, q2)
eqn4 = diff(pi2, s2)

equations = [eqn1, eqn2, eqn3, eqn4]

# solve for q1,s1,q2,s2
sol = solve(equations, (q1, q2, s1, s2))

# Convert symbolic expression to function handle for q1,q2,s1,s2
q1 = lambdify((c1, c2, k1, k2, d, g, theta, mu), sol[q1])
q2 = lambdify((c1, c2, k1, k2, d, g, theta, mu), sol[q2])
s1 = lambdify((c1, c2, k1, k2, d, g, theta, mu), sol[s1])
s2 = lambdify((c1, c2, k1, k2, d, g, theta, mu), sol[s2])


def my_range(start, end, step):
    while start <= end:
        yield start
        start += step

Q1 = np.zeros((21,10000,21,11))
Q2 = np.zeros((21,10000,21,11))
S1 = np.zeros((21,10000,21,11))
S2 = np.zeros((21,10000,21,11))

mu_counter = 0
for mu in my_range(0, 1, .1):

    d_counter = 0
    for d in my_range(0, 1, .05):

        g_counter = 0
        for g in my_range(0, 1, .05):

            theta = mu
            # Modelling Marginal Costs
            # ----------------Market1 -------------------------
            # Symmetric Shock on Market 1
            shock_c1 = (np.random.randint(1, 10, size=(1, 1000))) / 1000
            # creates a 1 x100 Vector with uniformly distrib.# random numbers between 0 and 0.01

            # Asymmetric Shock on Market 1
            shock_c11 = (np.random.randint(1, 10, size=(1, 1000))) / 1000  # Firm1
            shock_c21 = (np.random.randint(1, 10, size=(1, 1000))) / 1000  # Firm2

            c1 = shock_c1 + shock_c11  # MC Firm1 / Market1
            # print('c1:{}'.format(c1))

            c2 = shock_c1 + shock_c21  # MC Firm2 / Market1
            # print('c2:{}'.format(c2))
            # ----------------Market2 -------------------------
            # Symmetric Shock on Market 2
            shock_c2 = (np.random.randint(1, 10, size=(1, 1000))) / 1000

            # Asymmetric Shock on Market 1
            shock_c12 = (np.random.randint(1, 10, size=(1, 1000))) / 1000  # Firm 1
            shock_c22 = (np.random.randint(1, 10, size=(1, 1000))) / 1000  # Firm 2

            k1 = shock_c2 + shock_c12  # MC Firm1 / Market2
            # print('k1:{}'.format(k1))

            k2 = shock_c2 + shock_c22  # MC Firm2 / Market2
            # print('k2:{}'.format(k2))

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
QQ = np.zeros((21, 21, 11))
SS = np.zeros((21, 21, 11))
QS1 = np.zeros((21, 21, 11))
QS2 = np.zeros((21, 21, 11))


for mu in range(11):
    #print("mu:{}".format(mu))

    for d in range(21):
        #print("d:{}".format(d))

        for g in range(21):
            #print("g:{}".format(g))
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
QQdiag = np.zeros((21,11))
QS1diag = np.zeros((21,11))


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
