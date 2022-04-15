import scipy.stats as stats
import matplotlib.pyplot as plt
import numpy.random as rnd
import numpy as np

# Set random seed to obtain reproducible results
rnd.seed(123)

# Plot of censored normal distribution (a2)
# a2
# a2
a3_1=(np.random.randint(0,10))/100 # Firm1
a3_2=(np.random.randint(0,10))/100 # Firm2

lower_1 = (a3_1 + 1) / 2
lower_2 = (a3_2 + 1) / 2

upper = 0.000001 ** (-2)
m_1, m_2, sigma = a3_1,a3_2, 1

#instantiate an object a2 using the above four parameters,
X_1 = stats.truncnorm(lower_1, upper, loc=m_1, scale=sigma)
X_2 = stats.truncnorm(lower_2, upper, loc=m_2, scale=sigma)
#generate 1000 sample data
a2_1 = X_1.rvs(1000)
a2_2 = X_2.rvs(1000)

#compute the PDF of the sample data
#pdf_probs = stats.truncnorm.pdf(samples, lower, upper, mu, sigma)

#compute the CDF of the sample data
#cdf_probs = stats.truncnorm.cdf(samples, lower, upper, mu, sigma)

#make a histogram for the samples
plt.hist(a2_1, bins= 50,normed=True,alpha=0.3,label='histogram');
plt.hist(a2_2, bins= 50,normed=True,alpha=0.3,label='histogram')

#plot the PDF curves
#plt.plot(a2_1[a2_1.argsort()],pdf_probs[a2_1.argsort()],linewidth=2.3,label='PDF curve')

#plot CDF curve
#plt.plot(samples[samples.argsort()],cdf_probs[samples.argsort()],linewidth=2.3,label='CDF curve')


#legend
plt.legend(loc='best')