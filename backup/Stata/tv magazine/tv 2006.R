# Market 7: TV Magazines
# Included Magazines: TV Movie, TV Spielfilm, TV Today
# Time: 2006 - 2008 (2 week interval)

tv<- read.csv("/Users/Franzi/Google Drive/HSU/Paper/2sm/Stata/tv magazine/tv 2weekly_06-08.csv")
attach(tv)


# Set data as panel data
pdata <- plm.data(mydata, index=c("id","t"))

# Descriptive statistics
summary(Y)
summary(X)

# Pooled OLS estimator
pooling <- plm(Y ~ X, data=pdata, model= "pooling")
summary(pooling)

# Between estimator
between <- plm(Y ~ X, data=pdata, model= "between")
summary(between)

# First differences estimator
firstdiff <- plm(Y ~ X, data=pdata, model= "fd")
summary(firstdiff)

# Fixed effects or within estimator
fixed <- plm(Y ~ X, data=pdata, model= "within")
summary(fixed)

# Random effects estimator
random <- plm(Y ~ X, data=pdata, model= "random")
summary(random)

# LM test for random effects versus OLS
plmtest(pooling)

# LM test for fixed effects versus OLS
pFtest(fixed, pooling)

# Hausman test for fixed versus random effects model
phtest(random, fixed)
