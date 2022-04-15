library("urca")
library("tseries")
library("seasonal")
library("lmtest")
library("vars")
library("forecast")
library("graphics")
library("Hmisc")
library("xtable")
library("tikzDevice")
library("stargazer")
library("plotrix")
library("foreign")


rm(list = ls())

# Included Magazines: Brigitte, Freundin, Für Sie
frauen <- read.csv("/Users/Franzi/Desktop/R/2sm/14frauen.csv", header = TRUE, stringsAsFactors = FALSE)   

frauen <- ts(frauen, start=c(2003,1), end = c(2016,26), frequency = 26)

# Assign Globals
# Assign Globals
# Sub1
y11 <- frauen[,"retailBrigitte"]
y21 <- frauen[,"totaladsBrigitte"]
x11 <- (frauen[,"totaladsBrigitte"]/frauen[,"contentBrigitte"])*100

# Sub2
y12 <- frauen[,"retailfreundin"]
y22 <- frauen[,"totaladsfreundin"]
x12 <- (frauen[,"totaladsfreundin"]/frauen[,"contentfreundin"])*100

# Sub3
y13 <- frauen[,"retailfuerSie"]
y23 <- frauen[,"totaladsfuerSie"]
x13 <- (frauen[,"totaladsfuerSie"]/frauen[,"contentfuerSie"])*100


# -------- Select Sub-Sample
#  --- 2013w33 - 2015w33 ---
# Sub1
y11.2 <- window(y11, start=c(2011,15), end = c(2014,15), frequency = 26)
x11.2 <- window(x11, start=c(2011,15), end = c(2014,15), frequency = 26)
y21.2 <- window(y21, start=c(2011,15), end = c(2014,15), frequency = 26)

# Sub2
y12.2 <- window(y12,  start=c(2011,15), end = c(2014,15), frequency = 26)
x12.2 <- window(x12,  start=c(2011,15), end = c(2014,15), frequency = 26)
y22.2 <- window(y22,  start=c(2011,15), end = c(2014,15), frequency = 26)

# Sub3
y13.2 <- window(y13,  start=c(2011,15), end = c(2014,15), frequency = 26)
x13.2 <- window(x13,  start=c(2011,15), end = c(2014,15), frequency = 26)
y23.2 <- window(y23,  start=c(2011,15), end = c(2014,15), frequency = 26)

# Fill NA
y11.2[which(is.na(y11.2))] <- mean(y11.2, na.rm = TRUE)
y12.2[which(is.na(y12.2))] <- mean(y12.2, na.rm = TRUE)
y13.2[which(is.na(y13.2))] <- mean(y13.2, na.rm = TRUE)
# 
y21.2[which(is.na(y21.2))] <- mean(y21.2, na.rm = TRUE)
y22.2[which(is.na(y22.2))] <- mean(y22.2, na.rm = TRUE)
y23.2[which(is.na(y23.2))] <- mean(y23.2, na.rm = TRUE)

######################
# Prewhitening / ARIMA
######################

# ------ Reader Markt

# Brigitte
Y11.2 <- auto.arima(y11.2, xreg = cbind(y12.2, y13.2), seasonal = FALSE)
Y11.2
^resid11 <- Y11.2$residuals

# Freundin
Y12.2 <- auto.arima(y12.2, xreg = cbind(y11.2, y13.2), seasonal = FALSE)
Y12.2
resid12 <- Y12.2$residuals

# Für Sie
Y13.2 <- auto.arima(y13.2, xreg = cbind(y11.2, y12.2), seasonal = FALSE)
Y13.2
resid13 <- Y13.2$residuals

# --------Ad Market

# Brigitte
Y21.2 <- auto.arima(y21.2, xreg = cbind(y22.2, y23.2))
Y21.2
resid21 <- Y21.2$residuals

# Freundin
Y22.2 <- auto.arima(y22.2, xreg = cbind(y21.2, y23.2))
Y22.2
# Y22.2 <- arima(y22.2, c(0,1,1), xreg = cbind(y21.2, y23.2))
resid22 <- Y22.2$residuals

# Für Sie
Y23.2 <- auto.arima(y23.2, xreg = cbind(y21.2, y22.2))
Y23.2
# Y23.2 <- arima(y23.2, c(0,0,0),xreg = cbind(y21.2, y22.2))
resid23 <- Y23.2$residuals

#####
# INE
#####

Y11 <- lm(resid22 ~ resid21 + resid23 + resid12)
summary(Y11)

rcorr(resid23,resid13, type = "pearson")

par(mfrow=c(1,3))
Ccf(resid11, resid21, 
                  lag.max = 10, ylim=c(-1,1), main = "Brigitte")
Ccf(resid12, resid22, 
                  lag.max = 10, ylim=c(-1,1), main = "Freundin")
Ccf(resid12, resid23, 
                  lag.max = 10, ylim=c(-1,1), main = "Für Sie")

###################
# Cross Correlation
###################
# 1 ----------------
par(mfrow=c(2,3))
xcorr112.2 <- Ccf(resid11, resid12,
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "Brigitte & Freundin R.M.")
xcorr113.2 <- Ccf(resid11, resid13, 
                  lag.max = 6, 
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "Brigitte & Für Sie R.M.")
xcorr123.2 <- Ccf(resid12, resid13, 
                  lag.max = 6, 
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "Freundin & Für Sie R.M.")
xcorr212.2 <- Ccf(resid21, resid22,
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "Brigitte & Freundin A.M.")
xcorr213.2 <- Ccf(resid21, resid23, 
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "Brigitte & Für Sie A.M.")
xcorr223.2 <- Ccf(resid22, resid23, 
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "Freundin & Für Sie A.M.")

# Latex Table
xcorr1.2 <- matrix(c(xcorr112.2$lag, xcorr112.2$acf,xcorr113.2$acf,xcorr123.2$acf), ncol = 4)
colnames(xcorr1.2) <- c("Lags", "Brigitte + Freundin", "Brigitte + FuerSie","Freundin + FuerSie")
stargazer(xcorr1.2, summary = FALSE)

# 2 ----------------
xcorr2.2 <- matrix(c(xcorr212.2$lag, xcorr212.2$acf,xcorr213.2$acf,xcorr223.2$acf), ncol = 4)
colnames(xcorr2.2) <- c("Lags", "Brigitte + Freundin", "Brigitte + FuerSie","Freundin + FuerSie")
stargazer(xcorr2.2, summary = FALSE)

#############
# Residual AC
#############

# --- Reader Market
# 1----------------
par(mfrow=c(3,2))

# Reader Market
Acf(resid11, 18, main = "Brigtte (R.M.)")
Acf(resid12, 18, main = "Freundin (R.M.)")
Acf(resid13, 18, main = "Für Sie (R.M.)")

# Ad Market
Acf(resid21, 18, main = "Brigtte (A.M.)")
Acf(resid22, 18, main = "Freundin (A.M.)")
Acf(resid23, 18, main = "Für Sie (A.M.)")


###################
# Granger Causality
###################
write.csv(cbind(resid11, resid12, resid13, resid21, resid22, resid23), file="granger_frauen.csv")

VARselect(cbind(resid11, resid12, resid13), lag.max = 12, type = "none")
VARselect(cbind(resid21, resid22, resid23), lag.max = 12, type = "none")


# Impulse response function
# Reader Market
plot(irf(var.1.1))
plot(irf(var.1.2))

# Ad Market
plot(irf(var.2.1))
plot(irf(var.2.2))


###################################
#  Cross Correlation (Frauen + TV)
##################################
# Reader Market  ----------------
par(mfrow=c(2,3))
xcorr1 <- Ccf(resid11, resid31.2,
              lag.max = 6,
              xlim = c(-6,6),
              ylim=c(-1,1),
              main = "Brigitte & TV Movie (R.M.)")
xcorr2 <- Ccf(resid13, resid32.2, 
              lag.max = 6,
              xlim = c(-6,6),
              ylim=c(-1,1),
              main = "Für Sie & TV Spielfilm (R.M.)")
xcorr3 <- Ccf(resid12, resid33.2, 
              lag.max = 6,
              xlim = c(-6,6),
              ylim=c(-1,1),
              main = "Freundin & TV Digital (R.M.)")
xcorr1.2 <- Ccf(resid21, resid41.2,
              lag.max = 6,
              xlim = c(-6,6),
              ylim=c(-1,1),
              main = "Brigitte & TV Movie (A.M.)")
xcorr2.2 <- Ccf(resid23, resid42.2, 
              lag.max = 6,
              xlim = c(-6,6),
              ylim=c(-1,1),
              main = "Für Sie & TV Spielfilm (A.M.)")
xcorr3.2 <- Ccf(resid22, resid43.2, 
              lag.max = 6,
              xlim = c(-6,6),
              ylim=c(-1,1),
              main = "Freundin & TV Digital (A.M.)")

# Testing Indirect network effect
Ccf(resid11, resid12, lag.max = 20)
Ccf(resid12, resid22, lag.max = 20)
Ccf(resid13, resid23, lag.max = 20)


