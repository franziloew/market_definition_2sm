
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

rm(list = ls())
setwd("~/Google Drive/HSU/Paper/2sm/Paper 2sm/Data")

# Market 7: News Magazines 
# Included Magazines: Focus, DerSpiegel, Stern
# Time Interval: 2003w1 - 2016w33

fss <- read.csv("fss.csv", header = TRUE, stringsAsFactors = FALSE)

fss <- ts(fss, start=c(2003,1), end = c(2016,33), frequency = 52)

# Assign Globals
# Sub1
y11 <- fss[,"retailFOCUS"]
x11 <- fss[,"cpriceFOCUS"]
y21 <- fss[,"totaladsiteFOCUS"]
x21 <- (fss[,"totaladsiteFOCUS"]/fss[,"contentFOCUS"])*100
x31 <- fss[,"adpriceFOCUS"]
x41 <- fss[,"adpriceFOCUS"]*fss[,"totaladsiteFOCUS"]


# Sub2
y12 <- fss[,"retailDerSpiegel"]
x12 <- fss[,"cpriceDerSpiegel"]
y22 <- fss[,"totaladsiteDerSpiegel"]
x22 <- (fss[,"totaladsiteDerSpiegel"]/fss[,"contentDerSpiegel"])*100
x32 <- fss[,"adpriceDerSpiegel"]
x42 <- fss[,"adpriceDerSpiegel"]*fss[,"totaladsiteDerSpiegel"]

# Sub3
y13 <- fss[,"retailStern"]
x13 <- fss[,"cpriceStern"]
y23 <- fss[,"totaladsiteStern"]
x23 <- (fss[,"totaladsiteStern"]/fss[,"contentStern"])*100
x33 <- fss[,"adpriceStern"]
x43 <- fss[,"adpriceStern"]*fss[,"totaladsiteStern"]

# Time Trend
t<-seq(0, 709, length=length(y11))
t <- ts(t, start=c(2003,1), end = c(2016,33), frequency = 52)

# --------- Select Sub-Sample
# 1 --- 2004w33 - 2006w33 ---

# Sub1
y11.1 <- window(y11, start=c(2004,33), end = c(2006,33), frequency = 52)
x11.1 <- window(x11, start=c(2004,33), end = c(2006,33), frequency = 52)
y21.1 <- window(y21, start=c(2004,33), end = c(2006,33), frequency = 52)
x21.1 <- window(x21, start=c(2004,33), end = c(2006,33), frequency = 52)
x31.1 <- window(x31, start=c(2004,33), end = c(2006,33), frequency = 52)
x41.1 <- window(x41, start=c(2004,33), end = c(2006,33), frequency = 52)

# Sub2
y12.1 <- window(y12,  start=c(2004,33), end = c(2006,33), frequency = 52)
x12.1 <- window(x12,  start=c(2004,33), end = c(2006,33), frequency = 52)
y22.1 <- window(y22,  start=c(2004,33), end = c(2006,33), frequency = 52)
x22.1 <- window(x22, start=c(2004,33), end = c(2006,33), frequency = 52)
x32.1 <- window(x32, start=c(2004,33), end = c(2006,33), frequency = 52)
x42.1 <- window(x42, start=c(2004,33), end = c(2006,33), frequency = 52)

# Sub3
y13.1 <- window(y13,  start=c(2004,33), end = c(2006,33), frequency = 52)
x13.1 <- window(x13,  start=c(2004,33), end = c(2006,33), frequency = 52)
y23.1 <- window(y23,  start=c(2004,33), end = c(2006,33), frequency = 52)
x23.1 <- window(x23, start=c(2004,33), end = c(2006,33), frequency = 52)
x33.1 <- window(x33, start=c(2004,33), end = c(2006,33), frequency = 52)
x43.1 <- window(x43, start=c(2004,33), end = c(2006,33), frequency = 52)

# Time Trend
t1 <- window(t,  start=c(2004,33), end = c(2006,33), frequency = 52)

# 2 --- 2013w33 - 2015w33 ---
# Sub1
y11.2 <- window(y11, start=c(2013,33), end = c(2015,33), frequency = 52)
x11.2 <- window(x11, start=c(2013,33), end = c(2015,33), frequency = 52)
y21.2 <- window(y21, start=c(2013,33), end = c(2015,33), frequency = 52)
x21.2 <- window(x21, start=c(2013,33), end = c(2015,33), frequency = 52)
x31.2 <- window(x31, start=c(2013,33), end = c(2015,33), frequency = 52)
x41.2 <- window(x41, start=c(2013,33), end = c(2015,33), frequency = 52)

# Sub2
y12.2 <- window(y12,  start=c(2013,33), end = c(2015,33), frequency = 52)
x12.2 <- window(x12,  start=c(2013,33), end = c(2015,33), frequency = 52)
y22.2 <- window(y22,  start=c(2013,33), end = c(2015,33), frequency = 52)
x22.2 <- window(x22, start=c(2013,33), end = c(2015,33), frequency = 52)
x32.2 <- window(x32, start=c(2013,33), end = c(2015,33), frequency = 52)
x42.2 <- window(x42, start=c(2013,33), end = c(2015,33), frequency = 52)

# Sub3
y13.2 <- window(y13,  start=c(2013,33), end = c(2015,33), frequency = 52)
x13.2 <- window(x13,  start=c(2013,33), end = c(2015,33), frequency = 52)
y23.2 <- window(y23,  start=c(2013,33), end = c(2015,33), frequency = 52)
x23.2 <- window(x23, start=c(2013,33), end = c(2015,33), frequency = 52)
x33.2 <- window(x33, start=c(2013,33), end = c(2015,33), frequency = 52)
x43.2 <- window(x43, start=c(2013,33), end = c(2015,33), frequency = 52)

# Time Trend
t2 <- window(t,  start=c(2013,33), end = c(2015,33), frequency = 52)

# Fill NA
y11.1[which(is.na(y11.1))] <- mean(y11.1, na.rm = TRUE)
y12.1[which(is.na(y12.1))] <- mean(y12.1, na.rm = TRUE)
y13.1[which(is.na(y13.1))] <- mean(y13.1, na.rm = TRUE)

y11.2[which(is.na(y11.2))] <- mean(y11.2, na.rm = TRUE)
y12.2[which(is.na(y12.2))] <- mean(y12.2, na.rm = TRUE)
y13.2[which(is.na(y13.2))] <- mean(y13.2, na.rm = TRUE)
# 
y21.1[which(is.na(y21.1))] <- mean(y21.1, na.rm = TRUE)
y22.1[which(is.na(y22.1))] <- mean(y22.1, na.rm = TRUE)
y23.1[which(is.na(y23.1))] <- mean(y23.1, na.rm = TRUE)

y21.2[which(is.na(y21.2))] <- mean(y21.2, na.rm = TRUE)
y22.2[which(is.na(y22.2))] <- mean(y22.2, na.rm = TRUE)
y23.2[which(is.na(y23.2))] <- mean(y23.2, na.rm = TRUE)


######################
# Summary and plotting
######################

# ----- Reader Market
cbd.y1.1 <- data.frame(y11.1, y12.1, y13.1, x11.1,  x12.1, x13.1)
colnames(cbd.y1.1) <- c("FOCUS", "Der Spiegel", "Stern", "FOCUS", "Der Spiegel", "Stern")

cbd.y1.2 <- data.frame(y11.2, y12.2, y13.2, x11.2,  x12.2, x13.2)
colnames(cbd.y1.2) <- c("FOCUS", "Der Spiegel", "Stern", "FOCUS", "Der Spiegel", "Stern")

stargazer(cbd.y1.1, digits = 0, title = "Summary Statistic: Reader Market")

stargazer(cbd.y1.2, digits = 0, title = "Summary Statistic: Reader Market")

# ----- Ad Market
cbd.y2.1 <- data.frame(y21.1, y22.1, y23.1, x31.1, x32.1, x33.1, x21.1, x22.1, x23.1)
colnames(cbd.y2.1) <- c("FOCUS", "Der Spiegel", "Stern", "FOCUS", "Der Spiegel", "Stern",
                        "FOCUS", "Der Spiegel", "Stern")

cbd.y2.2 <- data.frame(y21.2, y22.2, y23.2, x31.2, x32.2, x33.2, x21.2, x22.2, x23.2)
colnames(cbd.y2.2) <- c("FOCUS", "Der Spiegel", "Stern", "FOCUS", "Der Spiegel", "Stern",
                        "FOCUS", "Der Spiegel", "Stern")

stargazer(cbd.y2.1, digits = 1, title = "Summary Statistic: advertising market")

stargazer(cbd.y2.2, digits = 1)
# ---- Reader Market
# Retail ------------
#tikz("/Users/Franzi/Desktop/R/circ_fss1.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts((y11)/1000, type = "l", col = "red", xlab = "Weekly Data",
        ylab = "Total Retail in tsd",
        ylim = c(0, 550))

lines((y12)/1000, type = "l", col = "blue")

lines((y13)/1000, type = "l", col = "green")

legend("topleft", lty = c(1,1,1), lwd = c(2.5,2.5,2.5),
       c("FOCUS", "Der Spiegel", "Stern"), 
       col = c("red", "blue", "green"),
       bty = "n", horiz = FALSE)
#dev.off()

# ---- Reader Market
# Price ------------
par(mfrow=c(1,1))
plot.ts(x11, type = "l", col = "red", xlab = "Weekly Data",
        ylab = "Copy Price")

lines(x12, type = "l", col = "blue")

lines(x13, type = "l", col = "green")

legend("topleft", c("FOCUS", "Der Spiegel", "Stern"), lty = c(1,1,1), lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)
#dev.off()

# Copyprice -----------
#tikz("/Users/Franzi/Desktop/R/circ_fss1.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts((x31)/1000, type = "l", col = "red", xlab = "Weekly Data",
        ylab = "Copyprice",
        ylim = c(45,70))

lines((x32)/1000, type = "l", col = "blue")
lines((x33)/1000, type = "l", col = "green")

legend("topleft", c("FOCUS", "Der Spiegel", "Stern"), lty = c(1,1,1), lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)
#dev.off()
#-------- Ad Market
# 1 ---------------
#tikz("/Users/Franzi/Desktop/R/ads_fss1.tex",width=5,height=3.5)
plot.ts(x21, type = "l", col = "red", xlab = "Weekly Data", ylab = "Share of ad-pages",
        ylim = c(0,60))

lines(x22, type = "l", col = "blue")
lines(x23, type = "l", col = "green")

legend("topleft", lty = c(1,1,1), lwd = c(2.5,2.5,2.5),
       c("FOCUS", "Der Spiegel", "Stern"),
       col = c("red", "blue", "green"),
       bty = "n", horiz = FALSE)
#dev.off()
# Revenue ----------------
plot.ts(x41, ylab = "Revenue", type = "l", col = "red")

lines(x42, type = "l", col = "blue")

lines(x43, type = "l", col = "green")
legend("topleft", lty = c(1,1,1), lwd = c(2.5,2.5,2.5),
       c("FOCUS", "Der Spiegel", "Stern"),
       col = c("red", "blue", "green"), bty = "n", horiz = FALSE)
# Adprice -----------
par(mfrow=c(1,1))
plot.ts((x31)/1000, type = "l", col = "red", xlab = "Weekly Data",
        ylab = "Price / advertising page",
        ylim = c(45,70))

lines((x32)/1000, type = "l", col = "blue")

lines((x33)/1000, type = "l", col = "green")

legend("topleft", lty = c(1,1,1), lwd = c(2.5,2.5,2.5),
       c("FOCUS", "Der Spiegel", "Stern"),
       col = c("red", "blue", "green"),
       bty = "n", horiz = FALSE)

#######
# ACF #
#######

par(mfrow=c(3,2))         

Acf(y11.1, 20, main = "FOCUS (R.M.1)")
Acf(y11.2,20, main = "FOCUS (R.M.2)")

Acf(y12.1,20, main = "Der Spiegel (R.M.1)")
Acf(y12.2, 20, main = "Der Spiegel (R.M.2)")

Acf(y13.1, 20, main = "Stern (R.M.1)")
Acf(y13.2, 20, main = "Stern (R.M.2)")

Acf(y21.1, 20, main = "FOCUS (A.M.1)")
Acf(y21.2, 20, main = "FOCUS (A.M.2)")

Acf(y22.1, 20, main = "Der Spiegel (A.M.1)")
Acf(y22.2, 20, main = "Der Spiegel (A.M.2)")

Acf(y23.1, 20, main = "Stern (A.M.1)")
Acf(y23.2, 20, main = "Stern (A.M.2)")


###################################
# Phillips \& Perron Unit Root Test
###################################

# Null-Hypothesis: Series is non-stationary. If the test statistic is bigger than the critical value, we cannot reject the Null and Series is non-stationary. 

# ------- Reader Market
# 1 ------------------
y11.1.p <- (ur.pp(y11.1, type = "Z-tau", model = "constant"))
y12.1.p <- (ur.pp(y12.1, type = "Z-tau", model = "constant"))
y13.1.p <- (ur.pp(y13.1, type = "Z-tau", model = "constant"))

# 2 ------------------
y11.2.p <- (ur.pp(y11.2, type = "Z-tau", model = "constant"))
y12.2.p <- (ur.pp(y12.2, type = "Z-tau", model = "constant"))
y13.2.p <- (ur.pp(y13.2, type = "Z-tau", model = "constant"))


# ------- Ad Market
# 1 ------------------
y21.1.p <- (ur.pp(y21.1, type = "Z-tau", model = "constant"))
y22.1.p <- (ur.pp(y22.1, type = "Z-tau", model = "constant"))
y23.1.p <- (ur.pp(y23.1, type = "Z-tau", model = "constant"))

# 2 ------------------
y21.2.p <- (ur.pp(y21.2, type = "Z-tau", model = "trend"))
y22.2.p <- (ur.pp(y22.2, type = "Z-tau", model = "trend"))
y23.2.p <- (ur.pp(y23.2, type = "Z-tau", model = "trend"))

# ------ Latex Table 

# 2004-2006---------
c.y1p <- data.frame(y11.1.p@teststat, y12.1.p@teststat, y13.1.p@teststat)
c.y1p[nrow(c.y1p)+1,]<-c(y21.1.p@teststat, y22.1.p@teststat, y23.1.p@teststat)
c.y1p[nrow(c.y1p)+1,]<-c("1pct","5pct","10pct")
c.y1p[nrow(c.y1p)+1,]<-y11.1.p@cval
colnames(c.y1p) <- c("FOCUS", "Der Spiegel", "Stern")
rownames(c.y1p) <- c("Sales", "Ad pages","Sig. Level", "Critical Values")

stargazer(c.y1p, summary = FALSE, digits = 1, title = "Unit Root: 2004-2006")

# 2013-2015-------- 
c.y2p <- data.frame(y11.2.p@teststat, y12.2.p@teststat, y13.2.p@teststat)
c.y2p[nrow(c.y2p)+1,]<-c(y21.2.p@teststat, y22.2.p@teststat, y23.2.p@teststat)
c.y2p[nrow(c.y2p)+1,]<-c("1pct","5pct","10pct")
c.y2p[nrow(c.y2p)+1,]<-y11.2.p@cval
colnames(c.y2p) <- c("FOCUS", "Der Spiegel", "Stern")
rownames(c.y2p) <- c("Sales", "Ad pages","Sig. Level", "Critical Values")

stargazer(c.y2p, summary = FALSE, digits = 1, title = "Unit Root: 2013-2015")

######################
# Prewhitening / ARIMA
######################

# ------ Reader Markt
# 1 ----------------

# FOCUS
Y11.1 <- auto.arima(y11.1, xreg = cbind(y12.1, y13.1), max.d = 0, seasonal = FALSE)
Y11.1
Y11.1 <- Arima(y11.1, order = c(4,0,0), xreg = cbind(y12.1, y13.1))
resid11.1 <- Y11.1$residuals
Acf(resid11.1, 20)

# Der Spiegel
Y12.1 <- auto.arima(y12.1, xreg = cbind(y11.1, y13.1), max.d = 0, seasonal = FALSE)
Y12.1

Y12.1 <- Arima(y12.1, order = c(4,0,0), xreg = cbind(y11.1, y13.1))
Y12.1$aic
resid12.1 <- Y12.1$residuals
Acf(resid12.1)

# Stern
Y13.1 <- auto.arima(y13.1, xreg = cbind(y11.1, y12.1), max.d = 0, seasonal = FALSE)
Y13.1
Y13.1 <- Arima(y13.1, order = c(1,0,1), xreg = cbind(y11.1, y12.1))
resid13.1 <- Y13.1$residuals
Acf(resid13.1, 20)

# Latex Table
#stargazer(Y11.1, Y12.1, Y13.1, title = "Regression Results: Reader Market (2004-2006)", align = TRUE, dep.var.labels = c("FOCUS", "Der Spiegel", "Stern"), omit.stat = c("LL"), no.space = TRUE)

# Plotting residuals
#tikz("/Users/Franzi/Desktop/R/arima_circ_fss1.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts((resid11.1)/1000, type = "l", col = "red", ylim = c(-120,120), xlab = "Weekly Data",
        ylab = "Residuals")

lines((resid12.1)/1000, type = "l", col = "blue")

lines((resid13.1)/1000, type = "l", col = "green")

abline(a = 0, b = 0)
dev.off()

# 2 ------------
# FOCUS
Y11.2 <- auto.arima(y11.2, xreg = cbind(y12.2, y13.2), max.d = 0, seasonal = FALSE)
Y11.2
Y11.2 <- Arima(y11.2, order = c(1,0,0), xreg = cbind(y12.2, y13.2))
resid11.2 <- Y11.2$residuals
Acf(resid11.2)

# Der Spiegel
Y12.2 <- auto.arima(y12.2, xreg = cbind(y11.2, y13.2),  max.d = 0, seasonal = FALSE)
Y12.2
#Y12.2 <- Arima(y12.2, c(1,0,0), xreg = cbind(y11.2, y13.2))
resid12.2 <- Y12.2$residuals
Acf(resid12.2)

# Stern
Y13.2 <- auto.arima(y13.2, xreg = cbind(y11.2, y12.2), max.d = 0, seasonal = FALSE)
Y13.2
Y13.2 <- Arima(y13.2, c(1,0,0), xreg = cbind(y11.2, y12.2))
resid13.2 <- Y13.2$residuals
Acf(resid13.2,20)

# Latex Table
#stargazer(Y11.2, Y12.2, Y13.2, title = "Regression Results: Reader Market (2013-2015)", align = TRUE, dep.var.labels = c("FOCUS", "Der Spiegel", "Stern"), omit.stat = c("LL"), no.space = TRUE)

# Plotting Residuals
tikz("/Users/Franzi/Desktop/R/arima_circ_fss2.tex",width=5,height=3.5)
plot.ts((resid11.2)/1000, type = "l", col = "red", ylim = c(-120,120),
        yaxt = "n", ann = FALSE)

lines((resid12.2)/1000, type = "l", col = "blue")

lines((resid13.2)/1000, type = "l", col = "green")

abline(a = 0, b = 0)
legend("bottomleft", lty = c(1,1,1), lwd = c(2.5,2.5,2.5), bty = "n", horiz = FALSE,
       c("FOCUS", "Der Spiegel", "Stern"),
       col = c("red", "blue", "green"))
dev.off()

##################
# CrossCorrelation
##################

# -------- Reader Market
# 1 -------------------
par(mfrow=c(2,3))
xcorr112.1 <- Ccf(resid11.1, resid12.1,
                lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                main = "FOCUS & Der Spiegel (R.M.1)")
xcorr113.1 <- Ccf(resid11.1, resid13.1, 
                lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                main = "FOCUS & Stern (R.M.1)")
xcorr123.1 <- Ccf(resid12.1, resid13.1, 
                lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                main = "Der Spiegel & Stern (R.M.1)")
xcorr112.2 <- Ccf(resid11.2, resid12.2,
                lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                main = "FOCUS & Der Spiegel (R.M.2)")
xcorr113.2 <- Ccf(resid11.2, resid13.2, 
                lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                main = "FOCUS & Stern (R.M.2)")
xcorr123.2 <- Ccf(resid12.2, resid13.2, 
                lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                main = "Der Spiegel & Stern (R.M.2)")


xcorr1.1 <- matrix(c(xcorr112.1$lag, xcorr112.1$acf,xcorr113.1$acf,xcorr123.1$acf), ncol = 4)
colnames(xcorr1.1) <- c("Lags", "FOCUS + DerSpiegel", "FOCUS + Stern","Der Spiegel + Stern")
stargazer(xcorr1.1, summary = FALSE, title = "Cross Correlation Reader Market")
# # 2 --------------------------
xcorr1.2 <- matrix(c(xcorr112.2$lag, xcorr112.2$acf,xcorr113.2$acf,xcorr123.2$acf), ncol = 4)
colnames(xcorr1.2) <- c("Lags", "FOCUS + DerSpiegel", "FOCUS + Stern","Der Spiegel + Stern")
stargazer(xcorr1.2, summary = FALSE)

# --------Ad Market
# 1 --------------
# FOCUS
Y21.1 <- auto.arima(y21.1, xreg = cbind(y22.1, y23.1), max.d = 0)
Y21.1
Y21.1 <- Arima(y21.1, c(4,0,2), xreg = cbind(y22.1, y23.1))
# Y21.1 <- lm(y21.1 ~ y22.1 + y23.1+t1)
resid21.1 <- Y21.1$residuals
Acf(resid21.1, 20)

# Der Spiegel
Y22.1 <- auto.arima(y22.1, xreg = cbind(y21.1, y23.1),  max.d = 0)
Y22.1
Y22.1 <- Arima(y22.1, c(1,0,1), xreg = cbind(y21.1, y23.1))
Y22.1$aic
# Y22.1 <- lm(y22.1 ~ y21.1 + y23.1+t1)
resid22.1 <- Y22.1$residuals
Acf(resid22.1, 20)

# Stern
Y23.1 <- auto.arima(y23.1, xreg = cbind(y21.1, y22.1), d=0)
Y23.1
Y23.1 <- arima(y23.1, c(1,0,1), xreg = cbind(y21.1, y22.1))
Y23.1$aic
# Y23.1 <- lm(y23.1 ~ y21.1 + y22.1 + t1)
resid23.1 <- Y23.1$residuals
Acf(resid23.1, 20)

## Latex Table
#stargazer(Y21.1, Y22.1,Y23.1, title = "Ads 1, Der Spiegel(1,1), Stern(1,1), FOCUS(4,2)", align = TRUE, dep.var.labels = c("FOCUS", "Der Spiegel", "Stern"), no.space = TRUE)

## Plotting residuals
tikz("/Users/Franzi/Desktop/R/arima_ads_fss1.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts(resid21.1, type = "l", col = "red", ylim = c(-50,50), xlab = "", ylab = "")

lines(resid22.1, type = "l", col = "blue")
lines(resid23.1, type = "l", col = "green")

abline(a = 0, b = 0)
dev.off()

# 2 --------------
# FOCUS
Y21.2 <- auto.arima(y21.2, xreg = cbind(y22.2, y23.2))
Y21.2
Y21.2 <- Arima(y21.2, c(1,0,4), xreg = cbind(y22.2, y23.2))
# Y21.2 <- lm(y21.2 ~ y22.2 + y23.2 +t2)
resid21.2 <- Y21.2$residuals
Acf(resid21.2,20)

# Der Spiegel
Y22.2 <- auto.arima(y22.2, xreg = cbind(y21.2, y23.2), d=0)
Y22.2
Y22.2 <- arima(y22.2, c(1,0,0), xreg = cbind(y21.2, y23.2, t2))
# Y22.2 <- lm(y22.2 ~ y21.2 + y23.2 + t2)
resid22.2 <- Y22.2$residuals
Acf(resid22.2)

# Stern
Y23.2 <- auto.arima(y23.2, xreg = cbind(y21.2, y22.2), d =0)
Y23.2
Y23.2 <- arima(y23.2, c(1,0,0), xreg = cbind(y21.2, y22.2, t2))
# Y23.2 <- lm(y23.2 ~ y21.2 + y22.2 + t2)
resid23.2 <- Y23.2$residuals
Acf(resid23.2)

## Latex Table
stargazer(Y21.2, Y22.2, Y23.2, title = "Ads 2, Der Spiegel(1,0) Stern(1,0), FOCUS(1,0)", align = TRUE, no.space = TRUE)

# 2----------------
tikz("/Users/Franzi/Desktop/R/arima_ads_fss2.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts(resid21.2, type = "l", col = "red", ylim = c(-50,50), yaxt = "n", ann = FALSE)

lines(resid22.2, type = "l", col = "blue")
lines(resid23.2, type = "l", col = "green")

abline(a = 0, b = 0)

legend("topleft", lty = c(1,1,1), lwd = c(2.5,2.5,2.5), bty = "n", horiz = FALSE,
       c("FOCUS", "Der Spiegel", "Stern"),
       col = c("red", "blue", "green"))

dev.off()

###################
# Cross Correlation
###################
# 1 ----------------
par(mfrow=c(2,3))
xcorr212.1 <- Ccf(resid21.1, resid22.1,
                lag.max = 6, xlim=c(-6,6),  ylim=c(-1,1),
                main = "FOCUS & Der Spiegel (A.M.1)")
xcorr213.1 <- Ccf(resid21.1, resid23.1, 
                lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                main = "FOCUS & Stern (A.M.1)")
xcorr223.1 <- Ccf(resid22.1, resid23.1, 
                lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                main = "Der Spiegel & Stern (A.M.1)")
xcorr212.2 <- Ccf(resid21.2, resid22.2,
                lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                main = "FOCUS & Der Spiegel (A.M.2)")
xcorr213.2 <- Ccf(resid21.2, resid23.2, 
                lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                main = "FOCUS & Stern (A.M.2)")
xcorr223.2 <- Ccf(resid22.2, resid23.2, 
                lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                main = "Der Spiegel & Stern (A.M.2)")

# Latex Table
xcorr2.1 <- matrix(c(xcorr212.1$lag, xcorr212.1$acf,xcorr213.1$acf,xcorr223.1$acf), ncol = 4)
colnames(xcorr2.1) <- c("Lags", "FOCUS + DerSpiegel", "FOCUS + Stern","Der Spiegel + Stern")
stargazer(xcorr2.1, summary = FALSE)

# 2 ----------------
xcorr2.2 <- matrix(c(xcorr212.2$lag, xcorr212.2$acf,xcorr213.2$acf,xcorr223.2$acf), ncol = 4)
colnames(xcorr2.2) <- c("Lags", "FOCUS + DerSpiegel", "FOCUS + Stern","Der Spiegel + Stern")
stargazer(xcorr2.2, summary = FALSE)


#############
# Residual AC
#############

# --- Reader Market
# 1----------------
par(mfrow=c(3,2))

# Reader Market
Acf(resid11.1, 20, main = "FOCUS 1, ARMA(4,1)")
Acf(resid11.2, 20, main = "FOCUS 2, ARMA(1,0)")

Acf(resid12.1, 20, main = "Der Spiegel 1, ARMA(4,0)")
Acf(resid12.2, 20, main = "Der Spiegel 2, ARMA(3,2)")

Acf(resid13.1, 20, main = "Stern 1, ARMA(1,1)")
Acf(resid13.2, 20, main = "Stern 2, ARMA(1,0)")

# Ad Market
Acf(resid21.1, 20, main = "FOCUS 1, ARMA(4,2)")
#Acf(resid21.1, 20, main = "FOCUS 1, OLS")

Acf(resid21.2, 20, main = "FOCUS 2, ARMA(1,4)")
# Acf(resid21.2, 20, main = "FOCUS 2, OLS")

Acf(resid22.1, 20, main = "Der Spiegel 1, ARMA(1,1)")
# Acf(resid22.1, 20, main = "Der Spiegel 1, OLS")

Acf(resid22.2, 20, main = "Der Spiegel 2, ARMA(1,0)")
# Acf(resid22.2, 20, main = "Der Spiegel 2, OLS")

Acf(resid23.1, 20, main = "Stern 1, ARMA(1,1)")
# Acf(resid23.1, 20, main = "Stern 1, OLS")

Acf(resid23.2, 20, main = "Stern 2, ARMA(1,0)")
# Acf(resid23.2, 20, main = "Stern 2, OLS")

###################
# Granger Causality
###################

 # 1 --------------------
write.csv(cbind(resid11.1, resid12.1, resid13.1, resid21.1, resid22.1, resid23.1), file="granger_fss1.csv")

VARselect(cbind(resid11.1, resid12.1, resid13.1), lag.max = 12, type = "none")
VARselect(cbind(resid21.1, resid22.1, resid23.1), lag.max = 12, type = "none")


# 2 --------------------
write.csv(cbind(resid11.2, resid12.2, resid13.2, resid21.2, resid22.2, resid23.2), file="granger_fss2.csv")

VARselect(cbind(resid11.2, resid12.2, resid13.2), lag.max = 12, type = "none")
VARselect(cbind(resid21.2, resid22.2, resid23.2), lag.max = 12, type = "none")


# Impulse response function
# Reader Market
plot(irf(var.1.1))
plot(irf(var.1.2))

# Ad Market
plot(irf(var.2.1))
plot(irf(var.2.2))







