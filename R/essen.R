rm(list = ls())

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

# Market 7: News Magazines 
# Included Magazines: EssenTrinken, MeineFamilie&ich, Lisa
# Time Interval: 2003w1 - 2016w33

essen <- read.csv("/Users/Franzi/Desktop/R/2sm/essen.csv", header = TRUE, stringsAsFactors = FALSE)

essen <- ts(essen, start=c(2003,1), end = c(2016,12), frequency = 12)

# Assign Globals
# Sub1
y11 <- essen[,"retailEssenTrinken"]
y21 <- essen[,"adsEssenTrinken"]
x11 <- (essen[,"adsEssenTrinken"]/essen[,"contentEssenTrinken"])*100

# Sub2
y12 <- essen[,"retailmeineFamilie"]
y22 <- essen[,"adsmeineFamilie"]
x12 <- (essen[,"adsmeineFamilie"]/essen[,"contentmeineFamilie"])*100

# Sub3
y13 <- essen[,"retailLisa"]
y23 <- essen[,"adsLisa"]
x13 <- (essen[,"adsLisa"]/essen[,"contentLisa"])*100


# --------- Select Sub-Sample
# 1 --- 2004w33 - 2006w33 ---

# Sub1
y11.1 <- window(y11, start=c(2004,6), end = c(2008,6), frequency = 12)
x11.1 <- window(x11, start=c(2004,6), end = c(2008,6), frequency = 12)
y21.1 <- window(y21, start=c(2004,6), end = c(2008,6), frequency = 12)

# Sub2
y12.1 <- window(y12, start=c(2004,6), end = c(2008,6), frequency = 12)
x12.1 <- window(x12, start=c(2004,6), end = c(2008,6), frequency = 12)
y22.1 <- window(y22, start=c(2004,6), end = c(2008,6), frequency = 12)

# Sub3
y13.1 <- window(y13, start=c(2004,6), end = c(2008,6), frequency = 12)
x13.1 <- window(x13, start=c(2004,6), end = c(2008,6), frequency = 12)
y23.1 <- window(y23, start=c(2004,6), end = c(2008,6), frequency = 12)


# 2 --- 2013 - 2015 ---
# Sub1
y11.2 <- window(y11, start=c(2012,6), end = c(2015,6), frequency = 12)
x11.2 <- window(x11, start=c(2012,6), end = c(2015,6), frequency = 12)
y21.2 <- window(y21, start=c(2012,6), end = c(2015,6), frequency = 12)

# Sub2
y12.2 <- window(y12, start=c(2012,6), end = c(2015,6), frequency = 12)
x12.2 <- window(x12, start=c(2012,6), end = c(2015,6), frequency = 12)
y22.2 <- window(y22, start=c(2012,6), end = c(2015,6), frequency = 12)

# Sub3
y13.2 <- window(y13, start=c(2012,6), end = c(2015,6), frequency = 12)
x13.2 <- window(x13, start=c(2012,6), end = c(2015,6), frequency = 12)
y23.2 <- window(y23, start=c(2012,6), end = c(2015,6), frequency = 12)


# Fill NA
y11.1.ts[which(is.na(y11.1.ts))] <- mean(y11.1.ts, na.rm = TRUE)
y12.1.ts[which(is.na(y12.1.ts))] <- mean(y12.1.ts, na.rm = TRUE)
y13.1.ts[which(is.na(y13.1.ts))] <- mean(y13.1.ts, na.rm = TRUE)

y11.2.ts[which(is.na(y11.2.ts))] <- mean(y11.2.ts, na.rm = TRUE)
y12.2.ts[which(is.na(y12.2.ts))] <- mean(y12.2.ts, na.rm = TRUE)
y13.2.ts[which(is.na(y13.2.ts))] <- mean(y13.2.ts, na.rm = TRUE)
# 
y21.1.ts[which(is.na(y21.1.ts))] <- mean(y21.1.ts, na.rm = TRUE)
y22.1.ts[which(is.na(y22.1.ts))] <- mean(y22.1.ts, na.rm = TRUE)
y23.1.ts[which(is.na(y23.1.ts))] <- mean(y23.1.ts, na.rm = TRUE)

y21.2.ts[which(is.na(y21.2.ts))] <- mean(y21.2.ts, na.rm = TRUE)
y22.2.ts[which(is.na(y22.2.ts))] <- mean(y22.2.ts, na.rm = TRUE)
y23.2.ts[which(is.na(y23.2.ts))] <- mean(y23.2.ts, na.rm = TRUE)


######################
# Summary and plotting
######################

# ----- Reader Market
combined.y1 <- data.frame(y11.1, y12.1, y13.1)
combined.y1.1 <- data.frame(y11.2, y12.2, y13.2)
colnames(combined.y1) <- c("Essen&Trinken", "meineFamilie&ich", "Lisa")
colnames(combined.y1.1) <- c("Essen&Trinken", "meineFamilie&ich", "Lisa")

stargazer(combined.y1, digits = 1, title = "Summary Statistic: Reader Market")
stargazer(combined.y1.1, digits = 1, title = "Summary Statistic: Reader Market")

# ----- Ad Market
combined.y2 <- data.frame(y21.1.ts, y22.1.ts, y23.1.ts)
combined.y2.1 <- data.frame(y21.2.ts, y22.2.ts, y23.2.ts)
colnames(combined.y2) <- c("Essen&Trinken", "meineFamilie&ich", "Lisa")
colnames(combined.y2.1) <- c("Essen&Trinken", "meineFamilie&ich", "Lisa")

stargazer(combined.y2, digits = 1, title = "Summary Statistic: advertising Market")
stargazer(combined.y2.1, digits = 1, title = "Summary Statistic: advertising Market")


# ---- Reader Market
# 1 ---------------
tikz("/Users/Franzi/Desktop/R/circ_essen1.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts((y11.1.ts)/1000, 
        type = "l",
        col = "red",
        xlab = "Monthly Data",
        ylab = "Total Retail in tsd",
        ylim = c(0, 400))

lines((y12.1.ts)/1000, 
      type = "l",
      col = "blue")

lines((y13.1.ts)/1000, 
      type = "l",
      col = "green")
dev.off()

# 2 ----------------------
tikz("/Users/Franzi/Desktop/R/circ_essen2.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts((y11.2.ts)/1000, 
        type = "l",
        col = "red",
        yaxt = "n",
        ann = FALSE,
        ylim = c(0, 450))

lines((y12.2.ts)/1000, 
      type = "l",
      col = "blue")

lines((y13.2.ts)/1000, 
      type = "l",
      col = "green")
legend("topleft",
       c("Essen&Trinken", "meineFamilie&ich", "Lisa"),
       lty = c(1,1,1),
       lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)
dev.off()

#-------- Ad Market
# 1 ---------------
tikz("/Users/Franzi/Desktop/R/ads_essen1.tex",width=5,height=3.5)
plot.ts(y21.1.ts, 
        type = "l",
        col = "red",
        xlab = "Monthly Data",
        ylab = "Total Ad Pages",
        ylim = c(0,70))

lines(y22.1.ts, 
      type = "l",
      col = "blue")

lines(y23.1.ts, 
      type = "l",
      col = "green")
dev.off()
#2-----------------
tikz("/Users/Franzi/Desktop/R/ads_essen2.tex",width=5,height=3.5)
plot.ts(y21.2.ts, 
        type = "l",
        col = "red",
        yaxt = "n",
        ann = FALSE,
        ylim = c(0,70))

lines(y22.2.ts, 
      type = "l",
      col = "blue")

lines(y23.2.ts, 
      type = "l",
      col = "green")
legend("topleft",
       c("Essen&Trinken", "meineFamilie&ich", "Lisa"),
       lty = c(1,1,1),
       lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)
dev.off()

#######
# ACF #
#######

par(mfrow=c(3,2))         

Acf(y11.1.ts, 20, main = "Essen&Trinken (R.M.1)")
Acf(y11.2.ts,20, main = "Essen&Trinken (R.M.2)")

Acf(y12.1.ts,20, main = "MeineFamilie&ich (R.M.1)")
Acf(y12.2.ts, 20, main = "MeineFamilie&ich (R.M.2)")

Acf(y13.1.ts, 20, main = "Lisa (R.M.1)")
Acf(y13.2.ts, 20, main = "Lisa (R.M.2)")

Acf(y21.1.ts, 20, main = "Essen&Trinken (A.M.1)")
Acf(y21.2.ts, 20, main = "Essen&Trinken (A.M.2)")

Acf(y22.1.ts, 20, main = "MeineFamilie&ich (A.M.1)")
Acf(y22.2.ts, 20, main = "MeineFamilie&ich (A.M.2)")

Acf(y23.1.ts, 20, main = "Lisa (A.M.1)")
Acf(y23.2.ts, 20, main = "Lisa (A.M.2)")


###################################
# Phillips \& Perron Unit Root Test
###################################

# Null-Hypothesis: Series is non-stationary. If the test statistic is bigger than the critical value, we cannot reject the Null and Series is non-stationary. 

# ------- Reader Market
# 1 ------------------
y11.1.p <- (ur.pp(y11.1.ts, type = "Z-tau", model = "trend", lags = "short"))
y12.1.p <- (ur.pp(y12.1.ts, type = "Z-tau", model = "trend", lags = "short"))
y13.1.p <- (ur.pp(y13.1.ts, type = "Z-tau", model = "trend", lags = "short"))

# 2 ------------------
y11.2.p <- (ur.pp(y11.2.ts, type = "Z-tau", model = "trend", lags = "short"))
y12.2.p <- (ur.pp(y12.2.ts, type = "Z-tau", model = "trend", lags = "short"))
y13.2.p <- (ur.pp(y13.2.ts, type = "Z-tau", model = "trend", lags = "short"))


# ------- Ad Market
# 1 ------------------
y21.1.p <- (ur.pp(y21.1.ts, type = "Z-tau", model = "trend", lags = "short"))
y22.1.p <- (ur.pp(y22.1.ts, type = "Z-tau", model = "trend", lags = "short"))
y23.1.p <- (ur.pp(y23.1.ts, type = "Z-tau", model = "trend", lags = "short"))

# 2 ------------------
y21.2.p <- (ur.pp(y21.2.ts, type = "Z-tau", model = "trend", lags = "short"))
y22.2.p <- (ur.pp(y22.2.ts, type = "Z-tau", model = "trend", lags = "short"))
y23.2.p <- (ur.pp(y23.2.ts, type = "Z-tau", model = "trend", lags = "short"))

# ------ Latex Table 
# 2004-2006---------
c.y1p <- data.frame(y11.1.p@teststat, y12.1.p@teststat, y13.1.p@teststat)
c.y1p[nrow(c.y1p)+1,]<-c(y21.1.p@teststat, y22.1.p@teststat, y23.1.p@teststat)
c.y1p[nrow(c.y1p)+1,]<-c("1pct","5pct","10pct")
c.y1p[nrow(c.y1p)+1,]<-y11.1.p@cval
colnames(c.y1p) <- c("Essen&Trinken", "MeineFamilie&ich", "Lisa")
rownames(c.y1p) <- c("Sales", "Ad pages","Sig. Level", "Critical Values")

stargazer(c.y1p, summary = FALSE, digits = 1, title = "Unit Root: 2004-2006")

# 2013-2015-------- 
c.y2p <- data.frame(y11.2.p@teststat, y12.2.p@teststat, y13.2.p@teststat)
c.y2p[nrow(c.y2p)+1,]<-c(y21.2.p@teststat, y22.2.p@teststat, y23.2.p@teststat)
c.y2p[nrow(c.y2p)+1,]<-c("1pct","5pct","10pct")
c.y2p[nrow(c.y2p)+1,]<-y11.2.p@cval
colnames(c.y2p) <- c("Essen&Trinken", "MeineFamilie&ich", "Lisa")
rownames(c.y2p) <- c("Sales", "Ad pages","Sig. Level", "Critical Values")

stargazer(c.y2p, summary = FALSE, digits = 1, title = "Unit Root: 2013-2015")

######################
# Prewhitening / ARIMA
######################

# ------ Reader Markt
# 1 ----------------

# Essen&Trinken
Y11.1 <- auto.arima(y11.1.ts, xreg = cbind(y12.1.ts, y13.1.ts))
Y11.1
#Y11.1 <- lm(y11.1.ts ~ y12.1.ts + y13.1.ts)
resid11.1 <- Y11.1$residuals

# MeineFamilie&ich
Y12.1 <- auto.arima(y12.1.ts, xreg = cbind(y11.1.ts, y13.1.ts))
Y12.1
#Y12.1 <- lm(y12.1.ts ~ y11.1.ts + y13.1.ts)
resid12.1 <- Y12.1$residuals

# Lisa
Y13.1 <- auto.arima(y13.1.ts, xreg = cbind(y11.1.ts, y12.1.ts))
Y13.1
#Y13.1 <- lm(y13.1.ts ~ y11.1.ts + y12.1.ts)
resid13.1 <- Y13.1$residuals

# Latex Table
#stargazer(Y11.1, Y12.1, Y13.1, title = "Regression Results: Reader Market (2004-2006)", align = TRUE, dep.var.labels = c("Essen&Trinken", "MeineFamilie&ich", "Lisa"), omit.stat = c("LL"), no.space = TRUE)

# Plotting residuals
#tikz("/Users/Franzi/Desktop/R/arima_circ_essen1.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts((resid11.1)/1000, 
        type = "l",
        col = "red",
        ylim = c(-120,120),
        xlab = "Weekly Data",
        ylab = "Residuals")

lines((resid12.1)/1000, 
      type = "l",
      col = "blue")

lines((resid13.1)/1000, 
      type = "l",
      col = "green")

abline(a = 0, b = 0)
#dev.off()

# 2 ------------
# Essen&Trinken
Y11.2 <- auto.arima(y11.2.ts, xreg = cbind(y12.2.ts, y13.2.ts))
Y11.2
#Y11.2 <- arima(y11.2.ts, order = c(0,0,0), xreg = cbind(y12.2.ts, y13.2.ts))
resid11.2 <- Y11.2$residuals

# MeineFamilie&ich
Y12.2 <- auto.arima(y12.2.ts, xreg = cbind(y11.2.ts, y13.2.ts))
Y12.2
# Y12.2 <- arima(y12.2.ts, c(0,1,1), xreg = cbind(y11.2.ts, y13.2.ts))
resid12.2 <- Y12.2$residuals

# Lisa
Y13.2 <- auto.arima(y13.2.ts, xreg = cbind(y11.2.ts, y12.2.ts))
Y13.2
# Y13.2 <- arima(y13.2.ts, c(1,0,0), xreg = cbind(y11.2.ts, y12.2.ts))
resid13.2 <- Y13.2$residuals

# Latex Table
#stargazer(Y11.2, Y12.2, Y13.2, title = "Regression Results: Reader Market (2013-2015)", align = TRUE, dep.var.labels = c("Essen&Trinken", "MeineFamilie&ich", "Lisa"), omit.stat = c("LL"), no.space = TRUE)

# Plotting Residuals
#tikz("/Users/Franzi/Desktop/R/arima_circ_essen2.tex",width=5,height=3.5)
plot.ts((resid11.2)/1000, 
        type = "l",
        col = "red",
        ylim = c(-120,120),
        yaxt = "n",
        ann = FALSE)

lines((resid12.2)/1000, 
      type = "l",
      col = "blue")

lines((resid13.2)/1000, 
      type = "l",
      col = "green")

abline(a = 0, b = 0)
legend("bottomleft",
       c("Essen&Trinken", "MeineFamilie&ich", "Lisa"),
       lty = c(1,1,1),
       lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)
dev.off()

##################
# CrossCorrelation
##################

# -------- Reader Market
# 1 -------------------
par(mfrow=c(2,3))
xcorr112.1 <- Ccf(resid11.1, resid12.1,
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "Essen&Trinken & MeineFamilie&ich (R.M.1)")
xcorr113.1 <- Ccf(resid11.1, resid13.1, 
                  lag.max = 6, 
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "Essen&Trinken & Lisa (R.M.1)")
xcorr123.1 <- Ccf(resid12.1, resid13.1, 
                  lag.max = 6, 
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "MeineFamilie&ich & Lisa (R.M.1)")
xcorr112.2 <- Ccf(resid11.2, resid12.2,
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "Essen&Trinken & MeineFamilie&ich (R.M.2)")
xcorr113.2 <- Ccf(resid11.2, resid13.2, 
                  lag.max = 6, 
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "Essen&Trinken & Lisa (R.M.2)")
xcorr123.2 <- Ccf(resid12.2, resid13.2, 
                  lag.max = 6, 
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "MeineFamilie&ich & Lisa (R.M.2)")


xcorr1.1 <- matrix(c(xcorr112.1$lag, xcorr112.1$acf,xcorr113.1$acf,xcorr123.1$acf), ncol = 4)
colnames(xcorr1) <- c("Lags", "Essen&Trinken + MeineFamilie&ich", "Essen&Trinken + Lisa","MeineFamilie&ich + Lisa")
stargazer(xcorr1.1, summary = FALSE, title = "Cross Correlation Reader Market")
# 2 --------------------------
xcorr1.2 <- matrix(c(xcorr112.2$lag, xcorr112.2$acf,xcorr113.2$acf,xcorr123.2$acf), ncol = 4)
colnames(xcorr1.2) <- c("Lags", "Essen&Trinken + MeineFamilie&ich", "Essen&Trinken + Lisa","MeineFamilie&ich + Lisa")
stargazer(xcorr1.2, summary = FALSE)

# --------Ad Market
# 1 --------------
# Essen&Trinken
Y21.1 <- auto.arima(y21.1.ts, xreg = cbind(y22.1.ts, y23.1.ts))
Y21.1
#Y21.1 <- arima(y21.1.ts, c(1,0,0), xreg = cbind(y22.1.ts, y23.1.ts))
resid21.1 <- Y21.1$residuals

# MeineFamilie&ich
Y22.1 <- auto.arima(y22.1.ts, xreg = cbind(y21.1.ts, y23.1.ts))
Y22.1
#Y22.1 <- arima(y22.1.ts, c(1,0,0), xreg = cbind(y21.1.ts, y23.1.ts))
resid22.1 <- Y22.1$residuals

# Lisa
Y23.1 <- auto.arima(y23.1.ts, xreg = cbind(y21.1.ts, y22.1.ts))
Y23.1
#Y23.1 <- arima(y23.1.ts, c(1,0,0), xreg = cbind(y21.1.ts, y22.1.ts))
resid23.1 <- Y23.1$residuals

## Latex Table
#stargazer(Y21.1, Y22.1, Y23.1, title = "Regression Results: Ad Market (2004-2006)", align = TRUE, dep.var.labels = c("Essen&Trinken", "MeineFamilie&ich", "Lisa"), omit.stat = c("LL"), no.space = TRUE)

## Plotting residuals
#tikz("/Users/Franzi/Desktop/R/arima_ads_essen1.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts(resid21.1, 
        type = "l",
        col = "red",
        ylim = c(-50,50),
        xlab = "Weekly Data",
        ylab = "Residuals")

lines(resid22.1, 
      type = "l",
      col = "blue")

lines(resid23.1, 
      type = "l",
      col = "green")

abline(a = 0, b = 0)
dev.off()

# 2 --------------
# Essen&Trinken
Y21.2 <- auto.arima(y21.2.ts, xreg = cbind(y22.2.ts, y23.2.ts))
Y21.2
# Y21.2 <- arima(y21.2.ts, c(2,0,0), xreg = cbind(y22.2.ts, y23.2.ts))
resid21.2 <- Y21.2$residuals

# MeineFamilie&ich
Y22.2 <- auto.arima(y22.2.ts, xreg = cbind(y21.2.ts, y23.2.ts))
Y22.2
# Y22.2 <- arima(y22.2.ts, c(0,1,1), xreg = cbind(y21.2.ts, y23.2.ts))
resid22.2 <- Y22.2$residuals

# Lisa
Y23.2 <- auto.arima(y23.2.ts, xreg = cbind(y21.2.ts, y22.2.ts))
Y23.2
# Y23.2 <- arima(y23.2.ts, c(0,0,0),xreg = cbind(y21.2.ts, y22.2.ts))
resid23.2 <- Y23.2$residuals

## Latex Table
#stargazer(Y21.2, Y22.2, Y23.2, title = "Regression Results: Ad Market", align = TRUE, dep.var.labels = c("Essen&Trinken", "MeineFamilie&ich", "Lisa"), omit.stat = c("LL"), no.space = TRUE)

# 2----------------
#tikz("/Users/Franzi/Desktop/R/arima_ads_essen2.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts(resid21.2, 
        type = "l",
        col = "red",
        ylim = c(-50,50),
        yaxt = "n",
        ann = FALSE)

lines(resid22.2, 
      type = "l",
      col = "blue")

lines(resid23.2, 
      type = "l",
      col = "green")

abline(a = 0, b = 0)

legend("topleft",
       c("Essen&Trinken", "MeineFamilie&ich", "Lisa"),
       lty = c(1,1,1),
       lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)

dev.off()

###################
# Cross Correlation
###################
# 1 ----------------
par(mfrow=c(2,3))
xcorr212.1 <- Ccf(resid21.1, resid22.1,
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "Essen&Trinken & MeineFamilie&ich (A.M.1)")
xcorr213.1 <- Ccf(resid21.1, resid23.1, 
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "Essen&Trinken & Lisa (A.M.1)")
xcorr223.1 <- Ccf(resid22.1, resid23.1, 
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "MeineFamilie&ich & Lisa (A.M.1)")
xcorr212.2 <- Ccf(resid21.2, resid22.2,
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "Essen&Trinken & MeineFamilie&ich (A.M.2)")
xcorr213.2 <- Ccf(resid21.2, resid23.2, 
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "Essen&Trinken & Lisa (A.M.2)")
xcorr223.2 <- Ccf(resid22.2, resid23.2, 
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "MeineFamilie&ich & Lisa (A.M.2)")

# Latex Table
xcorr2.1 <- matrix(c(xcorr212.1$lag, xcorr212.1$acf,xcorr213.1$acf,xcorr223.1$acf), ncol = 4)
colnames(xcorr2.1) <- c("Lags", "Essen&Trinken + MeineFamilie&ich", "Essen&Trinken + Lisa","MeineFamilie&ich + Lisa")
stargazer(xcorr2.1, summary = FALSE)

# 2 ----------------
xcorr2.2 <- matrix(c(xcorr212.2$lag, xcorr212.2$acf,xcorr213.2$acf,xcorr223.2$acf), ncol = 4)
colnames(xcorr2.2) <- c("Lags", "Essen&Trinken + MeineFamilie&ich", "Essen&Trinken + Lisa","MeineFamilie&ich + Lisa")
stargazer(xcorr2.2, summary = FALSE)


#############
# Residual AC
#############

# --- Reader Market
# 1----------------
par(mfrow=c(3,2))

# Reader Market
Acf(resid11.1, 20, main = "Essen&Trinken (R.M.1)")
Acf(resid11.2, 20, main = "Essen&Trinken (R.M.2)")

Acf(resid12.1, 20, main = "MeineFamilie&ich (R.M.1)")
Acf(resid12.2, 20, main = "MeineFamilie&ich (R.M.2)")

Acf(resid13.1, 20, main = "Lisa (R.M.1)")
Acf(resid13.2, 20, main = "Lisa (R.M.2)")

# Ad Market
Acf(resid21.1, 20, main = "Essen&Trinken (A.M.1)")
Acf(resid21.2, 20, main = "Essen&Trinken (A.M.2)")

Acf(resid22.1, 20, main = "MeineFamilie&ich (A.M.1)")
Acf(resid22.2, 20, main = "MeineFamilie&ich (A.M.2)")

Acf(resid23.1, 20, main = "Lisa (A.M.1)")
Acf(resid23.2, 20, main = "Lisa (A.M.2)")

###################
# Granger Causality
###################

# 1 --------------------
write.csv(cbind(resid11.1, resid12.1, resid13.1, resid21.1, resid22.1, resid23.1), file="granger_essen1.csv")

VARselect(cbind(resid11.1, resid12.1, resid13.1), lag.max = 12, type = "none")
VARselect(cbind(resid21.1, resid22.1, resid23.1), lag.max = 12, type = "none")


# 2 --------------------
write.csv(cbind(resid11.2, resid12.2, resid13.2, resid21.2, resid22.2, resid23.2), file="granger_essen2.csv")

VARselect(cbind(resid11.2, resid12.2, resid13.2), lag.max = 12, type = "none")
VARselect(cbind(resid21.2, resid22.2, resid23.2), lag.max = 12, type = "none")


# Impulse response function
# Reader Market
plot(irf(var.1.1))
plot(irf(var.1.2))

# Ad Market
plot(irf(var.2.1))
plot(irf(var.2.2))







