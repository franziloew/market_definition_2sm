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
library("plotrix")


# Included Magazines: TVMovie, TVSpielfilm, TVDigital


tv <- read.csv("/Users/Franzi/Desktop/R/2sm/tv.csv", header = TRUE, stringsAsFactors = FALSE)

tv <- ts(tv, start=c(2003,1), end = c(2016,25), frequency = 26)

# Assign Globals
# Sub1
y31 <- tv[,"retailTVMovie"]
y41 <- tv[,"totaladpageTVMovie"]
x41 <- (tv[,"totaladpageTVMovie"]/tv[,"contentTVMovie"])*100

# Sub2
y32 <- tv[,"retailTVSpielfilm"] 
y42 <- tv[,"totaladpageTVSpielfilm"]
x42 <- (tv[,"totaladpageTVSpielfilm"]/tv[,"contentTVSpielfilm"])*100

# Sub3
y33 <- tv[,"retailTVDigital"]
y43 <- tv[,"totaladpageTVDigital"]
x43 <- (tv[,"totaladpageTVDigital"]/tv[,"contentTVDigital"])*100


# --------- Select Sub-Sample
# 1 --- 2004w33 - 2006w33 ---

# Sub1
y31.1 <- window(y31, start=c(2005,15), end = c(2008,15), frequency = 26)
x41.1 <- window(x41, start=c(2005,15), end = c(2008,15), frequency = 26)
y41.1 <- window(y41, start=c(2005,15), end = c(2008,15), frequency = 26)

# Sub2
y32.1 <- window(y32,  start=c(2005,15), end = c(2008,15), frequency = 26)
x42.1 <- window(x42,  start=c(2005,15), end = c(2008,15), frequency = 26)
y42.1 <- window(y42,  start=c(2005,15), end = c(2008,15), frequency = 26)

# Sub3
y33.1 <- window(y33,  start=c(2005,15), end = c(2008,15), frequency = 26)
x43.1 <- window(x43,  start=c(2005,15), end = c(2008,15), frequency = 26)
y43.1 <- window(y43,  start=c(2005,15), end = c(2008,15), frequency = 26)


# 2 --- 2012/15 - 2015/15 ---
# Sub1
y31.2 <- window(y31, start=c(2012,15), end = c(2015,15), frequency = 26)
x41.2 <- window(x41, start=c(2012,15), end = c(2015,15), frequency = 26)
y41.2 <- window(y41, start=c(2012,15), end = c(2015,15), frequency = 26)

# Sub2
y32.2 <- window(y32,  start=c(2012,15), end = c(2015,15), frequency = 26)
x42.2 <- window(x42,  start=c(2012,15), end = c(2015,15), frequency = 26)
y42.2 <- window(y42,  start=c(2012,15), end = c(2015,15), frequency = 26)

# Sub3
y33.2 <- window(y33,  start=c(2012,15), end = c(2015,15), frequency = 26)
x43.2 <- window(x43,  start=c(2012,15), end = c(2015,15), frequency = 26)
y43.2 <- window(y43,  start=c(2012,15), end = c(2015,15), frequency = 26)


# Fill NA
y31.1[which(is.na(y31.1))] <- mean(y31.1, na.rm = TRUE)
y32.1[which(is.na(y32.1))] <- mean(y32.1, na.rm = TRUE)
y33.1[which(is.na(y33.1))] <- mean(y33.1, na.rm = TRUE)

y31.2[which(is.na(y31.2))] <- mean(y31.2, na.rm = TRUE)
y32.2[which(is.na(y32.2))] <- mean(y32.2, na.rm = TRUE)
y33.2[which(is.na(y33.2))] <- mean(y33.2, na.rm = TRUE)

# 
y41.1[which(is.na(y41.1))] <- mean(y41.1, na.rm = TRUE)
y42.1[which(is.na(y42.1))] <- mean(y42.1, na.rm = TRUE)
y43.1[which(is.na(y43.1))] <- mean(y43.1, na.rm = TRUE)

y41.2[which(is.na(y41.2))] <- mean(y41.2, na.rm = TRUE)
y42.2[which(is.na(y42.2))] <- mean(y42.2, na.rm = TRUE)
y43.2[which(is.na(y43.2))] <- mean(y43.2, na.rm = TRUE)



######################
# Summary and plotting
######################

# ----- Reader Market
combined.y1 <- data.frame(y31.1, y32.1, y33.1)
combined.y1.1 <- data.frame(y31.2, y32.2, y33.2)
colnames(combined.y1) <- c("TVMovie", "TVSpielfilm", "TVDigital")
colnames(combined.y1.1) <- c("TVMovie", "TVSpielfilm", "TVDigital")

stargazer(combined.y1, digits = 1, title = "Summary Statistic: Reader Market (2005-2008)")
stargazer(combined.y1.1, digits = 1, title = "Summary Statistic: Reader Market (2012-2015)")

# ----- Ad Market
combined.y2 <- data.frame(y41.1, y42.1, y43.1)
combined.y2.1 <- data.frame(y41.2, y42.2, y43.2)
colnames(combined.y2) <- c("TVMovie", "TVSpielfilm", "TVDigital")
colnames(combined.y2.1) <- c("TVMovie", "TVSpielfilm", "TVDigital")

stargazer(combined.y2, digits = 1, title = "Summary Statistic: advertising Market (2005-2008)")
stargazer(combined.y2.1, digits = 1, title = "Summary Statistic: advertising Market (2012-2015)")


# ---- Reader Market
# 1 ---------------
tikz("/Users/Franzi/Desktop/R/circ_tv1.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts((y31.1)/1000, 
        type = "l",
        col = "red",
        xlab = "Weekly Data",
        ylab = "Total Retail in tsd",
        ylim = c(800, 2200))

lines((y32.1)/1000, type = "l", col = "blue")

lines((y33.1)/1000, type = "l",col = "green")

legend("bottomleft", c("TVMovie", "TVSpielfilm", "TVDigital"),
       lty = c(1,1,1),
       lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)

dev.off()

# 2 ----------------------
tikz("/Users/Franzi/Desktop/R/circ_tv2.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts((y31.2)/1000, 
        type = "l",
        col = "red",
        yaxt = "n",
        ann = FALSE,
        ylim = c(800, 2200))

lines((y32.2)/1000, type = "l", col = "blue")

lines((y33.2)/1000, type = "l", col = "green")

dev.off()

#-------- Ad Market
# 1 ---------------
tikz("/Users/Franzi/Desktop/R/ads_tv1.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts(y41.1, 
        type = "l",
        col = "red",
        xlab = "Fortnightly Data",
        ylab = "Total ad pages",
        ylim = c(10, 100))

lines(y42.1, type = "l", col = "blue")

lines(y43.1, type = "l",col = "green")

dev.off()

# 2 ----------------------
tikz("/Users/Franzi/Desktop/R/ads_tv2.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts(y41.2, 
        type = "l",
        col = "red",
        yaxt = "n",
        ann = FALSE,
        ylim = c(10, 100))

lines(y42.2, type = "l", col = "blue")

lines(y43.2, type = "l",col = "green")

legend("topleft", c("TVMovie", "TVSpielfilm", "TVDigital"),
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

Acf(y31.1, 20, main = "TVMovie (R.M.1)")
Acf(y31.2, 20, main = "TVMovie (R.M.2)")

Acf(y32.1, 20, main = "TVSpielfilm (R.M.1)")
Acf(y32.2, 20, main = "TVSpielfilm (R.M.2)")

Acf(y33.1, 20, main = "TVDigital (R.M.1)")
Acf(y33.2, 20, main = "TVDigital (R.M.2)")

Acf(y41.1, 20, main = "TVMovie (A.M.1)")
Acf(y41.2, 20, main = "TVMovie (A.M.2)")

Acf(y42.1, 20, main = "TVSpielfilm (A.M.1)")
Acf(y42.2, 20, main = "TVSpielfilm (A.M.2)")

Acf(y43.1, 20, main = "TVDigital (A.M.1)")
Acf(y43.2, 20, main = "TVDigital (A.M.2)")

###################################
# Phillips \& Perron Unit Root Test
###################################

# Null-Hypothesis: Series is non-stationary. If the test statistic is bigger than the critical value, we cannot reject the Null and Series is non-stationary. 

# ------- Reader Market
# 1 ------------------
y31.1.p <- (ur.pp(y31.1, type = "Z-tau", model = "trend", lags = "short"))
y32.1.p <- (ur.pp(y32.1, type = "Z-tau", model = "trend", lags = "short"))
y33.1.p <- (ur.pp(y33.1, type = "Z-tau", model = "trend", lags = "short"))

# 2 ------------------
y31.2.p <- (ur.pp(y31.2, type = "Z-tau", model = "trend", lags = "short"))
y32.2.p <- (ur.pp(y32.2, type = "Z-tau", model = "trend", lags = "short"))
y33.2.p <- (ur.pp(y33.2, type = "Z-tau", model = "trend", lags = "short"))


# ------- Ad Market
# 1 ------------------
y41.1.p <- (ur.pp(y41.1, type = "Z-tau", model = "trend", lags = "short"))
y42.1.p <- (ur.pp(y42.1, type = "Z-tau", model = "trend", lags = "short"))
y43.1.p <- (ur.pp(y43.1, type = "Z-tau", model = "trend", lags = "short"))

# 2 ------------------
y41.2.p <- (ur.pp(y41.2, type = "Z-tau", model = "trend", lags = "short"))
y42.2.p <- (ur.pp(y42.2, type = "Z-tau", model = "trend", lags = "short"))
y43.2.p <- (ur.pp(y43.2, type = "Z-tau", model = "trend", lags = "short"))

# ------ Latex Table 
# 2004-2006---------
c.y1p <- data.frame(y31.1.p@teststat, y32.1.p@teststat, y33.1.p@teststat)
c.y1p[nrow(c.y1p)+1,]<-c(y41.1.p@teststat, y42.1.p@teststat, y43.1.p@teststat)
c.y1p[nrow(c.y1p)+1,]<-c("1pct","5pct","10pct")
c.y1p[nrow(c.y1p)+1,]<-y31.1.p@cval
colnames(c.y1p) <- c("TVMovie", "TVSpielfilm", "TVDigital")
rownames(c.y1p) <- c("Sales", "Ad pages","Sig. Level", "Critical Values")

stargazer(c.y1p, summary = FALSE, digits = 1, title = "Unit Root: 2005-2008")

# 2013-2015-------- 
c.y2p <- data.frame(y31.2.p@teststat, y32.2.p@teststat, y33.2.p@teststat)
c.y2p[nrow(c.y2p)+1,]<-c(y41.2.p@teststat, y42.2.p@teststat, y43.2.p@teststat)
c.y2p[nrow(c.y2p)+1,]<-c("1pct","5pct","10pct")
c.y2p[nrow(c.y2p)+1,]<-y31.2.p@cval
colnames(c.y2p) <- c("TVMovie", "TVSpielfilm", "TVDigital")
rownames(c.y2p) <- c("Sales", "Ad pages","Sig. Level", "Critical Values")

stargazer(c.y2p, summary = FALSE, digits = 1, title = "Unit Root: 2012-2015")

######################
# Prewhitening / ARIMA
######################

# ------ Reader Markt
# 1 ----------------

# TVMovie
Y31.1 <- auto.arima(y31.1, xreg = cbind(y32.1, y33.1))
Y31.1
#Y31.1 <- arima(y31.1, order = c(2,0,1), seasonal = list(order=c(1L,0L,0L)), xreg = cbind(y32.1, y33.1))
resid31.1 <- Y31.1$residuals

# TVSpielfilm
Y32.1 <- auto.arima(y32.1, xreg = cbind(y31.1, y33.1))
Y32.1
#Y32.1 <- arima(y32.1, c(1,1,1), seasonal = list(order=c(1L,0L,0L)), xreg = cbind(y31.1, y33.1))
resid32.1 <- Y32.1$residuals

# TVDigital
Y33.1 <- auto.arima(y33.1, xreg = cbind(y31.1, y32.1))
Y33.1
#Y33.1 <- arima(y33.1, order = c(1,0,2), xreg = cbind(y31.1, y32.1))
resid33.1 <- Y33.1$residuals


# Latex Table
# stargazer(Y31.1, Y32.1, Y33.1, title = "Regression Results: Reader Market (2005-2008)", align = TRUE, dep.var.labels = c("TVMovie", "TVSpielfilm", "TVDigital", "TVDigital"), omit.stat = c("LL"), no.space = TRUE)

# Plotting residuals
#tikz("/Users/Franzi/Desktop/R/arima_circ_tv1.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts((resid31.1)/1000, 
        type = "l",
        col = "red",
        ylim = c(-200,200),
        xlab = "Weekly Data",
        ylab = "Residuals")

lines((resid32.1)/1000, type = "l", col = "blue")
lines((resid33.1)/1000, type = "l", col = "green")

abline(a = 0, b = 0)
#dev.off()

# 2 ------------
# TVMovie
Y31.2 <- auto.arima(y31.2, xreg = cbind(y32.2, y33.2))
Y31.2
#Y31.2 <- arima(y31.2, order = c(1,1,0), seasonal = list(order=c(1L,0L,0L)),  xreg = cbind(y32.2, y33.2))
resid31.2 <- Y31.2$residuals

# TVSpielfilm
Y32.2 <- auto.arima(y32.2, xreg = cbind(y31.2, y33.2))
Y32.2
#Y32.2 <- arima(y32.2, c(1,1,0),  seasonal = list(order=c(1L,0L,0L)), xreg = cbind(y31.2, y33.2))
resid32.2 <- Y32.2$residuals

# TVDigital
Y33.2 <- auto.arima(y33.2, xreg = cbind(y31.2, y32.2))
Y33.2
#Y33.2 <- arima(y33.2, c(1,0,0), seasonal = list(order=c(1L,0L,0L)), xreg = cbind(y31.2, y32.2))
resid33.2 <- Y33.2$residuals

# Latex Table
#stargazer(Y31.2, Y32.2, Y33.2, title = "Regression Results: Reader Market (2012-2015)", align = TRUE, dep.var.labels = c("TVMovie", "TVSpielfilm", "TVDigital"), omit.stat = c("LL"), no.space = TRUE)

# Plotting Residuals
#tikz("/Users/Franzi/Desktop/R/arima_circ_tv2.tex",width=5,height=3.5)
plot.ts((resid31.2)/1000, 
        type = "l",
        col = "red",
        ylim = c(-200,200),
        yaxt = "n",
        ann = FALSE)

lines((resid32.2)/1000, type = "l", col = "blue")
lines((resid33.2)/1000, type = "l", col = "green")

abline(a = 0, b = 0)
legend("bottomleft",
       c("TVMovie", "TVSpielfilm", "TVDigital"),
       lty = c(1,1,1,1),
       lwd = c(2.5,2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)
#dev.off()

##################
# CrossCorrelation
##################

# -------- Reader Market
# 1 -------------------
#
xcorr1.1 <- matrix(c(xcorr112.1$lag, xcorr112.1$acf,xcorr113.1$acf,xcorr123.1$acf), ncol = 4)
colnames(xcorr1.1) <- c("Lags", "TVMovie + TVSpielfilm", "TVMovie + TVDigital","TVSpielfilm + TVDigital")
stargazer(xcorr1.1, summary = FALSE, title = "Cross Correlation Reader Market")
# 2 --------------------------


xcorr1.2 <- matrix(c(xcorr112.2$lag, xcorr112.2$acf,xcorr113.2$acf,xcorr123.2$acf), ncol = 4)
colnames(xcorr1.2) <- c("Lags", "TVMovie + Spielfilm", "TVMovie + TVDigital","TVSpielfilm + TVDigital")
stargazer(xcorr1.2, summary = FALSE)

# --------Ad Market
# 1 ----------------

# TVMovie
Y41.1 <- auto.arima(y41.1, xreg = cbind(y42.1, y43.1))
Y41.1
#Y41.1 <- arima(y41.1, order = c(4,1,1), seasonal = list(order=c(0L,0L,1L)), xreg = cbind(y42.1, y43.1))
resid41.1 <- Y41.1$residuals

# TVSpielfilm
Y42.1 <- auto.arima(y42.1, xreg = cbind(y41.1, y43.1))
Y42.1
#Y42.1 <- arima(y42.1, c(2,1,0), xreg = cbind(y41.1, y43.1))
resid42.1 <- Y42.1$residuals

# TVDigital
Y43.1 <- auto.arima(y43.1, xreg = cbind(y41.1, y42.1))
Y43.1
#Y43.1 <- arima(y43.1, order = c(0,0,0), seasonal = list(order=c(1L,0L,0L)), xreg = cbind(y41.1, y42.1))
resid43.1 <- Y43.1$residuals


# ## Latex Table
#stargazer(Y41.1, Y42.1, Y43.1, title = "Regression Results: Ad Market (2005-2008)", align = TRUE, dep.var.labels = c("TVMovie", "TVSpielfilm", "TVDigital"), omit.stat = c("LL"), no.space = TRUE)

## Plotting residuals
#tikz("/Users/Franzi/Desktop/R/arima_ads_tv1.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts(resid41.1, 
        type = "l",
        col = "red",
        ylim = c(-30,30),
        xlab = "Weekly Data",
        ylab = "Residuals")

lines(resid42.1, type = "l", col = "blue")
lines(resid43.1, type = "l", col = "green")

abline(a = 0, b = 0)
#dev.off()

# 2 --------------
# TVMovie
Y41.2 <- auto.arima(y41.2, xreg = cbind(y42.2,y43.2))
Y41.2
#Y41.2 <- arima(y41.2, c(1,0,0), xreg = cbind(y42.2,y43.2))
resid41.2 <- Y41.2$residuals

# TVSpielfilm
Y42.2 <- auto.arima(y42.2, xreg = cbind(y41.2, y43.2))
Y42.2
Y42.2 <- arima(y42.2, c(2,0,0), seasonal = list(order=c(1L,0L,0L)), xreg = cbind(y41.2, y43.2))
resid42.2 <- Y42.2$residuals

# TVDigital
Y43.2 <- auto.arima(y43.2, xreg = cbind(y41.2, y42.2))
Y43.2
#Y43.2 <- arima(y43.2, c(1,0,1), xreg = cbind(y41.2, y42.2))
resid43.2 <- Y43.2$residuals


## Latex Table
#stargazer(Y41.2, Y42.2, Y43.2, title = "Regression Results: Ad Market 2012-2015", align = TRUE, dep.var.labels = c("TVMovie", "TVSpielfilm", "TVDigital"), omit.stat = c("LL"), no.space = TRUE)

# 2----------------
#tikz("/Users/Franzi/Desktop/R/arima_ads_tv2.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts(resid41.2, 
        type = "l",
        col = "red",
        ylim = c(-30,30),
        yaxt = "n",
        ann = FALSE)

lines(resid42.2, type = "l", col = "blue")
lines(resid43.2, type = "l", col = "green")

abline(a = 0, b = 0)
legend("topleft",
       c("TVMovie", "TVSpielfilm", "TVDigital"),
       lty = c(1,1,1,1),
       lwd = c(2.5,2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)

#dev.off()

###################
# Cross Correlation
###################
# 1 ----------------
par(mfrow=c(2,3))
xcorr212.1 <- Ccf(resid41.1, resid42.1,
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "TVMovie & TVSpielfilm (A.M.1)")
xcorr213.1 <- Ccf(resid41.1, resid43.1, 
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "TVMovie & TVDigital (A.M.1)")
xcorr223.1 <- Ccf(resid42.1, resid43.1, 
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "TVSpielfilm & TVDigital (A.M.1)")
xcorr212.2 <- Ccf(resid41.2, resid42.2,
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "TVMovie & TVSpielfilm (A.M.2)")
xcorr213.2 <- Ccf(resid41.2, resid43.2, 
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "TVMovie & TVDigital (A.M.2)")
xcorr223.2 <- Ccf(resid42.2, resid43.2, 
                  lag.max = 6,
                  xlim = c(-6,6),
                  ylim=c(-1,1),
                  main = "TVSpielfilm & TVDigital (A.M.2)")


# Latex Table
xcorr2.1 <- matrix(c(xcorr212.1$acf,xcorr213.1$acf,xcorr223.1$acf), ncol = 3)
colnames(xcorr2.1) <- c("TVMovie + Spielfilm", "TVMovie + TVDigital","TVSpielfilm + TVDigital")
stargazer(xcorr2.1, summary = FALSE)

xcorr2.2 <- matrix(c(xcorr212.2$lag, xcorr212.2$acf,xcorr213.2$acf,xcorr223.2$acf), ncol = 4)
colnames(xcorr2.2) <- c("Lags", "TVMovie + Spielfilm", "TVMovie + TVDigital","TVSpielfilm + TVDigital")
stargazer(xcorr2.2, summary = FALSE)


#############
# Residual AC
#############

# --- Reader Market
par(mfrow=c(3,2))

Acf(resid31.1, 18, main = "TVMovie (R.M.1)")
Acf(resid31.2, 18, main = "TVMovie (R.M.2)")

Acf(resid32.1, 18, main = "TVSpielfilm (R.M.1)")
Acf(resid32.2, 18, main = "TVSpielfilm (R.M.2)")

Acf(resid33.1, 18, main = "TVDigital (R.M.1)")
Acf(resid33.2, 18, main = "TVDigital (R.M.2)")


# ------- Ad Market
Acf(resid41.1, 18, main = "TVMovie (A.M.1)")
Acf(resid41.2, 18, main = "TVMovie (A.M.2)")

Acf(resid42.1, 18, main = "TVSpielfilm (A.M.1)")
Acf(resid42.2, 18, main = "TVSpielfilm (A.M.2)")

Acf(resid43.1, 18, main = "TVDigital (A.M.1)")
Acf(resid43.2, 18, main = "TVDigital (A.M.2)")


###################
# Granger Causality
###################

# 1 --------------------
write.csv(cbind(resid31.1, resid32.1, resid33.1, resid41.1, resid42.1, resid43.1), file="granger_tv1.csv")

VARselect(cbind(resid31.1, resid32.1, resid33.1), lag.max = 12, type = "none")
VARselect(cbind(resid41.1, resid42.1, resid43.1), lag.max = 12, type = "none")

var1.1 <- VAR(cbind(resid31.1, resid32.1, resid33.1))
var2.1 <- VAR(cbind(resid41.1, resid42.1, resid43.1))

# 2 --------------------
write.csv(cbind(resid31.2, resid32.2, resid33.2, resid41.2, resid42.2, resid43.2), file="granger_tv2.csv")

VARselect(cbind(resid31.2, resid32.2, resid33.2), lag.max = 12, type = "none")
VARselect(cbind(resid41.2, resid42.2, resid43.2), lag.max = 12, type = "none")

var1.2 <- VAR(cbind(resid31.2, resid32.2, resid33.2))
var2.2 <- VAR(cbind(resid41.2, resid42.2, resid43.2))


# Impulse response function
# Reader Market
plot(irf(var.1.1))
plot(irf(var.1.2))

# Ad Market
plot(irf(var.2.1))
plot(irf(var.2.2))



