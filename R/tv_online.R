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


# Market: Digital TV Magazines 
# Included Magazines: FocusOnOn, DerSPON, stern.de.de
# Time Interval: 2009m1 - 2016m10

tv_online <- read.csv("/Users/Franzi/Desktop/R/2sm/tv_online.csv", header = TRUE, stringsAsFactors = FALSE)

tv_online <- ts(tv_online, start=c(2009,1), end = c(2016,12), frequency = 12)

# Assign Globals
# Sub1
y11 <- tv_online[,"visitstvdigital"]

# Sub2
y12 <- tv_online[,"visitstvdirekt"]

# Sub3
y13 <- tv_online[,"visitstvspielfilm"]

# -------- Select Sub-Sample
#  -1- 2009/Sep - 2014/Sep ---
# Sub1
y11.1 <- window(y11, start=c(2009,09), end = c(2014,09), frequency = 12)
# Sub2
y13.1 <- window(y13, start=c(2009,09), end = c(2014,09), frequency = 12)

# -------- Select Sub-Sample
#  -2- 2009/Sep - 2014/Sep ---
# Sub1
y11.2 <- window(y11, start=c(2015,06), end = c(2016,12), frequency = 12)
# Sub2
y12.2 <- window(y12, start=c(2015,06), end = c(2016,12), frequency = 12)
# Sub3
y13.2 <- window(y13, start=c(2015,06), end = c(2016,12), frequency = 12)

# Summary and plotting
combined.y1 <- data.frame(y11, y12, y13)
colnames(combined.y1) <- c("TV Digital", "TV Direkt", "TV Spielfilm")
stargazer(combined.y1)

# Reader Market
par(mfrow=c(2,1))
plot.ts((y11)/1000, 
        type = "l", col = "red", ylab = "Visits", xlab = "Monthly Data", ylim =c(200,1200))
lines((y12)/1000, type = "l", col = "blue")
legend("topleft",
       c("TV Digital", "TV Direkt"),
       lty = c(1,1),
       lwd = c(2.5,2.5),
       col = c("red", "blue"),
       bty = "n",
       horiz = FALSE)
plot.ts((y13)/1000, type = "l", col = "green")
legend("topleft",
       c("TV Spielfilm"),
       lty = c(1),
       lwd = c(2.5),
       col = c("green"),
       bty = "n",
       horiz = FALSE)
plot.ts(y12)


### Prewhiten
#############
# Subsample 1
Y11 <- auto.arima(y11.1, xreg = y13.1)
Y11
resid11.1 <- Y11$residuals

Y12 <- auto.arima(y13.1, xreg = y11.1)
Y12
resid13.1 <- Y12$residuals

# Subsample 2
Y11.2 <- auto.arima(y11.2, xreg = cbind(y12.2, y13.2))
Y11.2
resid11.2 <- Y11.2$residuals

Y12.2 <- auto.arima(y12.2, xreg = cbind(y11.2, y13.2))
Y12.2
resid12.2 <- Y12.2$residuals

Y13.2 <- auto.arima(y13.2, xreg = cbind(y11.2, y12.2))
Y13.2
resid13.2 <- Y13.2$residuals

## Plotting residuals
par(mfrow=c(1,1))
plot.ts((resid13.1)/1000, 
        type = "l",
        col = "red",
        xlab = "2009-2016",
        ylab = "Residuals")

lines((resid11.1)/1000, 
      type = "l",
      col = "blue")

legend("topleft",
       c("TV Spielfilm", "Tv Digital"),
       lty = c(1,1,1),
       lwd = c(2.5,2.5,2.5),
       col = c("red", "blue"),
       bty = "n",
       horiz = FALSE)
abline(a = 0, b = 0)
# ---------------------

par(mfrow=c(1,1))
plot.ts((resid13.2)/1000, 
        type = "l",
        col = "red",
        ylab = "Residuals")

lines((resid12.2)/1000, 
      type = "l",
      col = "blue")

lines((resid11.2)/1000, 
      type = "l",
      col = "green")

legend("topleft",
       c("TV Spielfilm", "TV Direkt", "TV Digital"),
       lty = c(1,1,1),
       lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)
abline(a = 0, b = 0)

# CrossCorrelation
# 2009-2016
par(mfrow=c(1,1))

Ccf(resid11.1, resid13.1,
                lag.max = 12, xlim = c(-12,12), ylim=c(-1,1), main = "TV Digital&TV Spielfilm (1)")

#-------- 2009-2013
par(mfrow=c(1,3))

Ccf(resid11.2, resid12.2,
    lag.max = 12, xlim = c(-12,12), ylim = c(-1,1), main = "TV Digital&TV Direkt")
Ccf(resid11.2, resid13.2,
    lag.max = 12, xlim = c(-12,12), ylim = c(-1,1), main = "TV Digital&TV Spielfilm")
Ccf(resid12.2, resid13.2,
    lag.max = 12, xlim = c(-12,12), ylim = c(-1,1), main = "TV Direkt&TV Spielfilm")


