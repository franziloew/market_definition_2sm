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


# Market: Digital News Magazines 
# Included Magazines: FocusOnOn, DerSPON, stern.de.de
# Time Interval: 2009m1 - 2016m10

fss_online <- read.csv("/Users/Franzi/Desktop/R/2sm/fss_online.csv", header = TRUE, stringsAsFactors = FALSE)

# Assign Globals
# Sub1
y11 <- fss_online$visitsFocusOnonline

# Sub2
y12 <- fss_online$visitsspiegelonline

# Sub3
y13 <- fss_online$visitsstern.dede

# Sub4
y14 <- fss_online$visitsdiewelt
y15 <- fss_online$visitszeitonline

# Fill NA
y11[which(is.na(y11))] <- mean(y11, na.rm = TRUE)
y12[which(is.na(y12))] <- mean(y12, na.rm = TRUE)
y13[which(is.na(y13))] <- mean(y13, na.rm = TRUE)
y14[which(is.na(y13))] <- mean(y14, na.rm = TRUE)
y15[which(is.na(y13))] <- mean(y15, na.rm = TRUE)

# Assign Times- Series Globals
# Sub1
y11.ts <- ts(y11, start=c(2009,1), end = c(2016,10), frequency = 12)
y21.ts <- window(y11.ts, start=c(2009,2), end = c(2015,2), frequency = 12)

# Sub2
y12.ts <- ts(y12,  start=c(2009,1), end = c(2016,10), frequency = 12)
y22.ts <- window(y12.ts,  start=c(2009,2), end = c(2015,2), frequency = 12)

# Sub3
y13.ts <- ts(y13,  start=c(2009,1), end = c(2016,10), frequency = 12)
y23.ts <- ts(y13.ts,  start=c(2009,2), end = c(2015,2), frequency = 12)

# Sub3
y14.ts <- ts(y14,  start=c(2009,1), end = c(2016,10), frequency = 12)
y24.ts <- ts(y14.ts,  start=c(2009,2), end = c(2015,2), frequency = 12)
y15.ts <- ts(y15,  start=c(2009,1), end = c(2015,10), frequency = 12)
y25.ts <- ts(y15.ts,  start=c(2009,2), end = c(2015,2), frequency = 12)

# Summary and plotting
combined.y1 <- data.frame(y11.ts, y12.ts, y13.ts)
colnames(combined.y1) <- c("FocusOnOn", "SPON", "stern.de.de")
stargazer(combined.y1)

# Reader Market
#tikz("/Users/Franzi/Desktop/R/circ_tv1.tex",width=5,height=3.5)
par(mfrow=c(1,2))
plot.ts((y11.ts)/1000,
        type = "l",
        col = "red",     
        ylab = "Visits",
        xlab = "Monthly Data",
        ylim = c(0,210000))
lines((y12.ts)/1000, 
      type = "l",
      col = "blue")
lines((y13.ts)/1000, 
      type = "l",
      col = "green")
lines((y14.ts)/1000, 
      type = "l",
      col = "yellow")
lines((y15.ts)/1000, 
      type = "l",
      col = "pink")
legend("topleft",
       c("FocusOnOn", "SPON", "stern.de.de", "welt.de", "zeitOnline"),
       lty = c(1,1,1),
       lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green", "yellow", "pink"),
       bty = "n",
       horiz = FALSE)
#dev.off()

plot.ts((y21.ts)/1000,
        type = "l",
        col = "red",     
        ylab = "Visits",
        xlab = "Monthly Data",
        ylim = c(0,210000))
lines((y22.ts)/1000, 
      type = "l",
      col = "blue")
lines((y23.ts)/1000, 
      type = "l",
      col = "green")
lines((y24.ts)/1000, 
      type = "l",
      col = "yellow")
lines((y25.ts)/1000, 
      type = "l",
      col = "pink")
legend("topleft",
       c("FocusOnOn", "SPON", "stern.de.de", "welt.de", "zeitOnline"),
       lty = c(1,1,1),
       lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green", "yellow", "pink"),
       bty = "n",
       horiz = FALSE)

# ACF and PACF
# Reader Market
#tikz("/Users/Franzi/Desktop/R/acf_circ_tv1.tex",width=5,height=3.5)
par(mfrow=c(3,2))         

Acf(y11.ts, 20, main = "FocusOnOn")
Pacf(y11.ts, 20, main = "FocusOnOn")

Acf(y12.ts, 20, main = "SPON")
Pacf(y12.ts, 20, main = "SPON")

Acf(y13.ts, 20, main = "stern.de.de")
Pacf(y13.ts, 20, main = "stern.de.de")

#dev.off()

par(mfrow=c(3,2))         

Acf(y21.ts, 20, main = "FocusOnOn")
Pacf(y21.ts, 20, main = "FocusOnOn")

Acf(y22.ts, 20, main = "SPON")
Pacf(y22.ts, 20, main = "SPON")

Acf(y23.ts, 20, main = "stern.de.de")
Pacf(y23.ts, 20, main = "stern.de.de")

# Phillips \& Perron Unit Root Test
# Null-Hypothesis: Series is non-stationary. If the test statistic is bigger than the critical value, we cannot reject the Null and Series is non-stationary. 
# Reader Market
y11.p <- (ur.pp(y11.ts, type = "Z-tau", model = "trend", lags = "short"))
y11.pp <- y11.p@teststat
y11.p@cval
plot(y11.p)

y12.p <- (ur.pp(y12.ts, type = "Z-tau", model = "trend", lags = "short"))
y12.pp <- y12.p@teststat
y12.p@cval
plot(y12.p)

y13.p <- (ur.pp(y13.ts, type = "Z-tau", model = "trend", lags = "short"))
y13.pp <- y13.p@teststat
y13.p@cval
plot(y13.p)


# Latex Table Unit Root Test
# c.y1p <- data.frame(y11.pp, y12.pp, y13.pp)
# c.y1p[nrow(c.y1p)+1,]<-c(y21.pp,y22.pp,y23.pp)
# c.y1p[nrow(c.y1p)+1,]<-c("1pct","5pct","10pct")
# c.y1p[nrow(c.y1p)+1,]<-y13.p@cval
# colnames(c.y1p) <- c("FocusOnOn", "SPON", "stern.de.de")
# rownames(c.y1p) <- c("Sales", "Ad pages","Sig. Level", "Critical Values")
# 
# stargazer(c.y1p, summary = FALSE, digits = 1)

#######  2009-2015
# FocusOnOn
combined.y11 <- matrix(c(y12, y13), ncol = 2, nrow = 94)
colnames(combined.y11) <- c("SPON", "stern.de.de")
combined.y11.ts <- ts(combined.y11, start=c(2009,1), end = c(2016,10), frequency = 12)

combined.y112 <- matrix(c(y12, y13, y14, y15), ncol = 4, nrow = 94)
colnames(combined.y112) <- c("SPON", "stern.de.de", "welt.de", "zeitOnline")
combined.y112.ts <- ts(combined.y112, start=c(2009,1), end = c(2016,10), frequency = 12)

Y11 <- auto.arima(y11.ts, xreg = combined.y11.ts)
Y11
#Y11 <- arima(y11.ts, order= c(0,1,0), seasonal = list(order=c(0L,1L,0L)), xreg = combined.y11.ts)
resid11 <- Y11$residuals

Y112 <- auto.arima(y11.ts, xreg = combined.y112.ts)
Y112
#Y11 <- arima(y11.ts, order= c(0,1,0), seasonal = list(order=c(0L,1L,0L)), xreg = combined.y11.ts)
resid112 <- Y112$residuals


# SPON
combined.y12 <- matrix(c(y11, y13), ncol = 2, nrow = 94)
colnames(combined.y12) <- c("FocusOnOn", "stern.de.de")
combined.y12.ts <- ts(combined.y12, start=c(2009,1), end = c(2016,10), frequency = 12)
combined.y122 <- matrix(c(y11, y13, y14, y15), ncol = 4, nrow = 94)
colnames(combined.y122) <- c("FocusOnOn", "stern.de.de", "welt.de", "zeitOnline")
combined.y122.ts <- ts(combined.y122, start=c(2009,1), end = c(2016,10), frequency = 12)

Y12 <- auto.arima(y12.ts, xreg = combined.y12.ts)
#Y12 <- arima(y12.ts, order= c(0,0,1), seasonal = list(order=c(0L,1L,0L)), xreg = combined.y12.ts)
resid12 <- Y12$residuals

Y122 <- auto.arima(y12.ts, xreg = combined.y122.ts)
#Y12 <- arima(y12.ts, order= c(0,0,1), seasonal = list(order=c(0L,1L,0L)), xreg = combined.y12.ts)
resid122 <- Y122$residuals

# stern.de.de
combined.y13 <- matrix(c(y11, y12), ncol = 2, nrow = 94)
colnames(combined.y13) <- c("FocusOnOn", "SPON")
combined.y13.ts <- ts(combined.y13, start=c(2009,1), end = c(2016,10), frequency = 12)
combined.y132 <- matrix(c(y11, y12, y14, y15), ncol = 4, nrow = 94)
colnames(combined.y132) <- c("FocusOnOn", "SPON", "welt.de", "zeitOnline")
combined.y132.ts <- ts(combined.y132, start=c(2009,1), end = c(2016,10), frequency = 12)

Y13 <- auto.arima(y13.ts, xreg = combined.y13.ts)
#Y13 <- arima(y13.ts, c(0,1,0), xreg = combined.y13.ts)
resid13 <- Y13$residuals

Y132 <- auto.arima(y13.ts, xreg = combined.y132.ts)
#Y13 <- arima(y13.ts, c(0,1,0), xreg = combined.y13.ts)
resid132 <- Y132$residuals


#######  2009-2015
# FocusOnOn
combined.y21.ts <- window(combined.y11.ts, start=c(2009,2), end = c(2015,2), frequency = 12)
combined.y212.ts <- window(combined.y112.ts, start=c(2009,2), end = c(2015,2), frequency = 12)

Y21 <- auto.arima(y21.ts, xreg = combined.y21.ts)
Y21
#Y11 <- arima(y11.ts, order= c(0,1,0), seasonal = list(order=c(0L,1L,0L)), xreg = combined.y11.ts)
resid21 <- Y21$residuals

Y212 <- auto.arima(y21.ts, xreg = combined.y212.ts)
Y212
#Y11 <- arima(y11.ts, order= c(0,1,0), seasonal = list(order=c(0L,1L,0L)), xreg = combined.y11.ts)
resid212 <- Y212$residuals


# SPON
combined.y22.ts <- window(combined.y12.ts, start=c(2009,2), end = c(2015,2), frequency = 12)
combined.y222.ts <- window(combined.y122.ts, start=c(2009,2), end = c(2015,2), frequency = 12)

Y22 <- auto.arima(y22.ts, xreg = combined.y22.ts)
resid22 <- Y22$residuals

Y222 <- auto.arima(y22.ts, xreg = combined.y222.ts)
resid222 <- Y222$residuals

# stern.de.de
combined.y23.ts <- window(combined.y13.ts, start=c(2009,2), end = c(2015,2), frequency = 12)
combined.y232.ts <- window(combined.y132.ts, start=c(2009,2), end = c(2015,2), frequency = 12)

Y23 <- auto.arima(y23.ts, xreg = combined.y23.ts)
resid23 <- Y23$residuals

Y232 <- auto.arima(y23.ts, xreg = combined.y232.ts)
resid232 <- Y232$residuals


## Latex Table
# stargazer(Y11, Y12, Y13, title = "Regression Results: Reader Market 2005-2007", align = TRUE, dep.var.labels = c("FocusOnOn", "SPON", "stern.de.de"), omit.stat = c("LL"), no.space = TRUE)

## Plotting residuals
#tikz("/Users/Franzi/Desktop/R/arima_circ_tv1.tex",width=5,height=3.5)
par(mfrow=c(1,2))
plot.ts((resid11)/1000, 
        type = "l",
        col = "red",
        xlab = "2009-2016",
        ylab = "Residuals",
        ylim = c(-20000,20000))

lines((resid12)/1000, 
      type = "l",
      col = "blue")

lines((resid13)/1000, 
      type = "l",
      col = "green")
legend("topleft",
       c("FocusOnOn", "SPON", "stern.de.de"),
       lty = c(1,1,1),
       lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)
abline(a = 0, b = 0)
# ---------------------
plot.ts((resid112)/1000, 
        type = "l",
        col = "red",
        xlab = "2009-2016 + add. Subs",
        ylab = "Residuals",
        ylim = c(-20000,20000))

lines((resid122)/1000, 
      type = "l",
      col = "blue")

lines((resid132)/1000, 
      type = "l",
      col = "green")
legend("topleft",
       c("FocusOnOn", "SPON", "stern.de.de"),
       lty = c(1,1,1),
       lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)
abline(a = 0, b = 0)

# Smaller Sample
par(mfrow=c(1,1))
plot.ts((resid21)/1000, 
        type = "l",
        col = "red",
        xlab = "2009-2013",
        ylab = "Residuals",
        ylim = c(-20000,20000))

lines((resid22)/1000, 
      type = "l",
      col = "blue")

lines((resid23)/1000, 
      type = "l",
      col = "green")
legend("topleft",
       c("FocusOnOn", "SPON", "stern.de.de"),
       lty = c(1,1,1),
       lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)
abline(a = 0, b = 0)
#------------------
plot.ts((resid212)/1000, 
        type = "l",
        col = "red",
        xlab = "2009-2013 + add. Subs",
        ylab = "Residuals",
        ylim = c(-20000,20000))

lines((resid222)/1000, 
      type = "l",
      col = "blue")

lines((resid232)/1000, 
      type = "l",
      col = "green")
legend("topleft",
       c("FocusOnOn", "SPON", "stern.de.de"),
       lty = c(1,1,1),
       lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)
abline(a = 0, b = 0)
#dev.off()

# Residual Autocorrelation
#tikz("/Users/Franzi/Desktop/R/resid_acf_circ_tv1.tex",width=5,height=3.5)
par(mfrow=c(3,2))

# 
Acf(resid21, 20)
Pacf(resid21, 20)

Acf(resid22, 20)
Pacf(resid22, 20)

Acf(resid23, 20)
Pacf(resid23, 20)
# --------------
Acf(resid212, 20)
Pacf(resid212, 20)

Acf(resid222, 20)
Pacf(resid222, 20)

Acf(resid232, 20)
Pacf(resid232, 20)


# CrossCorrelation
# 2009-2016
par(mfrow=c(2,3))
xcorr112 <- Ccf(resid11, resid12,
                lag.max = 12,
                xlim = c(-6,6),
                ylim=c(-1,1),
                main = "FocusOnOn & SPON")
xcorr113 <- Ccf(resid11, resid13, 
                lag.max = 12, 
                xlim = c(-6,6),
                ylim=c(-1,1),
                main = "FocusOnOn & stern.de.de")
xcorr123 <- Ccf(resid122, resid132, 
                lag.max = 12, 
                xlim = c(-6,6),
                ylim=c(-1,1),
                main = "SPON & stern.de.de")
xcorr1122 <- Ccf(resid112, resid122,
                lag.max = 12,
                xlim = c(-6,6),
                ylim=c(-1,1),
                main = "FocusOnOn & SPON (2)")
xcorr1132 <- Ccf(resid112, resid132, 
                lag.max = 12, 
                xlim = c(-6,6),
                ylim=c(-1,1),
                main = "FocusOnOn & stern.de.de (2)")
xcorr1232 <- Ccf(resid122, resid132, 
                lag.max = 12, 
                xlim = c(-6,6),
                ylim=c(-1,1),
                main = "SPON & stern.de.de (2)")

#-------- 2009-2013

xcorr212 <- Ccf(resid21, resid22,
                lag.max = 12,
                xlim = c(-6,6),
                ylim=c(-1,1),
                main = "FocusOnOn & SPON")
xcorr213 <- Ccf(resid21, resid23, 
                lag.max = 12, 
                xlim = c(-6,6),
                ylim=c(-1,1),
                main = "FocusOnOn & stern.de.de")
xcorr223 <- Ccf(resid22, resid23, 
                lag.max = 12, 
                xlim = c(-6,6),
                ylim=c(-1,1),
                main = "SPON & stern.de.de")
xcorr2122 <- Ccf(resid212, resid222,
                lag.max = 12,
                xlim = c(-6,6),
                ylim=c(-1,1),
                main = "FocusOnOn & SPON (2)")
xcorr2132 <- Ccf(resid212, resid232, 
                lag.max = 12, 
                xlim = c(-6,6),
                ylim=c(-1,1),
                main = "FocusOnOn & stern.de.de (2)")
xcorr2232 <- Ccf(resid222, resid232, 
                lag.max = 12, 
                xlim = c(-6,6),
                ylim=c(-1,1),
                main = "SPON & stern.de.de (2)")
xcorr <- matrix(c(xcorr112$lag, xcorr112$acf,xcorr113$acf,xcorr123$acf, xcorr212$acf,xcorr213$acf,xcorr223$acf), ncol = 7)
colnames(xcorr) <- c("Lags", "FocusOnOnline SPIEGELOnline (1)", "FocusOnOnline stern.de.de (1)" ,"SPIEGELOnline stern.de.de (1)", "FocusOnOnline SPIEGELOnline (2)", "FocusOnOnline stern.de.de (2)" ,"SPIEGELOnline stern.de.de (2)")
stargazer(xcorr, summary = FALSE)

# Granger Causality
# 2009 - 2016
granger.112.1 <- grangertest(resid11~resid12, order=1)
granger.112.2 <- grangertest(resid12~resid11, order=1)
granger.113.1 <- grangertest(resid11~resid13, order=1)
granger.113.2 <- grangertest(resid13~resid11, order=1)
granger.123.1 <- grangertest(resid12~resid13, order=1)
granger.123.2 <- grangertest(resid13~resid12, order=1)

df.res <- data.frame(`F-statistics` = c(granger.112.1$F, granger.112.2$F, granger.113.1$F, granger.113.2$F, granger.123.1$F, granger.123.2$F),
                     `p value` = c(granger.112.1$`Pr(>F)`, granger.112.2$`Pr(>F)`, granger.113.1$`Pr(>F)`, granger.113.2$`Pr(>F)`, granger.123.1$`Pr(>F)`, granger.123.2$`Pr(>F)`))
df.res <- na.omit(df.res)
row.names(df.res) = c("FocusOnOn -> SPON", "SPON -> FocusOnOn", "FocusOnOn -> stern.de.de", "stern.de.de -> FocusOnOn", "SPON -> stern.de.de", "stern.de.de -> SPONl")

stargazer(df.res, summary = FALSE)

# Ad Market
granger.212.1 <- grangertest(resid21~resid22, order=1)
granger.212.2 <- grangertest(resid22~resid21, order=1)
granger.213.1 <- grangertest(resid21~resid23, order=1)
granger.213.2 <- grangertest(resid23~resid21, order=1)
granger.223.1 <- grangertest(resid22~resid23, order=1)
granger.223.2 <- grangertest(resid23~resid22, order=1)
df.res <- data.frame(`F-statistics` = c(granger.212.1$F, granger.212.2$F, granger.213.1$F, granger.213.2$F, granger.223.1$F, granger.223.2$F),
                     `p value` = c(granger.212.1$`Pr(>F)`, granger.212.2$`Pr(>F)`, granger.213.1$`Pr(>F)`, granger.213.2$`Pr(>F)`, granger.223.1$`Pr(>F)`, granger.223.2$`Pr(>F)`))
df.res <- na.omit(df.res)
row.names(df.res) = c("FocusOn -> SPON", "SPON -> FocusOn", "FocusOn -> stern.de", "stern.de -> FocusOn", "SPON -> stern.de", "stern.de -> SPON")

stargazer(df.res, summary = FALSE)



# Impulse response function
# Reader Market
combined.resid112 <- matrix(c(resid11, resid12), ncol = 2, nrow = 37)
colnames(combined.resid112) <- c("FocusOnOn", "SPON")
combined.resid112.ts <- ts(combined.resid112, start=c(2009,2), end = c(2012,2), frequency = 12)

combined.resid113 <- matrix(c(resid11, resid13), ncol = 2, nrow = 37)
colnames(combined.resid113) <- c("FocusOnOn", "stern.de")
combined.resid113.ts <- ts(combined.resid113, start=c(2009,2), end = c(2012,2), frequency = 12)

combined.resid123 <- matrix(c(resid12, resid13), ncol = 2, nrow = 37)
colnames(combined.resid123) <- c("SPON", "stern.de")
combined.resid123.ts <- ts(combined.resid123, start=c(2009,2), end = c(2012,2), frequency = 12)


var.112 <- VAR(combined.resid112.ts, type = "none", ic ="AIC")
irf112 <- irf(var.112)
#plot(irf112, plot.type = "multiple")

var.113 <- VAR(combined.resid113.ts, type = "none", ic ="AIC")
irf113 <- irf(var.113)
#plot(irf113, plot.type = "multiple")

var.123 <- VAR(combined.resid123.ts, type = "none", ic ="AIC")
irf123 <- irf(var.123)
#plot(irf123, plot.type = "multiple")

