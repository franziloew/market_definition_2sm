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


# Market 7: Digital News Magazines 
# Included Magazines: Focus Online, DerSpiegel Online, Stern.de
# Time Interval: 2012m2 - 2015m2

fss_online <- read.csv("/Users/Franzi/Desktop/R/2sm/fss_online2.csv", header = TRUE, stringsAsFactors = FALSE)

# date <- as.yearmon(tv$month,"%Ym%m")
attach(fss_online)

# Assign Globals
# Assign Globals
# Sub1
y11 <- visitsfocusonline

# Sub2
y12 <- visitsspiegelonline

# Sub3
y13 <- visitsstern

# Fill NA
y11[which(is.na(y11))] <- mean(y11, na.rm = TRUE)
y12[which(is.na(y12))] <- mean(y12, na.rm = TRUE)
y13[which(is.na(y13))] <- mean(y13, na.rm = TRUE)


# Assign Times- Series Globals
# Sub1
y11.ts <- ts(y11, start=c(2012,2), end = c(2015,2), frequency = 12)

# Sub2
y12.ts <- ts(y12,  start=c(2012,2), end = c(2015,2), frequency = 12)

# Sub3
y13.ts <- ts(y13,  start=c(2012,2), end = c(2015,2), frequency = 12)

# Summary and plotting
combined.y1 <- data.frame(y11, y12, y13)
colnames(combined.y1) <- c("FOCUS Online", "SPIEGEL Online", "stern.de")
stargazer(combined.y1)



# Reader Market
#tikz("/Users/Franzi/Desktop/R/circ_tv1.tex",width=5,height=3.5)
par(mfrow=c(1,1))
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
legend("topleft",
       c("FOCUS Online", "SPON", "stern.de"),
       lty = c(1,1,1),
       lwd = c(2.5,2.5,2.5),
       col = c("red", "blue", "green"),
       bty = "n",
       horiz = FALSE)
dev.off()


# ACF and PACF
# Reader Market
#tikz("/Users/Franzi/Desktop/R/acf_circ_tv1.tex",width=5,height=3.5)
par(mfrow=c(3,2))         

Acf(y11.ts, 20, main = "FOCUS Online")
Pacf(y11.ts, 20, main = "FOCUS Online")

Acf(y12.ts, 20, main = "SPIEGEL Online")
Pacf(y12.ts, 20, main = "SPIEGEL Online")

Acf(y13.ts, 20, main = "stern.de")
Pacf(y13.ts, 20, main = "stern.de")

#dev.off()


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
# colnames(c.y1p) <- c("FOCUS Online", "SPIEGEL Online", "stern.de")
# rownames(c.y1p) <- c("Sales", "Ad pages","Sig. Level", "Critical Values")
# 
# stargazer(c.y1p, summary = FALSE, digits = 1)

# ARIMA(1,0,0) or AR(1)
####### Reader Markt
# FOCUS Online
combined.y11 <- matrix(c(y12, y13), ncol = 2, nrow = 37)
colnames(combined.y11) <- c("SPON", "stern.de")
combined.y11.ts <- ts(combined.y11, start=c(2012,2), end = c(2015,2), frequency = 12)

Y11 <- auto.arima(y11.ts, xreg = combined.y11.ts)
Y11
Y11 <- arima(y11.ts, order= c(0,1,1), xreg = combined.y11.ts)
coeftest(Y11)
resid11 <- Y11$residuals
#seasonal = list(order=c(0L,1L,0L))
# SPIEGEL Online
combined.y12 <- matrix(c(y11, y13), ncol = 2, nrow = 37)
colnames(combined.y12) <- c("FOCUS Online", "stern.de")
combined.y12.ts <- ts(combined.y12, start=c(2012,2), end = c(2015,2), frequency = 12)

Y12 <- auto.arima(y12.ts, xreg = combined.y12.ts)
Y12
Y12 <- arima(y12.ts, order= c(1,1,0), xreg = combined.y12.ts)
coeftest(Y12)
resid12 <- Y12$residuals

# stern.de
combined.y13 <- matrix(c(y11, y12), ncol = 2, nrow = 37)
colnames(combined.y13) <- c("FOCUS Online", "SPON")
combined.y13.ts <- ts(combined.y13, start=c(2012,2), end = c(2015,2), frequency = 12)

Y13 <- auto.arima(y13.ts, xreg = combined.y13.ts)
Y13
Y13 <- arima(y13.ts, c(1,1,0), xreg = combined.y13.ts)
coeftest(Y13)
resid13 <- Y13$residuals
## Latex Table

stargazer(Y11, Y12, Y13, title = "Regression Results: Reader Market 2005-2007", align = TRUE, dep.var.labels = c("FOCUS Online", "SPIEGEL Online", "stern.de"), omit.stat = c("LL"), no.space = TRUE)

## Plotting residuals
#tikz("/Users/Franzi/Desktop/R/arima_circ_tv1.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts((resid11)/1000, 
        type = "l",
        col = "red",
        xlab = "Monthly Data",
        ylab = "Residuals",
        ylim = c(-20000,20000))

lines((resid12)/1000, 
      type = "l",
      col = "blue")

lines((resid13)/1000, 
      type = "l",
      col = "green")
legend("topleft",
       c("FOCUS Online", "SPON", "stern.de"),
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

# Reader Market
Acf(resid11, 20)
Pacf(resid11, 20)

Acf(resid12, 20)
Pacf(resid12, 20)

Acf(resid13, 20)
Pacf(resid13, 20)


# CrossCorrelation
# Reader Market
par(mfrow=c(1,3))
xcorr112 <- Ccf(resid11, resid12,
                lag.max = 12,
                xlim = c(-12,12),
                ylim=c(-1,1),
                main = "FOCUS Online & SPIEGEL Online")
xcorr113 <- Ccf(resid11, resid13, 
                lag.max = 12, 
                xlim = c(-12,12),
                ylim=c(-1,1),
                main = "FOCUS Online & stern.de")
xcorr123 <- Ccf(resid12, resid13, 
                lag.max = 12, 
                xlim = c(-12,12),
                ylim=c(-1,1),
                main = "SPIEGEL Online & stern.de")
xcorr1 <- matrix(c(xcorr112$lag[7:19,,], xcorr112$acf[7:19,,],xcorr113$acf[7:19,,],xcorr123$acf[7:19,,]), ncol = 4)
colnames(xcorr1) <- c("Lags", "FOCUS Online & SPIEGEL Online", "FOCUS Online & stern.de" ,"SPIEGEL Online & stern.de")
stargazer(xcorr1, summary = FALSE)


# Impulse response function
# Reader Market
combined.resid112 <- matrix(c(resid11, resid12), ncol = 2, nrow = 37)
colnames(combined.resid112) <- c("FOCUS Online", "SPIEGEL Online")
combined.resid112.ts <- ts(combined.resid112, start=c(2009,2), end = c(2012,2), frequency = 12)

combined.resid113 <- matrix(c(resid11, resid13), ncol = 2, nrow = 37)
colnames(combined.resid113) <- c("FOCUS Online", "stern.de")
combined.resid113.ts <- ts(combined.resid113, start=c(2009,2), end = c(2012,2), frequency = 12)

combined.resid123 <- matrix(c(resid12, resid13), ncol = 2, nrow = 37)
colnames(combined.resid123) <- c("SPIEGEL Online", "stern.de")
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

# Granger Causality
granger.112.1 <- causality(var.112, cause = "FOCUS.Online")
granger.112.2 <- causality(var.112, cause = "SPIEGEL.Online")
granger.113.1 <- causality(var.113, cause = "FOCUS.Online")
granger.113.2 <- causality(var.113, cause = "stern.de")
granger.123.1 <- causality(var.123, cause = "SPIEGEL.Online")
granger.123.2 <- causality(var.123, cause = "stern.de")

df.res <- data.frame(`F statistics` = c(granger.112.1$Granger$statistic, granger.112.2$Granger$statistic, granger.113.1$Granger$statistic, granger.113.2$Granger$statistic, granger.123.1$Granger$statistic, granger.123.2$Granger$statistic),
                     `p value` = c(granger.112.1$Granger$p.value, granger.112.2$Granger$p.value, granger.113.1$Granger$p.value, granger.113.2$Granger$p.value, granger.123.1$Granger$p.value, granger.123.2$Granger$p.value),
                     row.names = c("FOCUS Online -> SPIEGEL Online", "SPIEGEL Online -> FOCUS Online", "FOCUS Online -> stern.de", "stern.de -> FOCUS Online", "SPIEGEL Online -> stern.de", "stern.de -> SPIEGEL Online"))
stargazer(df.res, summary = FALSE)
# Ad Market
