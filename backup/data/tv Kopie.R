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

rm(list = ls())

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
# Sub1
y31.2 <- window(y31,  start=c(2011,20), end = c(2014,20), frequency = 26)
x41.2 <- window(x41,  start=c(2011,20), end = c(2014,20), frequency = 26)
y41.2 <- window(y41,  start=c(2011,20), end = c(2014,20), frequency = 26)

# Sub2
y32.2 <- window(y32,  start=c(2011,20), end = c(2014,20), frequency = 26)
x42.2 <- window(x42,  start=c(2011,20), end = c(2014,20), frequency = 26)
y42.2 <- window(y42,  start=c(2011,20), end = c(2014,20), frequency = 26)

# Sub3
y33.2 <- window(y33,  start=c(2011,20), end = c(2014,20), frequency = 26)
x43.2 <- window(x43,  start=c(2011,20), end = c(2014,20), frequency = 26)
y43.2 <- window(y43,  start=c(2011,20), end = c(2014,20), frequency = 26)


# Fill NA
y31.2[which(is.na(y31.2))] <- mean(y31.2, na.rm = TRUE)
y32.2[which(is.na(y32.2))] <- mean(y32.2, na.rm = TRUE)
y33.2[which(is.na(y33.2))] <- mean(y33.2, na.rm = TRUE)

# 
y41.2[which(is.na(y41.2))] <- mean(y41.2, na.rm = TRUE)
y42.2[which(is.na(y42.2))] <- mean(y42.2, na.rm = TRUE)
y43.2[which(is.na(y43.2))] <- mean(y43.2, na.rm = TRUE)



######################
# Summary and plotting
######################

# ----- Reader Market
combined.y1 <- data.frame(y31.2, y32.2, y33.2)
colnames(combined.y1) <- c("TVMovie", "TVSpielfilm", "TVDigital")

stargazer(combined.y1, digits = 1, title = "Summary Statistic: sales")

# ----- Ad Market
combined.y2 <- data.frame(y41.2, y42.2, y43.2)
colnames(combined.y2) <- c("TVMovie", "TVSpielfilm", "TVDigital")

stargazer(combined.y2, digits = 1, title = "Summary Statistic: advertising pages / copy")


# ---- Reader Market
#tikz("/Users/Franzi/Desktop/R/circ_tv.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts((y31.2)/1000, type = "l", col = "red", ylab = "", xlab = "", 
        main = "sales in tsd.",
        ylim = c(800, 2100))

lines((y32.2)/1000, type = "l", col = "blue")
lines((y33.2)/1000, type = "l", col = "green")

#dev.off()

#-------- Ad Market
#tikz("/Users/Franzi/Desktop/R/ads_tv.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts(y41.2, type = "l", col = "red", xlab = "", ylab = "",
        main = "ad pages/copy", ylim = c(10, 60))

lines(y42.2, type = "l", col = "blue")
lines(y43.2, type = "l",col = "green")

legend("topleft", lty = c(1,1,1), lwd = c(2.5,2.5,2.5), bty = "n", horiz = FALSE,
       c("TVMovie", "TVSpielfilm", "TVDigital"),
       col = c("red", "blue", "green"))
#dev.off()

#######
# ACF #
#######

par(mfrow=c(3,2))         

Acf(y31.2, 20, main = "TVMovie (R.M.)")
Pacf(y31.2, 20, main = "TVMovie (R.M.)")

Acf(y32.2, 20, main = "TVSpielfilm (R.M.)")
Pacf(y32.2, 20, main = "TVSpielfilm (R.M.)")

Acf(y33.2, 20, main = "TVDigital (R.M.)")
Pacf(y33.2, 20, main = "TVDigital (R.M.)")

Acf(y41.2, 20, main = "TVMovie (A.M.)")
Pacf(y41.2, 20, main = "TVMovie (A.M.)")

Acf(y42.2, 20, main = "TVSpielfilm (A.M.)")
Pacf(y42.2, 20, main = "TVSpielfilm (A.M.)")

Acf(y43.2, 20, main = "TVDigital (A.M.)")
Pacf(y43.2, 20, main = "TVDigital (A.M.)")

###################################
# Phillips \& Perron Unit Root Test
###################################

# Null-Hypothesis: Series is non-stationary. If the test statistic is bigger than the critical value, we cannot reject the Null and Series is non-stationary. 

# ------- Reader Market
y31.2.p <- (ur.pp(y31.2, type = "Z-tau", model = "trend"))
y32.2.p <- (ur.pp(y32.2, type = "Z-tau", model = "trend"))
y33.2.p <- (ur.pp(y33.2, type = "Z-tau", model = "trend"))


# ------- Ad Market
y41.2.p <- (ur.pp(y41.2, type = "Z-tau", model = "trend"))
y42.2.p <- (ur.pp(y42.2, type = "Z-tau", model = "trend"))
y43.2.p <- (ur.pp(y43.2, type = "Z-tau", model = "trend"))

# ------ Latex Table 
c.y2p <- data.frame(y31.2.p@teststat, y32.2.p@teststat, y33.2.p@teststat)
c.y2p[nrow(c.y2p)+1,]<-c(y41.2.p@teststat, y42.2.p@teststat, y43.2.p@teststat)
c.y2p[nrow(c.y2p)+1,]<-c("1pct","5pct","10pct")
c.y2p[nrow(c.y2p)+1,]<-y31.2.p@cval
colnames(c.y2p) <- c("TVMovie", "TVSpielfilm", "TVDigital")
rownames(c.y2p) <- c("Sales", "Ad pages","Sig. Level", "Critical Values")

stargazer(c.y2p, summary = FALSE, digits = 1, title = "Phillips & Perron Unit Root")

######################
# Prewhitening / ARIMA
######################

# ------ Reader Markt

# TVMovie
Y31.2 <- auto.arima(y31.2, xreg = cbind(y32.2, y33.2), seasonal = FALSE)
Y31.2
#Y31.2 <- arima(y31.2, order = c(1,1,0), seasonal = list(order=c(1L,0L,0L)),  xreg = cbind(y32.2, y33.2))
resid31.2 <- Y31.2$residuals
Acf(resid31.2, 20)

# TVSpielfilm
Y32.2 <- auto.arima(y32.2, xreg = cbind(y31.2, y33.2), seasonal = FALSE)
Y32.2
#Y32.2 <- arima(y32.2, c(1,1,0),  seasonal = list(order=c(1L,0L,0L)), xreg = cbind(y31.2, y33.2))
resid32.2 <- Y32.2$residuals
Acf(resid32.2, 20)

# TVDigital
Y33.2 <- auto.arima(y33.2, xreg = cbind(y31.2, y32.2))
Y33.2
#Y33.2 <- arima(y33.2, c(1,0,0), seasonal = list(order=c(1L,0L,0L)), xreg = cbind(y31.2, y32.2))
resid33.2 <- Y33.2$residuals
Acf(resid33.2, 20)

# Latex Table
#stargazer(Y31.2, Y32.2, Y33.2, title = "Regression Results: Reader Market (2012-2015)", align = TRUE, dep.var.labels = c("TVMovie", "TVSpielfilm", "TVDigital"), omit.stat = c("LL"), no.space = TRUE)

# Plotting Residuals
#tikz("/Users/Franzi/Desktop/R/arima_circ_tv.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts((resid31.2)/1000, 
        type = "l",
        col = "red",
        ylim = c(-100,100),
        ylab = "",
        xlab = "",
        main = "sales (adjusted)")

lines((resid32.2)/1000, type = "l", col = "blue")
lines((resid33.2)/1000, type = "l", col = "green")

abline(a = 0, b = 0)

#dev.off()

# --------Ad Market

# TVMovie
Y41.2 <- auto.arima(y41.2, xreg = cbind(y42.2,y43.2), max.d = 0, seasonal = FALSE)
Y41.2
#Y41.2 <- arima(y41.2, c(1,0,0), xreg = cbind(y42.2,y43.2))
resid41.2 <- Y41.2$residuals
Acf(resid41.2)

# TVSpielfilm
Y42.2 <- auto.arima(y42.2, xreg = cbind(y41.2, y43.2), max.d = 0, max.p = 1, seasonal = FALSE)
Y42.2
# Y42.2 <- arima(y42.2, c(1,0,0), xreg = cbind(y41.2, y43.2))
resid42.2 <- Y42.2$residuals
Acf(resid42.2)

# TVDigital
Y43.2 <- auto.arima(y43.2, xreg = cbind(y41.2, y42.2))
Y43.2
#Y43.2 <- arima(y43.2, c(1,0,1), xreg = cbind(y41.2, y42.2))
resid43.2 <- Y43.2$residuals
Acf(resid42.2)

## Latex Table
#stargazer(Y41.2, Y42.2, Y43.2, title = "Regression Results: Ad Market 2012-2015", align = TRUE, dep.var.labels = c("TVMovie", "TVSpielfilm", "TVDigital"), omit.stat = c("LL"), no.space = TRUE)

# 2----------------
#tikz("/Users/Franzi/Desktop/R/arima_ads_tv.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts(resid41.2, 
        type = "l",
        col = "red",
        ylim = c(-30,30),
        ylab = "",
        xlab = "",
        main = "advertising pages/copy (adjusted)")

lines(resid42.2, type = "l", col = "blue")
lines(resid43.2, type = "l", col = "green")

abline(a = 0, b = 0)
legend("topleft", lty = c(1,1,1,1), lwd = c(2.5,2.5,2.5,2.5),  bty = "n", horiz = FALSE,
       c("TVMovie", "TVSpielfilm", "TVDigital"),
       col = c("red", "blue", "green"))

dev.off()

###################
# Cross Correlation
###################
# 1 ----------------
par(mfrow=c(2,3))
xcorr312.2 <- Ccf(resid31.2, resid32.2, 
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "TVMovie & TVSpielfilm (R.M.)")
xcorr313.2 <- Ccf(resid31.2, resid33.2, 
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "TVMovie & TVDigital (R.M.)")
xcorr323.2 <- Ccf(resid32.2, resid33.2, 
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "TVSpielfilm & TVDigital (R.M.)")
xcorr412.2 <- Ccf(resid41.2, resid42.2,
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "TVMovie & TVSpielfilm (A.M.)")
xcorr413.2 <- Ccf(resid41.2, resid43.2, 
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "TVMovie & TVDigital (A.M.)")
xcorr423.2 <- Ccf(resid42.2, resid43.2, 
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "TVSpielfilm & TVDigital (A.M.)")


# Latex Table
xcorr2.2 <- matrix(c(xcorr312.2$lag, xcorr312.2$acf,xcorr313.2$acf,xcorr323.2$acf), ncol = 4)
colnames(xcorr2.2) <- c("Lags", "TVMovie + Spielfilm", "TVMovie + TVDigital","TVSpielfilm + TVDigital")
stargazer(xcorr2.2, summary = FALSE)

xcorr1.2 <- matrix(c(xcorr412.2$lag, xcorr412.2$acf,xcorr413.2$acf,xcorr423.2$acf), ncol = 4)
colnames(xcorr1.2) <- c("Lags", "TVMovie + Spielfilm", "TVMovie + TVDigital","TVSpielfilm + TVDigital")
stargazer(xcorr1.2, summary = FALSE)

#############
# Residual AC
#############

# --- Reader Market
par(mfrow=c(3,1))

Acf(resid31.2, 20, main = "TVMovie, ARIMA(1,0,3)")
Acf(resid32.2, 20, main = "TV pielfilm, ARIMA(0,1,0)")
Acf(resid33.2, 20, main = "TVDigital, ARIMA(1,0,0)")


# ------- Ad Market
Acf(resid41.2, 20, main = "TVMovie, ARMA(2,0)")
Acf(resid42.2, 20, main = "TVSpielfilm, ARMA(1,0)")
Acf(resid43.2, 20, main = "TVDigital, ARMA(2,0)")


###################
# Granger Causality
###################

write.csv(cbind(resid31.2, resid32.2, resid33.2, resid41.2, resid42.2, resid43.2), file="granger_tv.csv")

VARselect(cbind(resid31.2, resid32.2, resid33.2), lag.max = 12, type = "none")
VARselect(cbind(resid41.2, resid42.2, resid43.2), lag.max = 12, type = "none")

var1.2 <- VAR(cbind(resid31.2, resid32.2, resid33.2))
var2.2 <- VAR(cbind(resid41.2, resid42.2, resid43.2))


# Impulse response function
# Reader Market
plot(irf(var.1.2))

# Ad Market
plot(irf(var.2.2))



