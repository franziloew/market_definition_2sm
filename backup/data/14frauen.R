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
x11 <- frauen[,"cpriceBrigitte"]
y21 <- frauen[,"totaladsBrigitte"]
x21 <- (frauen[,"totaladsBrigitte"]/frauen[,"contentBrigitte"])*100

# Sub2
y12 <- frauen[,"retailfreundin"]
x12 <- frauen[,"cpricefreundin"]
y22 <- frauen[,"totaladsfreundin"]
x22 <- (frauen[,"totaladsfreundin"]/frauen[,"contentfreundin"])*100

# Sub3
y13 <- frauen[,"retailfuerSie"]
x13 <- frauen[,"cpricefuerSie"]
y23 <- frauen[,"totaladsfuerSie"]
x23 <- (frauen[,"totaladsfuerSie"]/frauen[,"contentfuerSie"])*100

# Time Trend
t<-seq(0, 1, length=length(frauen))
t <- ts(t, start=c(2003,1), end = c(2016,26), frequency = 26)

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

# time trend
t <- window(t, start=c(2011,15), end = c(2014,15), frequency = 26)

# Fill NA
y11.2[which(is.na(y11.2))] <- mean(y11.2, na.rm = TRUE)
y12.2[which(is.na(y12.2))] <- mean(y12.2, na.rm = TRUE)
y13.2[which(is.na(y13.2))] <- mean(y13.2, na.rm = TRUE)
# 
y21.2[which(is.na(y21.2))] <- mean(y21.2, na.rm = TRUE)
y22.2[which(is.na(y22.2))] <- mean(y22.2, na.rm = TRUE)
y23.2[which(is.na(y23.2))] <- mean(y23.2, na.rm = TRUE)

######################
# Summary and plotting
######################

# ----- Reader Market
combined.y1 <- data.frame(y11.2, y12.2, y13.2)
colnames(combined.y1) <- c("Brigitte", "Freundin", "FuerSie")

stargazer(combined.y1, digits = 1, title = "Summary Statistics")

# ----- Ad Market
combined.y2 <- data.frame(y21.2, y22.2, y23.2)
colnames(combined.y2) <- c("Brigitte", "Freundin", "FuerSie")

stargazer(combined.y2, digits = 1, title = "Summary Statistic: advertising Market")

# ---- Reader Market

#tikz("/Users/Franzi/Desktop/R/circ_frauen.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts((y11.2)/1000, 
        type = "l",
        col = "red",
        ylab = "",
        xlab ="",
        main= "sales in tsd.",
        ylim = c(0, 400))

lines((y12.2)/1000, type = "l", col = "blue")
lines((y13.2)/1000, type = "l", col = "green")

legend("topleft", lty = c(1,1,1), lwd = c(2.5,2.5,2.5), bty = "n", horiz = FALSE,
       col = c("red", "blue", "green"),
       c("Brigitte", "Freundin", "FuerSie"))
#dev.off()

#-------- Ad Market

#tikz("/Users/Franzi/Desktop/R/ads_frauen.tex",width=5,height=3.5)
plot.ts(y21.2, 
        type = "l",
        col = "red",
        ylab = "",
        xlab ="",
        main = "advertising pages/copy",
        ylim = c(0,160))

lines(y22.2, type = "l", col = "blue")
lines(y23.2,  type = "l", col = "green")

dev.off()


#######
# ACF #
#######

par(mfrow=c(3,2))         

Acf(y11.2, 18, main = "Brigitte (R.M.)")
Pacf(y11.2, 18, main = "Brigitte (R.M.)")

Acf(y12.2, 18, main = "Freundin (R.M.)")
Pacf(y12.2, 18, main = "Freundin (R.M.)")

Acf(y13.2, 18, main = "FuerSie (R.M.)")
Pacf(y13.2, 18, main = "FuerSie (R.M.)")

Acf(y21.2, 18, main = "Brigitte (A.M)")
Pacf(y21.2, 18, main = "Brigitte (A.M)")

Acf(y22.2, 18, main = "Freundin (A.M.)")
Pacf(y22.2, 18, main = "Freundin (A.M.)")

Acf(y23.2, 18, main = "FuerSie (A.M.)")
Pacf(y23.2, 18, main = "FuerSie (A.M.)")


###################################
# Phillips \& Perron Unit Root Test
###################################

# Null-Hypothesis: Series is non-stationary. If the test statistic is bigger than the critical value, we cannot reject the Null and Series is non-stationary. 

# ------- Reader Market
y11.2.p <- (ur.pp(y11.2, type = "Z-tau", model = "trend"))
y12.2.p <- (ur.pp(y12.2, type = "Z-tau", model = "trend"))
y13.2.p <- (ur.pp(y13.2, type = "Z-tau", model = "trend"))


# ------- Ad Market
y21.2.p <- (ur.pp(y21.2, type = "Z-tau", model = "trend"))
y22.2.p <- (ur.pp(y22.2, type = "Z-tau", model = "trend"))
y23.2.p <- (ur.pp(y23.2, type = "Z-tau", model = "trend"))

# ------ Latex Table 
# 2013-2015-------- 
c.y2p <- data.frame(y11.2.p@teststat, y12.2.p@teststat, y13.2.p@teststat)
c.y2p[nrow(c.y2p)+1,]<-c(y21.2.p@teststat, y22.2.p@teststat, y23.2.p@teststat)
c.y2p[nrow(c.y2p)+1,]<-c("1pct","5pct","10pct")
c.y2p[nrow(c.y2p)+1,]<-y11.2.p@cval
colnames(c.y2p) <- c("Brigitte", "Freundin", "FuerSie")
rownames(c.y2p) <- c("Sales", "Ad pages","Sig. Level", "Critical Values")

stargazer(c.y2p, summary = FALSE, digits = 1, title = "Unit Root")

######################
# Prewhitening / ARIMA
######################

# ------ Reader Markt

# Brigitte
Y11.2 <- auto.arima(y11.2, xreg = cbind(y12.2, y13.2), d = 0, seasonal = FALSE)
Y11.2
Y11.2 <- arima(y11.2, order = c(2,0,2), xreg = cbind(y12.2, y13.2))
resid11.2 <- Y11.2$residuals
Acf(resid11.2, 20)

# Freundin
Y12.2 <- auto.arima(y12.2, xreg = cbind(y11.2, y13.2), d = 0, seasonal = FALSE)
Y12.2
Y11.2 <- arima(y12.2, order = c(1,0,0), xreg = cbind(y11.2, y13.2))
resid12.2 <- Y12.2$residuals
Acf(resid12.2, 20)

# Für Sie
Y13.2 <- auto.arima(y13.2, xreg = cbind(y11.2, y12.2), d = 0, seasonal = FALSE)
Y13.2
#Y13.2 <- arima(y13.2, c(0,0,0), seasonal = list(order=c(1L,0L,0L)), xreg = cbind(y11.2, y12.2))
resid13.2 <- Y13.2$residuals
Acf(resid13.2, 20)

# Latex Table
#stargazer(Y11.2, Y12.2, Y13.2, title = "Regression Results: Reader Market (2012-2015)", align = TRUE, dep.var.labels = c("Brigitte", "Freundin", "FuerSie"), omit.stat = c("LL"), no.space = TRUE)

# Plotting Residuals
tikz("/Users/Franzi/Desktop/R/arima_circ_frauen.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts((resid11.2)/1000, 
        type = "l",
        col = "red",
        ylim = c(-100,100),
        ylab = "",
        xlab = "",
        main = "sales (adjusted)")

lines((resid12.2)/1000, type = "l", col = "blue")
lines((resid13.2)/1000, type = "l", col = "green")

abline(a = 0, b = 0)
legend("bottomleft", lty = c(1,1,1), lwd = c(2.5,2.5,2.5), bty = "n", horiz = FALSE,
       col = c("red", "blue", "green"),
       c("Brigitte", "Freundin", "FuerSie"))
dev.off()

# --------Ad Market

# Brigitte
Y21.2 <- auto.arima(y21.2, xreg = cbind(y22.2, y23.2), max.d = 0)
Y21.2
#Y21.2 <- arima(y21.2, c(1,0,0), seasonal = list(order=c(1L,1L,0L)), xreg = cbind(y22.2, y23.2))
resid21.2 <- Y21.2$residuals
Acf(resid21.2, lag.max = 20)

# Freundin
Y22.2 <- auto.arima(y22.2, xreg = cbind(y21.2, y23.2))
Y22.2
Y22.2 <- arima(y22.2, c(4,0,2), xreg = cbind(y21.2, y23.2))
resid22.2 <- Y22.2$residuals
Acf(resid22.2, 20)

# Für Sie
Y23.2 <- auto.arima(y23.2, xreg = cbind(y21.2, y22.2))
Y23.2
Y23.2 <- arima(y23.2, c(2,0,1),xreg = cbind(y21.2, y22.2))
resid23.2 <- Y23.2$residuals
Acf(resid23.2)

## Latex Table
#stargazer(Y21.2, Y22.2, Y23.2, title = "Regression Results: Ad Market 2012-2015", align = TRUE, dep.var.labels = c("Brigitte", "Freundin", "FuerSie"), omit.stat = c("LL"), no.space = TRUE)

# 2----------------
#tikz("/Users/Franzi/Desktop/R/arima_ads_frauen.tex",width=5,height=3.5)
par(mfrow=c(1,1))
plot.ts(resid21.2, 
        type = "l",
        col = "red",
        ylim = c(-50,50),
        ylab = "",
        xlab = "",
        main = "ad pages/copy")

lines(resid22.2, type = "l",col = "blue")
lines(resid23.2, type = "l",col = "green")

abline(a = 0, b = 0)

#dev.off()

###################
# Cross Correlation
###################
# 1 ----------------
par(mfrow=c(2,3))
xcorr112.2 <- Ccf(resid11.2, resid12.2,
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "Brigitte & Freundin R.M.")
xcorr113.2 <- Ccf(resid11.2, resid13.2, 
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "Brigitte & Für Sie R.M.")
xcorr123.2 <- Ccf(resid12.2, resid13.2, 
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "Freundin & Für Sie R.M.")
xcorr212.2 <- Ccf(resid21.2, resid22.2,
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "Brigitte & Freundin A.M.")
xcorr213.2 <- Ccf(resid21.2, resid23.2, 
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "Brigitte & Für Sie A.M.")
xcorr223.2 <- Ccf(resid22.2, resid23.2, 
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
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
par(mfrow=c(3,1))

Acf(resid11.2, 20, main = "Brigitte, ARMA(1,0)")
Acf(resid12.2, 20, main = "Freundin, ARMA(2,2)")
Acf(resid13.2, 20, main = "Für Sie, ARMA(2,0)")

Acf(resid21.2, 20, main = "Brigitte, ARMA(1,0)")
Acf(resid22.2, 20, main = "Freundin, ARMA(4,2)")
Acf(resid23.2, 20, main = "Für Sie, ARMA(2,1)")


###################
# Granger Causality
###################
write.csv(cbind(resid11.2, resid12.2, resid13.2, resid21.2, resid22.2, resid23.2), file="granger_frauen.csv")

VARselect(cbind(resid11.2, resid12.2, resid13.2), lag.max = 12, type = "none")
VARselect(cbind(resid21.2, resid22.2, resid23.2), lag.max = 12, type = "none")


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
xcorr1 <- Ccf(resid11.2, resid32.2,
              lag.max = 6,
              xlim = c(-6,6),
              ylim=c(-1,1),
              main = "Brigitte & TV Spielfilm (R.M.)")
xcorr2 <- Ccf(resid13.2, resid31.2, 
              lag.max = 6,
              xlim = c(-6,6),
              ylim=c(-1,1),
              main = "Für Sie & TV Digital (R.M.)")
xcorr3 <- Ccf(resid12.2, resid33.2, 
              lag.max = 6,
              xlim = c(-6,6),
              ylim=c(-1,1),
              main = "Freundin & TV Digital (R.M.)")
xcorr1.2 <- Ccf(resid21.2, resid42.2,
              lag.max = 6,
              xlim = c(-6,6),
              ylim=c(-1,1),
              main = "Brigitte & TV Spielfilm (A.M.)")
xcorr2.2 <- Ccf(resid23.2, resid41.2, 
              lag.max = 6,
              xlim = c(-6,6),
              ylim=c(-1,1),
              main = "Für Sie & TV Movie (A.M.)")
xcorr3.2 <- Ccf(resid22.2, resid43.2, 
              lag.max = 6,
              xlim = c(-6,6),
              ylim=c(-1,1),
              main = "Freundin & TV Digital (A.M.)")




