library("urca")
library("tseries")
library("seasonal")
library("lmtest")
library("vars")
library("forecast")
library("ggplot2")
library("stargazer")

rm(list=ls())

setwd("~/CloudStation (Shared)/Projekte/Marktabgrezung 2SM")
load("output/fss.Rda")

# Prewhitening / ARIMA ####
# (1) Reader Markt ####
# (1.1) Sample 1 ----
# FOCUS
spiegel_sales1 <- auto.arima(fss_sales.xts1[,1],xreg = cbind(fss_sales.xts1[,2], fss_sales.xts1[,3]))
summary(spiegel_sales1)

# Der Siegel
focus_sales1 <- auto.arima(fss_sales.xts1[,2],xreg = cbind(fss_sales.xts1[,1], fss_sales.xts1[,3]))
summary(focus_sales1)

# Stern
stern_sales1 <- auto.arima(fss_sales.xts1[,3],xreg = cbind(fss_sales.xts1[,1], fss_sales.xts1[,2]))
summary(stern_sales1)

# Plotting residuals
autoplot(((cbind(focus_sales1$residuals,spiegel_sales1$residuals,stern_sales1$residuals))/1000)) + 
  geom_line(lwd=.3, alpha=1) +
  labs(title = "", x = "", y = "Residuals in tsd.", color = "") +
  scale_color_hue(labels = c("FOCUS", "Der Spiegel", "Stern")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0, lwd=.2) +
  coord_cartesian(ylim = c(-150, 150))
ggsave("figs/arima_sales_fss1.pdf",  width = 8, height = 6)

# (1.2) Sample 2 ----
# FOCUS
spiegel_sales2 <- auto.arima(fss_sales.xts2[,1],xreg = cbind(fss_sales.xts2[,2], fss_sales.xts2[,3]))
summary(spiegel_sales2)

# Der Siegel
focus_sales2 <- auto.arima(fss_sales.xts2[,2],xreg = cbind(fss_sales.xts2[,1], fss_sales.xts2[,3]))
summary(focus_sales2)

# Stern
stern_sales2 <- auto.arima(fss_sales.xts2[,3],xreg = cbind(fss_sales.xts2[,1], fss_sales.xts2[,2]))
summary(stern_sales2)

# Plotting residuals
autoplot(((cbind(focus_sales2$residuals,spiegel_sales2$residuals,stern_sales2$residuals))/1000)) + 
  geom_line(lwd=.3, alpha=1) +
  labs(title = "", x = "", y = "", color = "") +
  scale_color_hue(labels = c("Der Spiegel","FOCUS", "Stern")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0, lwd=.2) +
  coord_cartesian(ylim = c(-150, 150))
ggsave("figs/arima_sales_fss2.pdf",  width = 8, height = 6)

# (1.3) CrossCorrelation ####
# Reader Market
pdf("figs/ccf_sales_fss.pdf", width = 7, height = 7)
par(mfrow=c(2,3))
xcorr112.1 <- Ccf(focus_sales1$residuals, spiegel_sales1$residuals,
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "FOCUS & Der Spiegel (R.M.1)")
xcorr113.1 <- Ccf(focus_sales1$residuals, stern_sales1$residuals, 
                  lag.max = 6,xlim = c(-6,6), ylim=c(-1,1),
                  main = "FOCUS & Stern (R.M.1)")
xcorr123.1 <- Ccf(stern_sales1$residuals, spiegel_sales1$residuals, 
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "Der Spiegel & Stern (R.M.1)")
xcorr112.2 <- Ccf(focus_sales2$residuals, spiegel_sales2$residuals,
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "FOCUS & Der Spiegel (R.M.2)")
xcorr113.2 <- Ccf(focus_sales2$residuals, stern_sales2$residuals, 
                  lag.max = 6,xlim = c(-6,6), ylim=c(-1,1),
                  main = "FOCUS & Stern (R.M.2)")
xcorr123.2 <- Ccf(stern_sales2$residuals, spiegel_sales2$residuals, 
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "Der Spiegel & Stern (R.M.2)")
dev.off()

xcorr1.1 <- matrix(c(xcorr112.1$lag, xcorr112.1$acf,xcorr113.1$acf,xcorr123.1$acf), ncol = 4)
colnames(xcorr1.1) <- c("Lags", "FOCUS + DerSpiegel", "FOCUS + Stern","Der Spiegel + Stern")
stargazer(xcorr1.1, summary = FALSE, title = "Cross Correlation Reader Market")

# 2
xcorr1.2 <- matrix(c(xcorr112.2$lag, xcorr112.2$acf,xcorr113.2$acf,xcorr123.2$acf), ncol = 4)
colnames(xcorr1.2) <- c("Lags", "FOCUS + DerSpiegel", "FOCUS + Stern","Der Spiegel + Stern")
stargazer(xcorr1.2, summary = FALSE)

# (2) Ad Market ####
# (2.1) Sample 1 ----
# FOCUS
spiegel_ads1 <- auto.arima(fss_ads.xts1[,1],xreg = cbind(fss_ads.xts1[,2], fss_ads.xts1[,3]))
summary(spiegel_ads1)

# Der Siegel
focus_ads1 <- auto.arima(fss_ads.xts1[,2],xreg = cbind(fss_ads.xts1[,1], fss_ads.xts1[,3]))
summary(focus_ads1)

# Stern
stern_ads1 <- auto.arima(fss_ads.xts1[,3],xreg = cbind(fss_ads.xts1[,1], fss_ads.xts1[,2]))
summary(stern_ads1)

# Plotting residuals
autoplot((cbind(focus_ads1$residuals,spiegel_ads1$residuals,stern_ads1$residuals))) + 
  geom_line(lwd=.3, alpha=1) +
  labs(title = "", x = "", y = "Residuals", color = "") +
  scale_color_hue(labels = c("FOCUS", "Der Spiegel", "Stern")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(-40, 40)) +
  geom_hline(yintercept = 0, lwd=.2) 
ggsave("figs/arima_ads_fss1.pdf",  width = 8, height = 6)

# (2.2) Sample 2 ----
# FOCUS
spiegel_ads2 <- auto.arima(fss_ads.xts2[,1],xreg = cbind(fss_ads.xts2[,2], fss_ads.xts2[,3]))
summary(spiegel_ads2)

# Der Siegel
focus_ads2 <- auto.arima(fss_ads.xts2[,2],xreg = cbind(fss_ads.xts2[,1], fss_ads.xts2[,3]))
summary(focus_ads2)

# Stern
stern_ads2 <- auto.arima(fss_ads.xts2[,3],xreg = cbind(fss_ads.xts2[,1], fss_ads.xts2[,2]))
summary(stern_ads2)

# Plotting residuals
autoplot((cbind(focus_ads2$residuals,spiegel_ads2$residuals,stern_ads2$residuals))) + 
  geom_line(lwd=.3, alpha=1) +
  labs(title = "", x = "", y = "", color = "") +
  scale_color_hue(labels = c("FOCUS", "Der Spiegel", "Stern")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(-40, 40)) +
  geom_hline(yintercept = 0, lwd=.2) 
ggsave("figs/arima_ads_fss2.pdf",  width = 8, height = 6)

# (2.2) CrossCorrelation ####
# Ad Market
pdf("figs/ccf_ads_fss.pdf",  width = 7, height = 7)
par(mfrow=c(2,3))
xcorr212.1 <- Ccf(focus_ads1$residuals, spiegel_ads1$residuals,
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "FOCUS & Der Spiegel (R.M.1)")
xcorr213.1 <- Ccf(focus_ads1$residuals, stern_ads1$residuals, 
                  lag.max = 6,xlim = c(-6,6), ylim=c(-1,1),
                  main = "FOCUS & Stern (R.M.1)")
xcorr223.1 <- Ccf(stern_ads1$residuals, spiegel_ads1$residuals, 
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "Der Spiegel & Stern (R.M.1)")
xcorr212.2 <- Ccf(focus_ads2$residuals, spiegel_ads2$residuals,
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "FOCUS & Der Spiegel (R.M.2)")
xcorr213.2 <- Ccf(focus_ads2$residuals, stern_ads2$residuals, 
                  lag.max = 6,xlim = c(-6,6), ylim=c(-1,1),
                  main = "FOCUS & Stern (R.M.2)")
xcorr223.2 <- Ccf(stern_ads2$residuals, spiegel_ads2$residuals, 
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "Der Spiegel & Stern (R.M.2)")
dev.off()

# Latex Table
# xcorr2.1 <- matrix(c(xcorr212.1$lag, xcorr212.1$acf,xcorr213.1$acf,xcorr223.1$acf), ncol = 4)
# colnames(xcorr2.1) <- c("Lags", "FOCUS + DerSpiegel", "FOCUS + Stern","Der Spiegel + Stern")
# stargazer(xcorr2.1, summary = FALSE)
# 
# # 2
# xcorr2.2 <- matrix(c(xcorr212.2$lag, xcorr212.2$acf,xcorr213.2$acf,xcorr223.2$acf), ncol = 4)
# colnames(xcorr2.2) <- c("Lags", "FOCUS + DerSpiegel", "FOCUS + Stern","Der Spiegel + Stern")
# stargazer(xcorr2.2, summary = FALSE)

# Autocorrelation ####
# (1) Before Prewhitening ----
# Sample 1
pdf("figs/acf1_fss.pdf")
par(mfrow=c(2,3))
Acf(coredata(fss_sales.xts1[,1]), main="Der Spiegel R.M.1")
Acf(coredata(fss_sales.xts1[,2]), main="FOCUS R.M.1")
Acf(coredata(fss_sales.xts1[,3]), main="Stern R.M.1")

Acf(coredata(fss_ads.xts1[,1]), main="Der Spiegel A.M.1")
Acf(coredata(fss_ads.xts1[,2]), main="FOCUS A.M.1")
Acf(coredata(fss_ads.xts1[,3]), main="Stern A.M.1")
dev.off()

# Sample 2
pdf("figs/acf2_fss.pdf")
par(mfrow=c(2,3))
Acf(coredata(fss_sales.xts2[,1]), main="Der Spiegel R.M.2")
Acf(coredata(fss_sales.xts2[,2]), main="FOCUS R.M.2")
Acf(coredata(fss_sales.xts2[,3]), main="Stern R.M.2")

Acf(coredata(fss_ads.xts2[,1]), main="Der Spiegel A.M.2")
Acf(coredata(fss_ads.xts2[,2]), main="FOCUS A.M.2")
Acf(coredata(fss_ads.xts2[,3]), main="Stern A.M.2")
dev.off()

# (2) After Prewhitening ----
# Sample 1
pdf("figs/resid_acf1_fss.pdf")
par(mfrow=c(2,3))
Acf(spiegel_sales1$residuals, 20, main = "Der Spiegel (R.M.1)")
Acf(focus_sales1$residuals, 20, main = "FOCUS (R.M.1)")
Acf(stern_sales1$residuals, 20, main = "Stern (R.M.1)")

Acf(spiegel_ads1$residuals, 20, main = "Der Spiegel (A.M.1)")
Acf(focus_ads1$residuals, 20, main = "FOCUS (A.M.1)")
Acf(stern_ads1$residuals, 20, main = "Stern (A.M.1)")
dev.off()

# Sample 2
pdf("figs/resid_acf2_fss.pdf")
par(mfrow=c(2,3))
Acf(spiegel_sales2$residuals, 20, main = "Der Spiegel (R.M.2)")
Acf(focus_sales2$residuals, 20, main = "FOCUS (R.M.2)")
Acf(stern_sales2$residuals, 20, main = "Stern (R.M.2)")

Acf(spiegel_ads2$residuals, 20, main = "Der Spiegel (A.M.2)")
Acf(focus_ads2$residuals, 20, main = "FOCUS (A.M.2)")
Acf(stern_ads2$residuals, 20, main = "Stern (A.M.2)")
dev.off()

# Granger Causality ####

# 1
write.csv(cbind(resid11.1, resid12.1, resid13.1, resid21.1, resid22.1, resid23.1), file="granger_fss1.csv")

VARselect(cbind(resid11.1, resid12.1, resid13.1), lag.max = 12, type = "none")
VARselect(cbind(resid21.1, resid22.1, resid23.1), lag.max = 12, type = "none")

# 2
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
