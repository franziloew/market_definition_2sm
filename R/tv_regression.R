library("urca")
library("tseries")
library("seasonal")
library("lmtest")
library("vars")
library("forecast")
library(ggplot2)

rm(list=ls())

setwd("~/CloudStation (Shared)/Projekte/Marktabgrezung 2SM")
load("output/tv.Rda")

# (1) Prewhitening / ARIMA ####
# (1.1.) Reader Markt ####
# TV Movie
tvmovie_sales1 <- auto.arima(tv_sales_sub[,1],xreg = cbind(tv_sales_sub[,2], tv_sales_sub[,3]))
summary(tvmovie_sales1)

# TV Spielfilm
tvspielfilm_sales1 <- auto.arima(tv_sales_sub[,2],xreg = cbind(tv_sales_sub[,1], tv_sales_sub[,3]))
summary(tvspielfilm_sales1)

# TV Today
tvtoday_sales1 <- auto.arima(tv_sales_sub[,3],xreg = cbind(tv_sales_sub[,1], tv_sales_sub[,2]))
summary(tvtoday_sales1)

# Plotting residuals
autoplot(((cbind(tvmovie_sales1$residuals,tvspielfilm_sales1$residuals,tvtoday_sales1$residuals))/1000)) + 
  geom_line(lwd=.3, alpha=1) +
  labs(title = "", x = "", y = "Residuals in tsd.", color = "") +
  scale_color_hue(labels = c("TV Movie", "TV Spielfilm", "TV Today")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0, lwd=.2) +
  coord_cartesian(ylim = c(-150, 150))
ggsave("figs/arima_sales_tv.pdf",  width = 8, height = 6)

# (1.2.) Ad Market ####
# TV Movie
tvmovie_ads1 <- auto.arima(tv_ads_sub[,1],xreg = cbind(tv_ads_sub[,2], tv_ads_sub[,3]))
summary(tvmovie_ads1)

# TV Spielfilm
tvspielfilm_ads1 <- auto.arima(tv_ads_sub[,2],xreg = cbind(tv_ads_sub[,1], tv_ads_sub[,3]))
summary(tvspielfilm_ads1)

# tvtoday
tvtoday_ads1 <- auto.arima(tv_ads_sub[,3],xreg = cbind(tv_ads_sub[,1], tv_ads_sub[,2]))
summary(tvtoday_ads1)

# Plotting residuals
autoplot((cbind(tvmovie_ads1$residuals,tvspielfilm_ads1$residuals,tvtoday_ads1$residuals))) + 
  geom_line(lwd=.3, alpha=1) +
  labs(title = "", x = "", y = "Residuals", color = "") +
  scale_color_hue(labels = c("TV Movie", "TV Spielfilm", "TV Today")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(-40, 40)) +
  geom_hline(yintercept = 0, lwd=.2) 
ggsave("figs/arima_ads_tv.pdf",  width = 8, height = 6)

# (2) CrossCorrelation ####
# Reader Market
pdf('figs/ccf_tv.pdf', width = 7, height = 7)
par(mfrow=c(2,3))
xcorr12.1 <- Ccf(tvspielfilm_sales1$residuals, tvmovie_sales1$residuals,
                 lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                 main = "TV Spielfilm & TV Movie (R.M.)")
xcorr13.1 <- Ccf(tvspielfilm_sales1$residuals, tvtoday_sales1$residuals, 
                 lag.max = 6,xlim = c(-6,6), ylim=c(-1,1),
                 main = "TV Spielfilm & TV Today (R.M.)")
xcorr23.1 <- Ccf(tvtoday_sales1$residuals, tvmovie_sales1$residuals, 
                 lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                 main = "TV Movie & TV Today (R.M.)")
# Ad Market
xcorr12.2 <- Ccf(tvspielfilm_ads1$residuals, tvmovie_ads1$residuals,
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "TV Spielfilm & TV Movie (A.M.)")
xcorr13.2 <- Ccf(tvspielfilm_ads1$residuals, tvtoday_ads1$residuals, 
                  lag.max = 6,xlim = c(-6,6), ylim=c(-1,1),
                  main = "TV Spielfilm & TV Today (A.M.)")
xcorr23.2 <- Ccf(tvtoday_ads1$residuals, tvmovie_ads1$residuals, 
                  lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                  main = "TV Movie & TV Today (A.M.)")
dev.off()
xcorr13.1
# Autocorrelation ####
# (1) Before Prewhitening ----
pdf("figs/acf_tv.pdf",width = 7, height = 8 )
par(mfrow=c(2,3))
Acf(coredata(tv_sales_sub[,1]), main="tvmovie R.M.")
Acf(coredata(tv_sales_sub[,2]), main="tvspielfilm R.M.")
Acf(coredata(tv_sales_sub[,3]), main="tvtoday R.M.")

Acf(coredata(tv_ads_sub[,1]), main="tvmovie A.M.")
Acf(coredata(tv_ads_sub[,2]), main="tvspielfilm A.M.")
Acf(coredata(tv_ads_sub[,3]), main="tvtoday A.M.")
dev.off()

# (2) After Prewhitening ----
pdf("figs/resid_acf_tv.pdf", width = 7, height = 8)
par(mfrow=c(2,3))
Acf(tvmovie_sales1$residuals, 20, ylim=c(-0.5,0.5), main = "tvmovie (R.M.)")
Acf(tvspielfilm_sales1$residuals, 20, ylim=c(-0.5,0.5), main = "tvspielfilm (R.M.)")
Acf(tvtoday_sales1$residuals, 20, ylim=c(-0.5,0.5), main = "tvtoday (R.M.)")

Acf(tvmovie_ads1$residuals, 20, ylim=c(-0.5,0.5), main = "Der tvmovie (A.M.)")
Acf(tvspielfilm_ads1$residuals, 20, ylim=c(-0.5,0.5), main = "tvspielfilm (A.M.)")
Acf(tvtoday_ads1$residuals, 20, ylim=c(-0.5,0.5), main = "tvtoday (A.M.)")
dev.off()
