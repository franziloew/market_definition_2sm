library("urca")
library("tseries")
library("seasonal")
library("lmtest")
library("vars")
library("forecast")
library(ggplot2)

rm(list=ls())

setwd("~/CloudStation (Shared)/Projekte/Marktabgrezung 2SM")
load("output/women.Rda")

# (1) Prewhitening / ARIMA ####
# (1.1.) Reader Markt ####
# Brigitte
brigitte_sales <- auto.arima(women_sales_sub[,1],xreg = cbind(women_sales_sub[,2], women_sales_sub[,3]))
summary(brigtte_sales)

# Für Sie
fuersie_sales <- auto.arima(women_sales_sub[,2],xreg = cbind(women_sales_sub[,1], women_sales_sub[,3]))
summary(fuersie_sales)

# freundin
freundin_sales <- auto.arima(women_sales_sub[,3],xreg = cbind(women_sales_sub[,1], women_sales_sub[,2]))
summary(freundin_sales)

# Latex Table
# t_fss1.1 <- stargazer(brigtte_sales1, brigtte1.1, freundin1.1, title = "Regression Results: Reader Market (2004-2006)", align = TRUE, dep.var.labels = c("fuersie", "Der brigtte", "freundin"), omit.stat = c("LL"), no.space = TRUE)
# 
# writeLines(t_fss1.1, con = "doc/tables/t_fss1.1.tex")

# Plotting residuals
autoplot(((cbind(brigitte_sales$residuals,fuersie_sales$residuals,freundin_sales$residuals))/1000)) + 
  geom_line(lwd=.3, alpha=1) +
  labs(title = "", x = "", y = "Residuals in tsd.", color = "") +
  scale_color_hue(labels = c("Brigitte", "Für Sie", "Freundin")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0, lwd=.2) +
  coord_cartesian(ylim = c(-150, 150))
ggsave("figs/arima_sales_women.pdf", width = 8, height = 6)

# (1.2.) Ad Market ####
# fuersie
brigitte_ads <- auto.arima(women_ads_sub[,1],xreg = cbind(women_ads_sub[,2], women_ads_sub[,3]))
summary(brigtte_ads1)

# Der Siegel
fuersie_ads <- auto.arima(women_ads_sub[,2],xreg = cbind(women_ads_sub[,1], women_ads_sub[,3]))
summary(fuersie_ads1)

# freundin
freundin_ads <- auto.arima(women_ads_sub[,3],xreg = cbind(women_ads_sub[,1], women_ads_sub[,2]))
summary(freundin_ads1)

# Plotting residuals
autoplot((cbind(brigitte_ads$residuals,fuersie_ads$residuals,freundin_ads$residuals))) + 
  geom_line(lwd=.3, alpha=1) +
  labs(title = "", x = "", y = "Residuals", color = "") +
  scale_color_hue(labels = c("Brigitte", "Für Sie", "Freundin")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(-80, 80)) +
  geom_hline(yintercept = 0, lwd=.2) 
ggsave("figs/arima_ads_women.pdf", width = 8, height = 6)


# (2) CrossCorrelation ####
pdf("figs/ccf_women.pdf", width = 7, height = 7)
# Reader Market
par(mfrow=c(2,3))
xcorr12.1 <- Ccf(brigitte_sales$residuals, fuersie_sales$residuals,
                 lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                 main = "Brigitte & Für Sie (R.M.)")
xcorr13.1 <- Ccf(brigitte_sales$residuals, freundin_sales$residuals, 
                 lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                 main = "Brigitte & Freundin (R.M.)")
xcorr23.1 <- Ccf(fuersie_sales$residuals, freundin_sales$residuals, 
                 lag.max = 6,xlim = c(-6,6), ylim=c(-1,1),
                 main = "Für Sie & Freundin (R.M.)")
# Ad Market
xcorr12.2 <- Ccf(brigitte_ads$residuals, fuersie_ads$residuals,
                 lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                 main = "Brigitte & Für Sie (A.M.)")
xcorr13.2 <- Ccf(brigitte_ads$residuals, freundin_ads$residuals,
                 lag.max = 6, xlim = c(-6,6), ylim=c(-1,1),
                 main = "Brigitte & Freundin (A.M.)")
xcorr23.2 <- Ccf(fuersie_ads$residuals, freundin_ads$residuals, 
                 lag.max = 6,xlim = c(-6,6), ylim=c(-1,1),
                 main = "Für Sie & Freundin (A.M.)")

dev.off()

# (3) Autocorrelation ####
# (3.1.) Before Prewhitening ----
pdf("figs/acf_women.pdf", width = 7, height = 7)
par(mfrow=c(2,3))
Acf(coredata(women_sales_sub[,1]), main="Brigtte R.M.")
Acf(coredata(women_sales_sub[,2]), main="Für Sie R.M.")
Acf(coredata(women_sales_sub[,3]), main="Freundin R.M.")

Acf(coredata(women_ads_sub[,1]), main="Brigtte A.M.")
Acf(coredata(women_ads_sub[,2]), main="Für Sie A.M.")
Acf(coredata(women_ads_sub[,3]), main="Freundin A.M.")
dev.off()

# (3.2.) After Prewhitening ----
pdf("figs/resid_acf_women.pdf", width = 7, height = 7)
par(mfrow=c(2,3))
Acf(brigitte_sales$residuals, 20, ylim=c(-0.5,0.5), main = "Brigitte (R.M.)")
Acf(fuersie_sales$residuals, 20, ylim=c(-0.5,0.5), main = "Für Sie (R.M.)")
Acf(freundin_sales$residuals, 20, ylim=c(-0.5,0.5), main = "Freundin (R.M.)")

Acf(brigitte_ads$residuals, 20, ylim=c(-0.5,0.5), main = "Brigitte (A.M.)")
Acf(fuersie_ads$residuals, 20, ylim=c(-0.5,0.5), main = "Für Sie (A.M.)")
Acf(freundin_ads$residuals, 20, ylim=c(-0.5,0.5), main = "Freundin (A.M.)")
dev.off()

