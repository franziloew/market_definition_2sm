setwd("~/CloudStation (Shared)/Projekte/Marktabgrezung 2SM")

# Load Packages
if(!require(devtools)) install.packages("devtools")
devtools::install_github("sinhrks/ggfortify")
library("ggfortify")
devtools::install_github("kassambara/ggcorrplot")
library(ggcorrplot)
library(xts)
library(scales)
library(forecast)
library("ggthemes")
library(stargazer)

# Market: Online News 
rm(list = ls())
load("output/news.Rda")
source("R/functions.R")

# define color palette
#tmaptools::palette_explorer()

fig_news <- autoplot(news.xts, facets = F) +
  labs(title = "", x = "", y = "Visits in 10K", color = "") +
  theme_minimal() +
  scale_colour_brewer(palette="Paired") +
  theme(legend.position = "bottom") +
  scale_x_date(breaks = pretty_breaks(10))

# Subsample 1
fig_news + 
  geom_vline(xintercept = as.numeric(as.Date("2003-05-1")), linetype = "dashed", lwd=.5) +
  geom_vline(xintercept = as.numeric(as.Date("2007-05-1")), linetype = "dashed", lwd=.5) 

# save plot
ggsave("figs/fig_news1.pdf",  width = 8, height = 6)

# Subsample 2
fig_news + 
  geom_vline(xintercept = as.numeric(as.Date("2010-05-1")), linetype = "dashed", lwd=.5) +
  geom_vline(xintercept = as.numeric(as.Date("2014-05-1")), linetype = "dashed", lwd=.5) 

# save plot
ggsave("figs/fig_news2.pdf",  width = 8, height = 6)

# Select subsample
news1.xts<- news.xts['2003-05/2007-05']
news2.xts<- news.xts['2010-05/2014-05']

# (1) 2003-2007 ####
# Regression ####
pdf("figs/acf_news1.pdf", width = 7, height = 7)
par(mfrow=c(3,3))
# Bild.de
bild.arima <- Arima(news1.xts[,1], xreg = cbind(news1.xts[,-1]), order = c(1,0,0))
#summary(bild.arima)
Acf(bild.arima$residuals)

# DIE WELT
welt.arima <- Arima(news1.xts[,2],xreg = cbind(news1.xts[,-2]), order = c(1,0,0))
#summary(welt.arima)
Acf(welt.arima$residuals)

# FAZ.NET
faz.arima <- Arima(news1.xts[,3],xreg = cbind(news1.xts[,-3]), order = c(1,0,0))
#summary(faz.arima)
Acf(faz.arima$residuals)

# FOCUS ONLINE
focus.arima <- Arima(news1.xts[,4],xreg = cbind(news1.xts[,-4]), order = c(1,0,0))
#summary(focus.arima)
Acf(focus.arima$residuals)

# SPIEGEL ONLINE
spiegel.arima <- Arima(news1.xts[,5],xreg = cbind(news1.xts[,-5]), order = c(1,0,0))
#summary(spiegel.arima)
Acf(spiegel.arima$residuals)

# ZEIT ONLINE
zeit.arima <- Arima(news1.xts[,6],xreg = cbind(news1.xts[,-6]), order = c(1,0,0))
#summary(zeit.arima)
Acf(zeit.arima$residuals)

# nt-v.de
ntv.arima <- Arima(news1.xts[,7],xreg = cbind(news1.xts[,-7]), order = c(1,0,0))
#summary(ntv.arima)
Acf(ntv.arima$residuals)

# Stern
stern.arima <- Arima(news1.xts[,8],xreg = cbind(news1.xts[,-8]), order = c(1,0,0))
#summary(stern.arima)
Acf(stern.arima$residuals)

# SZ.de
sz.arima <- Arima(news1.xts[,9],xreg = cbind(news1.xts[,-9]), order = c(1,0,0))
#summary(sz.arima)
Acf(sz.arima$residuals)

dev.off()

# Plotting residuals
resid <- cbind(bild.arima$residuals, welt.arima$residuals, faz.arima$residuals, focus.arima$residuals, spiegel.arima$residuals, zeit.arima$residuals, ntv.arima$residuals, stern.arima$residuals, sz.arima$residuals)

# Plot residuals
autoplot(resid) + 
  geom_line(lwd=.3, alpha=1) +
  labs(title = "", x = "", y = "Residuals in tsd.", color = "") +
  scale_color_hue(labels = names(news1.xts)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0, lwd=.2)
ggsave("figs/resid_news1.pdf",  width = 8, height = 6)

# Ccf ####
# calculate every combination of ccf
par(mfrow=c(3,3))
crossc <- combn(seq_len(ncol(resid)), 2,
                FUN=function(x) Ccf(resid[, x[1]], resid[, x[2]]), simplify=F)
dev.off()

# extract ccf from list object
cc <- data.frame(purrr::map(crossc, "acf"))

# find correpsonting pair-names
mypairs <- combn(seq_len(ncol(resid)), 2)
mypairs1 <- as.character(mypairs[1,])
mypairs2 <- as.character(mypairs[2,])
mypairs <- rbind(mypairs1,mypairs2)

mypairs <- gsub("1", "Bild.de", mypairs) 
mypairs <- gsub("2", "DIE WELT", mypairs) 
mypairs <- gsub("3", "FAZ.NET", mypairs) 
mypairs <- gsub("4", "FOCUS ONLINE", mypairs) 
mypairs <- gsub("5", "SPIEGEL ONLINE", mypairs) 
mypairs <- gsub("6", "ZEIT ONLINE", mypairs) 
mypairs <- gsub("7", "n-tv.de", mypairs) 
mypairs <- gsub("8", "stern.de", mypairs) 
mypairs <- gsub("9", "sueddeutsche.de", mypairs) 

pairnames <- paste(mypairs[1,],mypairs[2,], sep = "-")

names(cc) <- pairnames

# Row Names (lag)
lag <- purrr::map(crossc, "lag")
lag <- unlist(lag[1])
rownames(cc) <- lag

# subset lags -5/5
cc1 <- cc[12:22,]
#transpose table
cc1 <- t(cc1)

# sort by contemporary ccf
# cc1 <- cc1[order(cc1[,"0"]),]

# only contemporary correlation
cc0 <- data.frame(cc1[,6])
cc0$pair1 <- mypairs[1,]
cc0$pair2 <- mypairs[2,]
rownames(cc0) <- seq(1:36)

names(cc0)[names(cc0)=="cc1...6."] <- "ccf"

# build matrix
start <- data.frame(diag(1,9,9))
start[1,2:9] <- cc0[1:8,1]      #Bild.de
start[2,3:9] <- cc0[9:15,1]     #DIE WELT
start[3,4:9] <- cc0[16:21,1]    #FAZ.NET
start[4,5:9] <- cc0[22:26,1]    #FOCUS ONLINE
start[5,6:9] <- cc0[27:30,1]    #SPIEGEL ONLINE
start[6,7:9] <- cc0[31:33,1]    #ZEIT ONLINE
start[7,8:9] <- cc0[34:35,1]    #n-tv
start[8,9] <- cc0[36,1]         #sueddeutsche.de

names(start) <- names(news)
rownames(start) <- t(names(news))

# Copy upper triangle to lower triangle
start[lower.tri(start)] <- t(start)[lower.tri(start)]

# Correlogram ####
ggcorrplot(start, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           #colors = c("tomato2", "white", "springgreen3"), 
           title="", 
           ggtheme=theme_bw)
ggsave("figs/ccf_news1.pdf",  width = 7, height = 6)

# (2) 2010-2014 ####
# Regression ####
pdf("figs/acf_news2.pdf", width = 7, height = 7)
par(mfrow=c(3,3))
# Bild.de
bild.arima <- Arima(news2.xts[,1], xreg = cbind(news2.xts[,-1]), order = c(1,0,0))
#summary(bild.arima)
Acf(bild.arima$residuals)

# FAZ.NET
faz.arima <- Arima(news2.xts[,2],xreg = cbind(news2.xts[,-2]), order = c(1,0,0))
#summary(faz.arima)
Acf(faz.arima$residuals)

# FOCUS ONLINE
focus.arima <- Arima(news2.xts[,3],xreg = cbind(news2.xts[,-3]), order = c(1,0,0))
#summary(focus.arima)
Acf(focus.arima$residuals)

# SPIEGEL ONLINE
spiegel.arima <- Arima(news2.xts[,4],xreg = cbind(news2.xts[,-4]), order = c(1,0,0))
#summary(spiegel.arima)
Acf(spiegel.arima$residuals)

# WELT ONLINE
welt.arima <- Arima(news2.xts[,5],xreg = cbind(news2.xts[,-5]), order = c(1,0,0))
#summary(welt.arima)
Acf(welt.arima$residuals)

# ZEIT ONLINE
zeit.arima <- Arima(news2.xts[,6],xreg = cbind(news2.xts[,-6]), order = c(1,0,0))
#summary(zeit.arima)
Acf(zeit.arima$residuals)

# nt-v.de
ntv.arima <- Arima(news2.xts[,7],xreg = cbind(news2.xts[,-7]), order = c(1,0,0))
#summary(ntv.arima)
Acf(ntv.arima$residuals)

# Stern
stern.arima <- Arima(news2.xts[,8],xreg = cbind(news2.xts[,-8]), order = c(1,0,0))
#summary(stern.arima)
Acf(stern.arima$residuals)

# SZ.de
sz.arima <- Arima(news2.xts[,9],xreg = cbind(news2.xts[,-9]), order = c(1,0,0))
#summary(sz.arima)
Acf(sz.arima$residuals)

dev.off()

# Plotting residuals
resid <- cbind(bild.arima$residuals, welt.arima$residuals, faz.arima$residuals, focus.arima$residuals, spiegel.arima$residuals, zeit.arima$residuals, ntv.arima$residuals, stern.arima$residuals, sz.arima$residuals)

# Plot residuals
autoplot(resid) + 
  geom_line(lwd=.3, alpha=1) +
  labs(title = "", x = "", y = "Residuals in tsd.", color = "") +
  scale_color_hue(labels = names(news1.xts)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0, lwd=.2)
ggsave("figs/resid_news2.pdf",  width = 8, height = 6)

# Ccf ####
# calculate every combination of ccf
par(mfrow=c(3,3))
crossc <- combn(seq_len(ncol(resid)), 2,
                FUN=function(x) Ccf(resid[, x[1]], resid[, x[2]]), simplify=F)
dev.off()

# extract ccf from list object
cc <- data.frame(purrr::map(crossc, "acf"))

# find correpsonting pair-names
mypairs <- combn(seq_len(ncol(resid)), 2)
mypairs1 <- as.character(mypairs[1,])
mypairs2 <- as.character(mypairs[2,])
mypairs <- rbind(mypairs1,mypairs2)

mypairs <- gsub("1", "Bild.de", mypairs) 
mypairs <- gsub("2", "DIE WELT", mypairs) 
mypairs <- gsub("3", "FAZ.NET", mypairs) 
mypairs <- gsub("4", "FOCUS ONLINE", mypairs) 
mypairs <- gsub("5", "SPIEGEL ONLINE", mypairs) 
mypairs <- gsub("6", "ZEIT ONLINE", mypairs) 
mypairs <- gsub("7", "n-tv.de", mypairs) 
mypairs <- gsub("8", "stern.de", mypairs) 
mypairs <- gsub("9", "sueddeutsche.de", mypairs) 

pairnames <- paste(mypairs[1,],mypairs[2,], sep = "-")

names(cc) <- pairnames

# Row Names (lag)
lag <- purrr::map(crossc, "lag")
lag <- unlist(lag[1])
rownames(cc) <- lag

# subset lags -5/5
cc1 <- cc[12:22,]
#transpose table
cc1 <- t(cc1)

# sort by contemporary ccf
# cc1 <- cc1[order(cc1[,"0"]),]

# only contemporary correlation
cc0 <- data.frame(cc1[,6])
cc0$pair1 <- mypairs[1,]
cc0$pair2 <- mypairs[2,]
rownames(cc0) <- seq(1:36)

names(cc0)[names(cc0)=="cc1...6."] <- "ccf"

# build matrix
start <- data.frame(diag(1,9,9))
start[1,2:9] <- cc0[1:8,1]      #Bild.de
start[2,3:9] <- cc0[9:15,1]     #DIE WELT
start[3,4:9] <- cc0[16:21,1]    #FAZ.NET
start[4,5:9] <- cc0[22:26,1]    #FOCUS ONLINE
start[5,6:9] <- cc0[27:30,1]    #SPIEGEL ONLINE
start[6,7:9] <- cc0[31:33,1]    #ZEIT ONLINE
start[7,8:9] <- cc0[34:35,1]    #n-tv
start[8,9] <- cc0[36,1]         #sueddeutsche.de

names(start) <- names(news)
rownames(start) <- t(names(news))

# Copy upper triangle to lower triangle
start[lower.tri(start)] <- t(start)[lower.tri(start)]

# Correlogram ####
ggcorrplot(start, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           #colors = c("tomato2", "white", "springgreen3"), 
           title="", 
           ggtheme=theme_bw)
ggsave("figs/ccf_news1.pdf",  width = 7, height = 6)

# output table
# t_ccf <- stargazer(cc1, summary=F)
# writeLines(t_ccf, con = "doc/tables/t_ccf.tex")



