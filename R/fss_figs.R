if(!require(devtools)) install.packages("devtools")
devtools::install_github("sinhrks/ggfortify")
install.packages("ggfortify")
library("ggfortify")
library(xts)
library(scales)

# Market: News Magazines 
setwd("~/CloudStation (Shared)/Projekte/Marktabgrezung 2SM")
rm(list = ls())

load("output/fss.Rda")

# (1) Plots ####

# ---- Reader Market
fss_sales <- autoplot((fss_sales.xts)/1000, facets = FALSE) +
  labs(title = "", x = "", y = "Sales in tsd.", color = "") +
  scale_color_hue(labels = names) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_date(breaks = pretty_breaks(20))

# adding sample region
fss_sales + 
  geom_vline(xintercept = as.numeric(as.Date("2004-08-18")), linetype = "dashed", lwd=.5) +
  geom_vline(xintercept = as.numeric(as.Date("2006-08-18")), linetype = "dashed", lwd=.5) +
  geom_vline(xintercept = as.numeric(as.Date("2013-08-18")), linetype = "dashed", lwd=.5) +
  geom_vline(xintercept = as.numeric(as.Date("2015-08-18")), linetype = "dashed", lwd=.5) 
# save plot
ggsave("figs/sales_fss.pdf",  width = 8, height = 6)

# --- Ad Market
fss_ads <- autoplot(fss_ads.xts, facets = FALSE) +
  labs(title = "", x = "", y = "Total advertising pages", color = "") +
  scale_color_hue(labels = names) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_date(breaks = pretty_breaks(20))

# adding sample region
fss_ads + 
  geom_vline(xintercept = as.numeric(as.Date("2004-08-18")), linetype = "dashed", lwd=.5) +
  geom_vline(xintercept = as.numeric(as.Date("2006-08-18")), linetype = "dashed", lwd=.5) +
  geom_vline(xintercept = as.numeric(as.Date("2013-08-18")), linetype = "dashed", lwd=.5) +
  geom_vline(xintercept = as.numeric(as.Date("2015-08-18")), linetype = "dashed", lwd=.5) 
#save plot
ggsave("figs/ads_fss.pdf",  width = 8, height = 6)

# (2) Phillips \& Perron Unit Root Test ####
# Null-Hypothesis: Series is non-stationary. If the test statistic is bigger than the critical value, we cannot reject the Null and Series is non-stationary
# Reader Market
# 1
focus_sales1.p <- pp.test(fss_sales.xts1[,"focus"])
spiegel_sales1.p <- pp.test(fss_sales.xts1[,"spiegel"])
stern_sales1.p <- pp.test(fss_sales.xts1[,"stern"])

# 2
focus_sales2.p <- pp.test(fss_sales.xts2[,"focus"])
spiegel_sales2.p <- pp.test(fss_sales.xts2[,"spiegel"])
stern_sales2.p <- pp.test(fss_sales.xts2[,"stern"])

# Ad Market
# 1
focus_ads1.p <- pp.test(fss_ads.xts1[,"focus"])
spiegel_ads1.p <- pp.test(fss_ads.xts1[,"spiegel"])
stern_ads1.p <- pp.test(fss_ads.xts1[,"stern"])

# 2
focus_ads2.p <- pp.test(fss_ads.xts2[,"focus"])
spiegel_ads2.p <- pp.test(fss_ads.xts2[,"spiegel"])
stern_ads2.p <- pp.test(fss_ads.xts2[,"stern"])

# ------ Latex Table 
sample1.1 <- c(focus_sales1.p$p.value, spiegel_sales1.p$p.value, stern_sales1.p$p.value)
sample1.2 <- c(focus_ads1.p$p.value, spiegel_ads1.p$p.value, stern_ads1.p$p.value)
sample2.1 <- c(focus_sales2.p$p.value, spiegel_sales2.p$p.value, stern_sales2.p$p.value)
sample2.2 <- c(focus_ads2.p$p.value, spiegel_ads2.p$p.value, stern_ads2.p$p.value)

t_pperron <- rbind("2004-2006",sample1.1,sample1.2,"2013-2015",sample2.1,sample2.2)

colnames(t_pperron) <- c("FOCUS", "Der Spiegel","Stern")
rownames(t_pperron) <- c("Sample","sales","ad pages","Sample","sales","ad pages")
 
t_unitroot <- stargazer(t_pperron, summary = FALSE, digits = 1, title = "", label = "t_unitroot")
 
writeLines(t_unitroot, con = "doc/tables/t_pperron.tex")

# ALTERNATIVE PLOTTING 
# ---- Reader Market
pdf('figs/sales_fss1.pdf')
plot((fss_sales.xts1[,"sales_FOCUS"])/1000, main="", ylim=c(400,1200), ylab = "Total Sales in tsd.")
lines((fss_sales.xts1[,"sales_Der Spiegel"])/1000, col="red")
lines((fss_sales.xts1[,"sales_Stern"])/1000, col="blue")
dev.off()

pdf('figs/sales_fss2.pdf')
plot((fss_sales.xts2[,"focus"])/1000, main="", ylim=c(400,1200), yaxt="n")
lines((fss_sales.xts2[,"spiegel"])/1000, col="red")
lines((fss_sales.xts2[,"stern"])/1000, col="blue")
legend(x="topleft", legend = c("FOCUS", "Der Spiegel", "Stern"), lty=1, col = c("black", "red", "blue"))
dev.off()

# ---- Ad Market 
pdf('figs/ads_fss1.pdf')
plot(fss_ads.xts1[,"focus"], main="", ylim=c(20,150), ylab = "Total advertising pages")
lines(fss_ads.xts1[,"spiegel"], col="red")
lines(fss_ads.xts1[,"stern"], col="blue")
dev.off()

pdf('figs/ads_fss2.pdf')
plot(fss_ads.xts2[,"focus"], main="", ylim=c(10,150), yaxt="n")
lines(fss_ads.xts2[,"spiegel"], col="red")
lines(fss_ads.xts2[,"stern"], col="blue")
legend(x="topleft", legend = c("FOCUS", "Der Spiegel", "Stern"), lty=1, col = c("black", "red", "blue"))
dev.off()





