library("graphics")
library("tikzDevice")
library("stargazer")
library(ggplot2)
library(lubridate)
library(ggthemes)
library(scales)
library(ggfortify)

# Market: News Magazines 
setwd("~/CloudStation (Shared)/Projekte/Marktabgrezung 2SM")
rm(list = ls())

load("output/women.Rda")

# (1) Plots ####
# ---- Reader Market
autoplot((women_sales)/1000, facets = FALSE)  +
  labs(title = "", x = "", y = "Sales in tsd.", color = "") +
  scale_color_hue(labels = c("Brigitte", "Für Sie", "Freundin")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
# adding sample region
  geom_vline(xintercept = 2007.15, linetype = "dashed", lwd=.5) +
  geom_vline(xintercept = 2011.15, linetype = "dashed", lwd=.5)

# save plot
ggsave("figs/sales_women.pdf",  width = 8, height = 6)

# --- Ad Market
autoplot(women_ads, facets = FALSE)  +
  labs(title = "", x = "", y = "Advertising Pages", color = "") +
  scale_color_hue(labels = c("Brigitte", "Für Sie", "Freundin")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  # adding sample region
  geom_vline(xintercept = 2007.15, linetype = "dashed", lwd=.5) +
  geom_vline(xintercept = 2011.15, linetype = "dashed", lwd=.5)

# save plot
ggsave("figs/ads_women.pdf",  width = 8, height = 6)


