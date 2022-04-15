### loading packages
library(gplots)
library(lattice)
library(R.matlab)
library("plot3D")
library(RColorBrewer)
library(data.table)
library(ggplot2)
library(stargazer)
library(tikzDevice)
library(ggthemes)
library(plotrix)

source("R/functions.R")

setwd("~/GD/hsu/paper/2sm")
# load data
rm(list = ls())
# Load 3d Matrix
qq <- readMat('simulation/qq.mat')
qq <- qq$qq

colnames(qq) <- c('0','.05','.1','.15','.2','.25','.3','.35','.4','.45','.5')
row.names(qq) <- c('0','.05','.1','.15','.2','.25','.3','.35','.4','.45','.5')

#Load Diagonal
qqdiag <- readMat('simulation/qqdiag.mat')
qqdiag <- qqdiag$qqdiag

colnames(qqdiag) <- c('0', '.1','.2','.3','.4','.5','.6','.7','.8','.9','1')
row.names(qqdiag)<-  c('0','.1','.2','.3','.4','.5','.6','.7','.8','.9','1')

# Define Color palette
par(mfrow=c(1, 1))
display.brewer.all()

mypalette1 <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')
mypalette2 <- colorRampPalette(rev(brewer.pal(11, 'Paired')), space='Lab') 

# 3D Diagonal Matrix plots ---------
qqdiag.melt <- melt(qqdiag)

cc_matrix <- ggplot(qqdiag.melt, aes(x = Var1 , y = Var2, fill = value)) +  
  geom_raster() +
  coord_equal() +
  labs(title = "", x = "Sum of indirect network effects (d+g)", y = "Degree of Homogeneity", color = "") +
  scale_x_continuous(breaks = seq(0,1,.1), labels = c(seq(0,1,.1))) +
  scale_y_continuous(breaks = seq(0,1,.1), labels = c(seq(0,1,.1))) +
  scale_fill_gradientn(colours = mypalette1(100), name="Correlation\nCoefficient")

# safe figure
cc_matrix
ggsave('figs/qqmatrix.jpeg', width = 6, height = 6)

# 2D Plot ####
cc_plot <- ggplot(data=qqdiag.melt, aes(x=Var1, y=value, group=Var2, colour = Var2))+
  geom_line() +
  labs(title = "", x = "Sum of indirect network effects (d+g)", y = "Correlation Coefficient", color = "") +
  scale_x_continuous(breaks = seq(0,1,.1), labels = c(seq(0,1,.1))) +
  scale_y_continuous(breaks = seq(-1,0.3,.1), labels = c(seq(-1,0.3,.1))) +
  theme_minimal() +
  scale_colour_gradientn(colours = mypalette2(100), limits=c(0, 1)) +
  labs(colour="Degree of\nHomogeneity")

# safe figure
cc_plot
ggsave('figs/qqplot.jpeg', width = 7, height = 6)

# News Magazines ####
# sales: 
dss <- -.29 # Der Spiegel / Stern

cc_plot +
  # Der Spiegel / Stern
  geom_hline(aes(yintercept=dss), colour="grey25", linetype="dashed") + 
  geom_text(aes( 0, dss, label = "DS/S"), size = 3.5, color = "black") 
ggsave('figs/qq_sales_fss.jpeg', width = 5, height = 4)

# ad: 
fds <- -.553 # FOCUS / Der Spiegel
fs <- -.322  # FOCUS / Stern
dss <- -.379 # Der Spiegel / Stern

cc_plot +
  # FOCUS / Der Spiegel
  geom_hline(aes(yintercept=fds), colour="grey25", linetype="dashed") + 
  geom_text(aes( 0, fds, label = "F/DS"), size = 3.5, color = "black") +
  # FOCUS / Stern
  geom_hline(aes(yintercept=fs), colour="grey25", linetype="dashed") + 
  geom_text(aes( 1, fs, label = "F/S"), size = 3.5, color = "black") +
  # Der Spiegel / Stern
  geom_hline(aes(yintercept=dss), colour="grey25", linetype="dashed") + 
  geom_text(aes( 0, dss, label = "DS/S"), size = 3.5, color = "black") +
  theme(legend.position="none")
ggsave('figs/qq_ads_fss.jpeg', width = 4, height = 4)

# Print Matrix ####
t_qq <- stargazer(qq, summary = FALSE)

log_base <- AddAdjustBox(log_base)
writeLines(log_base, con = "doc/tables/t_log_base.tex")
