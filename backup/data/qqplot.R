### loading packages
library(gplots)
library(lattice)
library(R.matlab)
library("plot3D")
library(RColorBrewer)

source("R/functions.R")

setwd("~/GD/hsu/paper/2sm")
# load data
rm(list = ls())
# Load 3d Matrix
qq <- readMat('simulation/qq.mat')
qq <- qq$qq

colnames(qq) <- c('0','.1','.2','.3','.4','.5','.6','.7','.8','.9','1')
row.names(qq)<-  c('0','.1','.2','.3','.4','.5','.6','.7','.8','.9','1')

#Load Diagonal
qqdiag <- readMat('simulation/qqdiag.mat')
qqdiag <- qqdiag$qqdiag

colnames(qqdiag) <- c('0','.1','.2','.3','.4','.5','.6','.7','.8','.9','1')
row.names(qqdiag)<-  c('0','.2','.4','.6','.8','1','1.2','1.4','1.6','1.8','2')



abline(h = c(-.2), col = c("grey"), lwd=c(1), lty=c(1))
abline(h = c(-.3), col = c("grey"), lwd=c(1), lty=c(1))
abline(h = c(-.5), col = c("grey"), lwd=c(1), lty=c(1))
abline(h = c(-.6), col = c("grey"), lwd=c(1), lty=c(1))

abline(v = c(10), col = c("red"), lwd=c(1), lty=c(1))

dev.off()


# FSS Data
#tikz("/Users/Franzi/Desktop/R/QQ_fss.tex",width=6,height=5)

brewer.pal.info
colors <- brewer.pal(11, "Paired")

plot(QQ[,11], 
     pch = 1,
     type = "b",
     col = colors[1],
     lty = 1,
     lwd = 1,
     xlab = "d+g",
     ylab = "Correlation Coefficient",
     ylim = c(-1,.2),
     xlim = c(1,19), 
     xaxt = "n")
axis(1, at = 1:19, labels = c("-.9","-.8","-.7","-.6","-.5","-.4","-.3","-.2","-.1","0",".1",".2",".3",".4",".5",".6", ".7", ".8", ".9"))
lines(QQ[,10], type = "b", pch = 2, lty = 1, lwd = 1, col = colors[2])
lines(QQ[,9],  type = "b", pch = 3, lty = 1, lwd = 1, col = colors[3])
lines(QQ[,8],  type = "b", pch = 4, lty = 1, lwd = 1, col = colors[4])
lines(QQ[,7],  type = "b", pch = 5, lty = 1, lwd = 1, col = colors[5])
lines(QQ[,6],  type = "b", pch = 6, lty = 1, lwd = 1, col = colors[6])
lines(QQ[,5],  type = "b", pch = 7, lty = 1, lwd = 1, col = colors[7])
lines(QQ[,4],  type = "b", pch = 8, lty = 1, lwd = 1, col = colors[8])
lines(QQ[,3],  type = "b", pch = 9, lty = 1, lwd = 1, col = colors[9])
lines(QQ[,2],  type = "b", pch = 10, lty = 1, lwd = 1, col = colors[10])
lines(QQ[,1],  type = "b", pch = 11, lty = 1, lwd = 1, col = colors[11])
legend("topleft",
       c("$\theta=1$", "$\theta=.9$", "$\theta=.8$", "$\theta=.7$", "$\theta=.6$", "$\theta=.5$", "$\theta=.4$","$\theta=.3$", "$\theta=.2$", "$\theta=.1$", "$\theta=1$"),
       pch = 1:11,
       lty = c(1,1,1,1,1,1,1,1,1,1,1),
       lwd = c(1,1,1,1,1,1,1,1,1,1,1),
       col = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99'),
       bty = "n",
       ncol = 6,
       cex = .8)
abline(h = -.25, col = "red", lwd=1, lty=1)
#text(19,-.2, "r.m.", col = "red")
abline(h = -.56, col = "red", lwd=1, lty=1)

#abline(v = c(11,19), col = c("blue","blue"), lwd=c(1,1), lty=c(1,1))

dev.off()
