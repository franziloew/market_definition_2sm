
###############################################################################
#
#     Entschuldige bitte den uneleganten Code ... habe es nicht mehr 
#     überarbeitet. Das hier ist ein Auszug mit dem hierarchical 
#     clustering.
#
#     RELEVANT SIND EIG. NUR DIE ZEILEN AB 212!
#
#     Wir haben mit Korrelation als Distanzmatrix gearbeitet. Es gibt auch 
#     andere Möglichkeiten. Wichtig ist eben nur, dass man dieses Ma?
#     als Matrix vorliegen hat. 
#     Wenn ich Meli richtig verstanden habe, dann würde ich Dir aber auch 
#     dazu raten sich mal Random Forrest Classification Methods anzuschauen.
#     Das soll auch super funktionieren wenn man eben mehrere Komponenten
#     hat nach denen man "Klassifizieren" möchte.
#
###############################################################################

rm (list = ls(all=TRUE))

# loading required packages:

library(ggplot2)
library(ggmap)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(zoo)
library(vars)
library(spatstat)
library(data.table)
library(Hmisc)
library(stargazer)
#library(MSwM)

#============================================================================


# Loading the data into R:

df_info <- read.csv("C:/Users/Matthias/Documents/Daten/Benzinpreise/gas_station2.csv", sep = ";", header = T)
str(df_info)
df_prices.36.raw <- read.csv(
  "C:/Users/Matthias/Documents/Daten/Benzinpreise/Projekt ML Marktabgrenzung/prices_36.csv",
  sep = ",", header = T, na.strings = "-1")
df_prices.36.raw$X <- NULL
df_prices.36.raw$X.1 <- NULL

#============================================================================
# we need a new ID:
df_info.36 <- df_info[which(df_info$id%in%df_prices.36.raw$stid),]
df_info.36 <- df_info.36[c(1,5,6,7,8,9,11,12)]
df_info.36 <- dplyr::rename(df_info.36, stid=id)
df_info.36$id <- seq.int(nrow(df_info.36))

df_prices.36.raw <- merge(df_prices.36.raw, df_info.36[c("stid", "id")], by="stid")
df_prices.36.raw <- df_prices.36.raw[c(2,10,11)]

df_prices.36.raw$e5[df_prices.36.raw$e5 < 1100] <- NA
df_prices.36.raw$e5[df_prices.36.raw$e5 > 2000] <- NA

df_prices.36.raw$datetime <- as.POSIXct(as.character(df_prices.36.raw$datetime), format='%Y-%m-%d %H:%M:%S')
df_prices.36.raw["newtime"] <- floor_date(df_prices.36.raw$datetime, "1 hour")
df_prices.36 <- aggregate(df_prices.36.raw$e5, by=list(df_prices.36.raw$id, df_prices.36.raw$newtime), FUN=mean, na.rm=TRUE)
df_prices.36 <- dplyr::rename(df_prices.36, id=Group.1)
df_prices.36 <- dplyr::rename(df_prices.36, date=Group.2)
df_prices.36 <- dplyr::rename(df_prices.36, price=x)

# Reshape to get price matrix

df_prices.36.reshaped <- reshape(data = df_prices.36,
                                 idvar = "date",
                                 v.names = c("price"),
                                 timevar = "id",
                                 direction = "wide")

# df_prices.36.reshaped[c(2:187)][df_prices.36.reshaped[c(2:187)]<1100] <- NA
# df_prices.36.reshaped[c(2:187)][df_prices.36.reshaped[c(2:187)]==9999] <- NA

# Fill time series with missing dates (in 60 minute-steps):

df_prices.36.reshaped <- df_prices.36.reshaped[order(df_prices.36.reshaped$date),]
time.length <- length(df_prices.36.reshaped$date)
time.min <- df_prices.36.reshaped$date[1]
time.max <- df_prices.36.reshaped$date[time.length]
full.dates <- seq(time.min, time.max, by="1 hour")
df_full.dates <- data.frame(list(date=full.dates))
df_prices.36.reshaped <- merge(df_full.dates, df_prices.36.reshaped, all=T)
rm(time.min, time.max, time.length, full.dates)

# Fill prices with the last price:

for(i in names(df_prices.36.reshaped)){
  df_prices.36.reshaped[i] <- na.locf(df_prices.36.reshaped[i], na.rm = F)
}

df_prices.36.reshaped$newtime <- as.Date(as.character(df_prices.36.reshaped$date), format='%Y-%m-%d')

# Aggregate back to daily prices:
df_prices.hourly <- aggregate(df_prices.36.reshaped[c(2:187)], by=list(df_prices.36.reshaped$newtime), FUN=mean, na.rm=TRUE)
df_prices.hourly <- dplyr::rename(df_prices.hourly, date=Group.1)
df_prices.hourly$date <- as.POSIXct(as.character(df_prices.hourly$date ), format='%Y-%m-%d')

#============================================================================
# Clean from common factor
# and create log-returns

# Load common factors - crude oil and exchange rate (federal reserve st. louis)

df_brent <- read.csv("C:/Users/Matthias/Documents/Daten/Benzinpreise/fred_brentcrude.csv", sep = ",", header = T)
df_brent <- dplyr::rename(df_brent, date = DATE)
df_brent <- dplyr::rename(df_brent, crude = DCOILBRENTEU)
df_brent$date <- as.POSIXct(as.character(df_brent$date), format='%Y-%m-%d')


df_exr <- read.csv("C:/Users/Matthias/Documents/Daten/Benzinpreise/fred_exrate_useuro.csv", sep = ",", header = T)
df_exr <- dplyr::rename(df_exr, date = DATE)
df_exr <- dplyr::rename(df_exr, exrate = DEXUSEU)
df_exr$date <- as.POSIXct(as.character(df_exr$date), format='%Y-%m-%d')


# Merge crude oil and exchange rates and keep observations between June 2014 - June 2017

df_brent.exr <- left_join(df_brent, df_exr, by=c("date"))
time.min <- df_brent.exr$date[1]
time.max <- df_brent.exr$date[length(df_brent.exr$date)]
full.dates <- seq(time.min, time.max, by="day")
df_full.dates <- data.frame(list(time=full.dates))
df_full.dates$time <- as.POSIXct(as.character(df_full.dates$time), format='%Y-%m-%d')
df_full.dates <- dplyr::rename(df_full.dates, date=time)
df_brent.exr <- merge(df_full.dates, df_brent.exr, all=T)
df_brent.exr[c(2,3)] <- na.locf(df_brent.exr[c(2,3)])
df_brent.exr[c(2,3)] <- na.locf(with(df_brent.exr, { 
  is.na(df_brent.exr[c(2,3)]) <- df_brent.exr[c(2,3)] == 0; 
  df_brent.exr[c(2,3)]}), fromLast = TRUE)
df_brent.exr[c(2,3)] <- na.locf(with(df_brent.exr, { 
  is.na(df_brent.exr[c(2,3)]) <- df_brent.exr[c(2,3)] == "."; 
  df_brent.exr[c(2,3)]}), fromLast = TRUE)
rm(time.min, time.max, full.dates, df_full.dates)

df_brent.exr$crude <- as.numeric(df_brent.exr$crude)
df_brent.exr$exrate <- as.numeric(df_brent.exr$exrate)
df_brent.exr$brent <- df_brent.exr$crude / df_brent.exr$exrate

# Convert BBL into liters: 1 bbl = 158,987294928 Liter
df_brent.exr$brent <- df_brent.exr$brent / 158.987294928

# Merge with price series:

df_prices.36.reshaped.merged <- merge(df_prices.hourly, 
                                      df_brent.exr, by="date")
df_prices.36.reshaped.merged <- df_prices.36.reshaped.merged[order(
  df_prices.36.reshaped.merged$date),]

# log returns:
df_lret.prices.36 <- as.data.frame(sapply(df_prices.36.reshaped.merged[c(2:187, 190)], function(x) diff(log(x))))

# Adding the dates (with one lag)
df_lret.prices.36 <- cbind(df_prices.36.reshaped.merged$date[
  2:length(df_prices.36.reshaped.merged$date)], df_lret.prices.36)
df_lret.prices.36 <- dplyr::rename(df_lret.prices.36, date = `df_prices.36.reshaped.merged$date[2:length(df_prices.36.reshaped.merged$date)]`)



# Cleaning up the prices:

lm_f <- function(x) {
  x <- residuals(lm(data = df_lret.prices.36, x  ~ brent + lag(brent) + lag(brent, 2) + lag(brent, 3) -1))
}
res <- apply(df_lret.prices.36[, 2:187], 2, lm_f)

df_clean.prices <- data.frame(df_lret.prices.36$date)
for(i in names(res)){
  newCol <- paste(i, 'res', sep = '')
  df_clean.prices[[newCol]] <- NA
  df_clean.prices[[newCol]][as.integer(names(res[[i]]))] <- res[[i]]
}

#============================================================================
# Creating graphs and tables: Sample

# getting a map of Stuttgart
mapstgt <- get_map(location = "Kornwestheim", zoom = 10, maptype = "roadmap", color = "bw")
sample.graph <- ggmap(mapstgt) + geom_point(data = df_info.36, aes(x = lng, y = lat)) +
  labs(x = 'Longitude', y = 'Latitude')

pdf(file = "C:/Users/Matthias/Documents/Projekt Price Tests Gasoline/aktuellste Version/graphs2/sample.pdf", width = 5, height = 5)
par(mar=c(1.1,1.1,1.1,1.1))
plot(sample.graph)
dev.off()


# setEPS()
#  postscript("C:/Users/Matthias/Documents/Projekt Price Tests Gasoline/
#            M?rz 2018/Zwischenstand/HOS 50 Version/graphs/sample.eps")
# plot(sample.graph)
#  dev.off()

# Descriptives:
#brands
df_info.36$brand <- as.character(df_info.36$brand)
table.brands <- count(df_info.36, 'brand')
table.brands <- table.brands[order(-table.brands$freq),]

#prices (original)
describe(df_prices.36$price)
sd(df_prices.36$price, na.rm = T)

#============================================================================

# Creating correlation matrix:
df_36 <- df_clean.prices[,2:187]
df_36.corr <- round(cor(df_36, use = "complete.obs"),4)

# as a measure of distance
df_36.dis <- 1 - abs(df_36.corr)
df_distance <- as.dist(df_36.dis)

# hierarchical clustering
clustering.36 <- hclust(df_distance, method = "complete")



# Dendrogram
par(cex=0.3, mar=c(5, 8, 4, 1)) # reduced label size
plot(clustering.36, main="Durchschnittspreis", xlab="")

pdf(file = "C:/Users/Matthias/Documents/Projekt Price Tests Gasoline/aktuellste Version/graphs2/dendro_mean.pdf", width = 12, height = 8)
par(cex=0.3, mar=c(2.1,3.1,6.1,1.1))
plot(clustering.36, xlab="", main="")
#plot(clustering.36, main="Daily Average Prices", xlab="")
dev.off()


# extract dendrogram as eps:
#    setEPS()
#    postscript("C:/Users/Matthias/Documents/Projekt Price Tests Gasoline/M?rz 2018/Zwischenstand/HOS 50 Version/graphs/dendro_mean.eps")
#    par(cex=0.3, mar=c(5, 8, 4, 1)) # reduced label size
#    plot(clustering.36, main="Daily Average Prices", xlab="")
#    dev.off()

# CUT at height = 0.4
par(cex=0.35, mar=c(5, 8, 4, 1)) # reduced label size
plot(clustering.36, main="Durchschnittspreis -- Cut at height = 0.4))", xlab="")
rect.hclust(clustering.36, h = 0.4)

# CUT at height = 0.6
par(cex=0.35, mar=c(5, 8, 4, 1)) # reduced label size
plot(clustering.36, main="Durchschnittspreis -- Cut at height = 0.6))", xlab="")
rect.hclust(clustering.36, h = 0.6)


# Cut at h = 0.4 and identify each cluster:
par(cex=0.5, mar=c(5, 8, 4, 1)) # reduced label size
cut04 <- cutree(clustering.36, h = 0.4)
df_info.04 <- as.data.frame(cut04)
df_info.04["rowname"] <- rownames(df_info.04)
df_info.04["id"] <- sub("*res", "", sub("price.", "", df_info.04$rowname))
df_info.04$rowname <- NULL

df_info.04 <- merge(df_info.04, df_info.36, by="id")

# Cut at h = 0.6 and identify each cluster:
par(cex=0.5, mar=c(5, 8, 4, 1)) # reduced label size
cut06 <- cutree(clustering.36, h = 0.6)
df_info.06 <- as.data.frame(cut06)
df_info.06["rowname"] <- rownames(df_info.06)
df_info.06["id"] <- sub("*res", "", sub("price.", "", df_info.06$rowname))
df_info.06$rowname <- NULL

df_info.06 <- merge(df_info.06, df_info.36, by="id")


# getting a map of Stuttgart
#mapstgt <- get_map(location = "Kornwestheim", zoom = 10, maptype = "roadmap", color = "bw")
# cut 0.4
ggmap(mapstgt) + geom_point(data = df_info.04, aes(x = lng, y = lat, color = cut04)) +
  scale_color_gradientn(colours = heat.colors(8))

# cut 0.6
map.mean.06 <- ggmap(mapstgt) + geom_point(data = df_info.06, aes(x = lng, y = lat, color = cut06)) +
  scale_color_gradientn(colours = heat.colors(8))

df_info.04 <- df_info.04[order(df_info.04$cut04),]

df_info.06 <- df_info.06[order(df_info.06$cut06),]


#########################################################################################
# Plotting only 1 or 2 Clusters:
#########################################################################################

# Cut h = 0.4:
clusters04 <- df_info.04 %>% count(cut04)
clusters04 <- clusters04[order(clusters04$cut04),]
# largest clusters:
# 7 with 17 stations
# 14 with 17 stations
# 21 with 14 stations
# 22 with 14 stations
# 17 with 13 stations
# 19 with 11 stations
# 26 with 11 stations

map.mean.04 <- ggmap(mapstgt) + 
  geom_point(data = df_info.04[which(df_info.04$cut04==7),], 
             aes(x = lng, y = lat, color = "cluster 7"), shape=15, size=1.5) +
  geom_point(data = df_info.04[which(df_info.04$cut04==14),], 
             aes(x = lng, y = lat, color = "cluster 14"), shape=16, size=1.5) +
  geom_point(data = df_info.04[which(df_info.04$cut04==21),], 
             aes(x = lng, y = lat, color = "cluster 21"), shape=17, size=1.5) +
  geom_point(data = df_info.04[which(df_info.04$cut04==22),], 
             aes(x = lng, y = lat, color = "cluster 22"), shape=18, size=1.5) +
  geom_point(data = df_info.04[which(df_info.04$cut04==17),], 
             aes(x = lng, y = lat, color = "cluster 17"), shape=19, size=1.5) +
  geom_point(data = df_info.04[which(df_info.04$cut04==19),], 
             aes(x = lng, y = lat, color = "cluster 19"), shape=12, size=1.5) +
  geom_point(data = df_info.04[which(df_info.04$cut04==26),], 
             aes(x = lng, y = lat, color = "cluster 26"), shape=13, size=1.5) +
  labs(x = 'Longitude', y = 'Latitude')


pdf(file = "C:/Users/Matthias/Documents/Projekt Price Tests Gasoline/aktuellste Version/graphs2/map_mean_04.pdf", width = 5, height = 5)
par(mar=c(1.1,1.1,1.1,1.1))
plot(map.mean.04)
dev.off()