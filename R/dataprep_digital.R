library(data.table)
library(plyr)
library(tidyr)
library(xts)
library(stringr)

rm(list = ls())
setwd("~/CloudStation (Shared)/Projekte/Marktabgrezung 2SM")

# Import data ####
digital <- read.csv("data/ivwdata2.csv", stringsAsFactors=F)

# convert visits and pI to numeric values
digital[c("visits")] <- lapply(digital[c("visits")], function(x) as.numeric(gsub("\\.", "", x)))

# delete after 2014-05 (no data)
digital <- digital[!(digital$year==2014 & digital$month>5),]

# Time var
digital$date <- as.Date(paste(digital$month,1,digital$year, sep="-"), "%m-%d-%Y")

drops <- c("year","month")
digital <- digital[,!(names(digital)%in%drops)] 

# Save dataframe
save(list = ls(), file = "output/digital.Rda")

# (I) News Market ----
news_long <- digital[which(
# (1) Der Spiegel
  digital$title=="SPIEGEL ONLINE"
# (2) Stern
  | digital$title=="STERN Online"
  | digital$title=="stern.de"
# (3) Focus
  | digital$title=="FOCUS ONLINE"
# (4) Bild.de
  | digital$title=="Bild Online"
  | digital$title=="Bild.de"
# (5) N-TV 
  | digital$title=="n-tv online"
  | digital$title=="n-tv.de"
# (6) Die Welt
  | digital$title=="DIE WELT online"
  | digital$title=="WELT ONLINE"
  | digital$title=="DIE WELT"
# (7) Die Zeit
  | digital$title=="Zeit im Internet"
  | digital$title=="ZEIT online"
  | digital$title=="ZEIT ONLINE"
# (8) SZ
  | digital$title=="sueddeutsche.de"
  | digital$title=="Süddeutsche.de"
# (9) FAZ
  | digital$title=="FAZ.NET"),]

news_long$title <- gsub("STERN Online", "stern.de", news_long$title) 
news_long$title <- gsub("Bild Online", "Bild.de", news_long$title) 
news_long$title <- gsub("DIE WELT online", "DIE WELT", news_long$title)
news_long$title <- gsub("WELT ONLINE", "DIE WELT", news_long$title)
news_long$title <- gsub("Zeit im Internet", "ZEIT ONLINE", news_long$title)
news_long$title <- gsub("ZEIT online", "ZEIT ONLINE", news_long$title)
news_long$title <- gsub("Süddeutsche.de", "sueddeutsche.de", news_long$title)
news_long$title <- gsub("n-tv online", "n-tv.de", news_long$title)

# Reshape dataframe
news <- dcast(setDT(news_long), date ~ title, value.var = c("visits"))
date <- news$date
news <- news[,-1]

# Visits
# news_visits <- news %>% dplyr:: select(grep("visits", names(news)))

# rename variables
names(news) <- sub(".*_", "", names(news))

# convert to xts object 
news.xts <- xts(x=news, order.by = date)

# for calculation purposes, descale the data
news.xts <- news.xts/10000

# Save Dataframe
rm(drops,news_long)
save(list = ls(), file = "output/news.Rda")


