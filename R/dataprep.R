library(readxl)
library(plyr)
library(xts)
library(tidyr)
library(broom)
library(magrittr)
library(psych)
library(tseries)
library(data.table)

rm(list = ls())
setwd("~/CloudStation (Shared)/Projekte/Marktabgrezung 2SM")

### Import Data ####
# (1) Sales
sales <- data.frame(read_excel("data/sales.xls"))
sales <- plyr::rename(sales, c("Titel"="titel", "Verkauf.Gesamt"="sales", "Heft.Nr."="edition","Verlag"="publisher","Gattung"="segment","Verbreitung.Gesamt"="circulation"))

# (2) Ads + Content
ads <- read_excel("data/ads.xls")
ads <- plyr::rename(ads, c("Titel"="titel",`Umsatz Brutto TEUR`="revenue", `Heft Nr.`="edition", Heftumfang="content", `Anzeigen Gesamt`="ads"))

# (3) Adsite
adprice <- read_excel("data/adprice.xls")
adprice <- plyr::rename(adprice, c("Titel"="titel",`Heft Nr.`="edition", `Preis 4C 1/1`="adprice"))

# Drop variables we do not need
drops <- c("Status","Preisliste Nr.", "Preisliste Gültig ab","Anzeigen Seiten","Anzeigen Beihefter")
sales <- sales[,!(names(sales)%in%drops)]
ads <- ads[,!(names(ads)%in%drops)]
adprice <- adprice[,!(names(adprice)%in%drops)]

#merge data
magazines <- merge(adprice, ads, by=c("titel","edition"), all=TRUE)
magazines <- merge(magazines, sales, by=c("titel","edition"), all = TRUE)

save(list = ls(), file = "output/magazines.Rda")

# (1) News Market ----
fss_long <- magazines[which(magazines$titel=="Der Spiegel" | magazines$titel == "Stern" | magazines$titel == "FOCUS"),]

# Reshape dataframe
fss <- dcast(setDT(fss_long), edition ~ titel, value.var = c("adprice","revenue", "content","ads","segment","publisher","sales","circulation"))

# Time variable
week <- as.numeric(substr(fss$edition,1,2))
year <- as.numeric(substr(fss$edition,5,9))
fss$date <- as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u")

fss<-fss[!is.na(fss$date),]
date<-fss$date

# select subsamples
# 1
Date1<-as.Date("2004-08-18")  
Date2<-as.Date("2006-08-18")
fss1<-fss[which(fss$date>=Date1 | fss$date<=Date2),]

t.fss1.sum <- stargazer(fss1 %>% dplyr:: select(grep("sales", names(fss)), grep("ads", names(fss))), 
                       title = "Summary Statistics: Sample 1",
                       label = "t_sum_fss",
                       header = FALSE,
                       align = FALSE)

#writeLines(t.fss1.sum, con = "doc/tables/t_fss1_sum.tex")

# 2
Date3<-as.Date("2013-08-18")  
Date4<-as.Date("2015-08-18")
fss2<-fss[which(fss$date>=Date3 | fss$date<=Date4),]

#t.fss2.sum <- stargazer(fss2 %>% dplyr:: select(grep("sales", names(fss)), grep("ads", names(fss))), 
#                        title = "Summary Statistics: Sample 2",
#                        label = "t_sum_fss",
#                        header = FALSE,
#                        align = FALSE)
#writeLines(t.fss2.sum, con = "doc/tables/t_fss2_sum.tex")

# Change to xts object
fss_sales <- fss %>% dplyr:: select(grep("sales", names(fss)))
fss_ads <- fss %>% dplyr:: select(grep("ads", names(fss)))

# rename variables
names(fss_sales) <- sub(".*_", "", names(fss_sales))
names(fss_ads) <- sub(".*_", "", names(fss_ads))

fss_sales.xts<- xts(x=fss_sales, order.by = date)
fss_ads.xts<- xts(x=fss_ads, order.by = date)

# Select Sub-Sample
# 2004w33 - 2006w33 
fss_sales.xts1 <- fss_sales.xts["2004-08-18/2006-08-18"]
fss_ads.xts1 <- fss_ads.xts["2004-08-18/2006-08-18"]

# 2013w33 - 2015w33 
fss_sales.xts2 <- fss_sales.xts["2013-08-18/2015-08-18"]
fss_ads.xts2 <- fss_ads.xts["2013-08-18/2015-08-18"]

rm(adprice,ads,sales,drops,week,year)

save(list = ls(), file = "output/fss.Rda")

# (2) Program Guides  ----
rm(list=ls())
load("output/magazines.Rda")

tv_long <- magazines[which(magazines$titel=="TV Movie" | magazines$titel == "TV Spielfilm" | magazines$titel == "TV Today" | magazines$titel == "TV Digital"),]

# Reshape dataframe
tv <- dcast(setDT(tv_long), edition ~ titel, value.var = c("adprice","revenue", "content","ads","segment","publisher","sales","circulation"))

# Time variable
tv$week <- as.numeric(substr(tv$edition,1,2))
tv$year <- as.numeric(substr(tv$edition,5,9))
tv <- tv[order(tv$year,tv$week),]

# select subsamples
# Change to xts object
tv_sales <- tv %>% dplyr:: select(grep("sales", names(tv)))
names(tv_sales) <- sub(".*_", "", names(tv_sales))
tv_sales <- ts(tv_sales, start=c(2004,1), end = c(2017,1), frequency = 26)
tv_sales_sub <- window(tv_sales, start=c(2007,15), end = c(2011,15), frequency = 26)

tv_ads <- tv %>% dplyr:: select(grep("ads", names(tv)))
names(tv_ads) <- sub(".*_", "", names(tv_ads))
tv_ads <- ts(tv_ads, start=c(2004,1), end = c(2017,1), frequency = 26)
tv_ads_sub <- window(tv_ads, start=c(2007,15), end = c(2011,15), frequency = 26)

rm(adprice,ads,sales,drops)

save(list = ls(), file = "output/tv.Rda")

# (3) Womens Magazines  ----
rm(list=ls())
load("output/magazines.Rda")

women_long <- magazines[which(magazines$titel=="Brigitte" | magazines$titel == "freundin" | magazines$titel == "FÜR SIE"),]

# Reshape dataframe
women <- dcast(setDT(women_long), edition ~ titel, value.var = c("adprice","revenue", "content","ads","segment","publisher","sales","circulation"))

# Time variable
women$week <- as.numeric(substr(women$edition,1,2))
women$year <- as.numeric(substr(women$edition,5,9))
women <- women[order(women$year,women$week),]
women <- women[!is.na(women$publisher_Brigitte)]

# select subsamples
# Change to xts object
women_sales <- women %>% dplyr:: select(grep("sales", names(women)))
names(women_sales) <- sub(".*_", "", names(women_sales))
women_sales <- ts(women_sales, start=c(2004,1), end = c(2017,1), frequency = 26)
women_sales_sub <- window(women_sales, start=c(2007,15), end = c(2011,15), frequency = 26)

women_ads <- women %>% dplyr:: select(grep("ads", names(women)))
names(women_ads) <- sub(".*_", "", names(women_ads))
women_ads <- ts(women_ads, start=c(2004,1), end = c(2017,1), frequency = 26)
women_ads_sub <- window(women_ads, start=c(2007,15), end = c(2011,15), frequency = 26)

save(list = ls(), file = "output/women.Rda")
