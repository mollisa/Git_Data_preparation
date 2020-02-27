# Clean the Vegetation Data for Analysis

# extension for actual raw data 24.02.2020

# Date: 05.12.2019
# Author: Elisabeth Sellenriek
# Description:
# names especially of the sites are changed in to comparable formats without Umlaute
# survey Cover-classes are transformed to midpoint percentage cover
# R version 3.5.1 (2018-07-02), platform x86_64-w64-mingw32

library(tidyr)
library(dplyr)

########################################################################
# FUNCTION, which takes only plants with Coverages more than class 1 and creates midpoint percentages from classes in a new column
cvr.cnvrt <- function(Data){
  Data <- filter(Data, cover.c!=1)
  Data$cover.p <- ifelse(Data$cover.c ==2, "0.1",
                         ifelse(Data$cover.c ==3,"2.5",
                                ifelse(Data$cover.c ==4, "15.5",
                                       ifelse(Data$cover.c ==5, "37.5",
                                              ifelse(Data$cover.c ==6, "62.5",
                                                     ifelse(Data$cover.c ==7, "87.5","unclear"))))))
  
  
  return(Data)
}
################################################################
# Correct the names of identifiers

# Read in the survey data
# setwd("C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation")
Dat <- read.table("C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Plants/VegSurveys_Plantnames_corrected.csv", sep = ";", header = T) #Plant names are correct but not all match in TRY
#Dat <- read.csv2("C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Plants/VegSurveys_Plantnames_matching_TRY.csv")

# # get original database excerpt (files from Alex)
# Zoeb <- read.csv2("C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/ORIGINAL/Export_20190606_zoeb.csv")
# Rest <- read.csv2("C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/ORIGINAL/tmp002.csv")
# ORIGINAL <- rbind(Rest,Zoeb)
# Dat <- ORIGINAL


Dat$site <- Dat$MAINLOCATION
Dat$site <- gsub("Bad Lauchst?dt","BL",Dat$site)
Dat$site <- gsub("Bad Lauchstädt","BL",Dat$site)
Dat$site <- gsub("Bayreuth","BR",Dat$site)
Dat$site <- gsub("Gimritz","GR",Dat$site)
Dat$site <- gsub("Zoeberitz","ZR",Dat$site)
Dat$site <- as.factor(Dat$site)

# gsub doesn??t work for special characters like ? and space
# Even Mick Wu did?t know why
# 
# Dat$location <- gsub("Acker ged\u0081ngt (AD)","AD",Dat$location)
# Dat$location <- gsub("Acker unged\u0081ngt (AU)","AU",Dat$location)
# Dat$location <- gsub("Fr?hjahr","F",Dat$location)
# Dat$location <- gsub("Gimritz 10x10","10x10",Dat$location)
# Dat$location <- gsub("Gr\u0081nland  ged\u0081ngt (GD)", "GD",Dat$location)
# Dat$location <- gsub("Gr\u0081nland  unged\u0081ngt (GU)","GU",Dat$location)
# Dat$location <- gsub("Herbstbrache 10x10","H_10x10",Dat$location)
# Dat$location <- gsub("Kontrolle (K)" ,"K",Dat$location)
# Dat$location <- gsub("Herbst","H",Dat$location)
# Dat$location <- gsub("Sommer","S",Dat$location)
unique(Dat$LOCATION_LABEL)
Dat$location <- ifelse(Dat$LOCATION_LABEL=="Acker ged\u0081ngt (AD)","AD",
                  ifelse(Dat$LOCATION_LABEL=="Acker unged\u0081ngt (AU)","AU",
                    ifelse(Dat$LOCATION_LABEL=="Fr?hjahr","F",
                     ifelse(Dat$LOCATION_LABEL=="Frühjahr","F",
                      ifelse(Dat$LOCATION_LABEL=="Gimritz 10x10","10x10",
                        ifelse(Dat$LOCATION_LABEL=="Gr\u0081nland  ged\u0081ngt (GD)", "GD",
                          ifelse(Dat$LOCATION_LABEL=="Gr\u0081nland  unged\u0081ngt (GU)","GU",
                            ifelse(Dat$LOCATION_LABEL=="Herbstbrache 10x10","H_10x10",
                              ifelse(Dat$LOCATION_LABEL=="Kontrolle (K)" ,"K",
                                ifelse(Dat$LOCATION_LABEL=="Herbst","H",
                                  ifelse(Dat$LOCATION_LABEL=="Sommer","S",
                                    "none")))))))))))
Dat$location <- as.factor(Dat$location)
unique(Dat$location)
str(Dat)
# unique(Dat$LOCATION_LABEL2) I don?t know what the second label means
Dat$plot <- Dat$PLOT_ID
Dat <- Dat %>% group_by(site,location) %>% mutate(time=YEAR_FROM-min(YEAR_FROM)+1)
Dat %>% group_by(site,location) %>% summarise(NPlots=n_distinct(plot))
Dat$year <- Dat$YEAR_FROM
    # str(Dat)
    # levels(Dat$site)
    # levels(Dat$location)

# write.table(Dat, "SurveyData_1.csv", sep = ";", row.names = F)

Dat1 <- Dat %>% group_by(site,location,plot,spec,time,year) %>% summarise(cover.c=max(cover.c)) # BL is measured on different levels -> I need to summarize it to one cover per species per plot at each time
Dat2 <- cvr.cnvrt(as.data.frame(Dat1))

  #str(Dat2)
# write.table(Dat2, "SurveyData_locations_coverp.csv", sep = ";", row.names = F)

#================================
#Dat2 <- read.table("SurveyData_locations_coverp.csv", sep = ";", header=T)

Dat2 <- select(Dat2,  "site", "location", "plot", cover=cover.p, "spec", "time", "year" )

BL.all <- Dat2[Dat2$site=="BL",]
BL <- subset(BL.all, location=="H_10x10")
BR <- subset(Dat2,site=="BR")
GR.all <- subset(Dat2, site=="GR")
GR <- subset(GR.all, location=="10x10")
ZR.all <- subset(Dat2, site=="ZR")
BL <- droplevels(BL)
BR <- droplevels(BR)
GR <- droplevels(GR)
ZR.all <- droplevels(ZR.all)

# > str(ZR.all$plot)
# int [1:79247] 210 210 210 210 210 210 210 210 210 210 ...
# > length(unique(ZR.all$plot))
# [1] 400
# > range(ZR.all$plot)
# [1] 210 609

# ZR.all<- ZR.all %>% group_by(location) %>% mutate(rank = rank(plot, ties.method = "min")) %>% ungroup


ZR.location.index <- unique(ZR.all[c("site","location","plot")])
ZR.location.index$index <- rep(1:4, times=4, each=25)
ZR <- full_join(ZR.all,ZR.location.index)
ZR <- subset(ZR, location=="GU" | location=="AU")
ZR$location <- paste(ZR$location,ZR$index, sep = "_")
ZR <- ZR[,-8]
unique(ZR$location)

write.table(BL, "C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Plants/Survey_BL_clean.csv", sep = ";", row.names = F)
write.table(BR, "C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Plants/Survey_BR_clean.csv", sep = ";", row.names = F)
write.table(GR, "C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Plants/Survey_GR_clean.csv", sep = ";", row.names = F)
write.table(ZR, "C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Plants/Survey_ZR_clean.csv", sep = ";", row.names = F)

#adjusted names, so that they fit to TRY
# write.table(BL, "C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Plants/Survey_BL_clean_TRYnames.csv", sep = ";", row.names = F)
# write.table(BR, "C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Plants/Survey_BR_clean_TRYnames.csv", sep = ";", row.names = F)
# write.table(GR, "C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Plants/Survey_GR_clean_TRYnames.csv", sep = ";", row.names = F)
# write.table(ZR, "C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Plants/Survey_ZR_clean_TRYnames.csv", sep = ";", row.names = F)

#==========================
# spreaded data
# create matrices with species as variables

max(GR$time)
length(unique(GR$time))
sort(unique(GR$time))
plot <- c(unique(GR$plot))
year <- rep(2016,25)
time <- rep(19,25)
year.miss <- data.frame(plot,year,time)
GR.spread <- GR %>% group_by_at(vars(-cover)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%
  spread(key=spec, value=cover, fill = 0) %>%
  select(-row_id)%>%
  bind_rows(year.miss) %>% # 19th year was missing 2016, add a row of NA
  arrange(time) #reorder it according to the time

ZR.spread<- ZR %>% group_by_at(vars(-cover)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=spec, value=cover, fill = 0) %>%    # spread
  select(-row_id)  # drop the index
ZR_AU1.spread <- filter(ZR.spread, location=="AU_1") 
ZR_AU2.spread  <- filter(ZR.spread, location=="AU_2") 
ZR_AU3.spread  <- filter(ZR.spread, location=="AU_3")
ZR_AU4.spread  <- filter(ZR.spread, location=="AU_4") 
ZR_GU1.spread  <- filter(ZR.spread, location=="GU_1")
ZR_GU2.spread  <- filter(ZR.spread, location=="GU_2")
ZR_GU3.spread  <- filter(ZR.spread, location=="GU_3")
ZR_GU4.spread  <- filter(ZR.spread, location=="GU_4")


#BR<- BR.raw[,-c(2,1,6)] %>% arrange(time) %>% spread(spec,cover, fill = 0) #maybe there are again double entries? running TBI over all times gives the error "Matrices not of same length
#GR<- GR.raw[,-c(2,1,6)] %>% spread(spec,cover, fill = 0) %>% bind_rows(year.miss) %>% arrange(time) # 19th year was missing 2016, add a row of NA

plot <- c(unique(BR$plot))
year <- rep(2011,25)
time <- rep(18,25)
year.miss <- data.frame(plot,year,time)
BR.spread <- BR%>% group_by_at(vars(-cover)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=spec, value=cover, fill = 0) %>%    # spread
  select(-row_id)%>%
  bind_rows(year.miss) %>% # 18th year was missing 2011, add rows of NA
  arrange(time)

BL.spread <- BL %>% group_by_at(vars(-cover)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=spec, value=cover, fill = 0) %>%    # spread
  select(-row_id)  # drop the index

# #write csv-files to use matrices later again
# write.csv(BL.spread,"BL_survey_spread.txt")
# write.csv(BR.spread,"BR_survey_spread.txt")
# write.csv(GR.spread,"GR_survey_spread.txt")
# write.csv(ZR_AU1.spread ,"ZR_AU1_survey_spread.txt")
# write.csv(ZR_AU2.spread ,"ZR_AU2_survey_spread.txt")
# write.csv(ZR_AU3.spread ,"ZR_AU3_survey_spread.txt")
# write.csv(ZR_AU4.spread ,"ZR_AU4_survey_spread.txt")
# write.csv(ZR_GU1.spread ,"ZR_GU1_survey_spread.txt")
# write.csv(ZR_GU2.spread ,"ZR_GU2_survey_spread.txt")
# write.csv(ZR_GU3.spread ,"ZR_GU3_survey_spread.txt")
# write.csv(ZR_GU4.spread ,"ZR_GU4_survey_spread.txt")

