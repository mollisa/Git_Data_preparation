# Clean the vegetation-surveys and prepare them for later analyses
# 
# started version control via GIT 25.02.2020
# adjusted 24.02.2020
# Date: 28.11.2019
# Author: Elisabeth Mollenkopf
# R version 3.5.1 (2018-07-02), platform x86_64-w64-mingw32

# Description: 
# identifier for site and location are changed in to comparable formats without Umlaute
  # take care! Zöberitz contains 4 replicats per treatment, which are not identified seperately in the overall data (ORIGINAL, Dat and Dat1)! -> each location at site ZR is 20 by 20 m 
# 
# The vegetation-surveys contained typos, small mistakes and some old names.
# These were cleaned in consultation of Mr. Klotz and the original field-notes.
# survey Cover-classes are transformed to midpoint percentage cover
# the plantnames are checked using "The National Resolution Service" and "The Plant List" 
# 
# for each site a file with the survey data for the no-treatment locations is extracted (ZR locations are split into 4 replicats)
# no treatment locations are additionally wide formatted

# the outputs of the script are:
# Dat -> Surveys_ORIGINAL_with_corrections.csv
# Dat1 -> Surveys_corrected_simplified.csv only corrected columns were kept : site|location|plot|timestep|year|speciesname|midpoint percentage cover
# a file for each site´s control plots 10m x 10m in long and wide format

library(taxize) # for name resolution
library(dplyr)
library(tidyr)

########################################################################
# FUNCTION, which takes only plants with Coverages more than class 1 and creates midpoint percentages from classes in a new column
cvr.cnvrt <- function(Data){
  Data <- filter(Data, cover.c!=1)
  Data$cover.p <- ifelse(Data$cover.c ==2, 0.1,
                         ifelse(Data$cover.c ==3,2.5,
                                ifelse(Data$cover.c ==4, 15.5,
                                       ifelse(Data$cover.c ==5, 37.5,
                                              ifelse(Data$cover.c ==6, 62.5,
                                                     ifelse(Data$cover.c ==7, 87.5,
                                                            999)))))) #999 means that none of the classes applied, so the coverage is unclear
  
  
  return(Data)
}
################################################################

# get original database excerpt (files from Alex)
Zoeb <- read.csv2("C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/ORIGINAL/Export_20190606_zoeb.csv")
Rest <- read.csv2("C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/ORIGINAL/tmp002.csv")
ORIGINAL <- rbind(Rest,Zoeb)
Dat <- ORIGINAL

####### Correct labelling ####
# depending on the encoding of this script Umlaute need to be adjusted again!!! -> Bad Lauchstädt and Frühjar!
Dat$site <- Dat$MAINLOCATION
Dat$site <- gsub("Bad Lauchst?dt","BL",Dat$site)
Dat$site <- gsub("Bad Lauchst�dt","BL",Dat$site)
Dat$site <- gsub("Bayreuth","BR",Dat$site)
Dat$site <- gsub("Gimritz","GR",Dat$site)
Dat$site <- gsub("Zoeberitz","ZR",Dat$site)
Dat$site <- as.factor(Dat$site)
#unique(Dat$LOCATION_LABEL)
Dat$location <- ifelse(Dat$LOCATION_LABEL=="Acker ged\u0081ngt (AD)","AD",
                       ifelse(Dat$LOCATION_LABEL=="Acker unged\u0081ngt (AU)","AU",
                              ifelse(Dat$LOCATION_LABEL=="Fr?hjahr","F",
                                     ifelse(Dat$LOCATION_LABEL=="Fr�hjahr","F",
                                            ifelse(Dat$LOCATION_LABEL=="Gimritz 10x10","10x10",
                                                   ifelse(Dat$LOCATION_LABEL=="Gr\u0081nland  ged\u0081ngt (GD)", "GD",
                                                          ifelse(Dat$LOCATION_LABEL=="Gr\u0081nland  unged\u0081ngt (GU)","GU",
                                                                 ifelse(Dat$LOCATION_LABEL=="Herbstbrache 10x10","H_10x10",
                                                                        ifelse(Dat$LOCATION_LABEL=="Kontrolle (K)" ,"K",
                                                                               ifelse(Dat$LOCATION_LABEL=="Herbst","H",
                                                                                      ifelse(Dat$LOCATION_LABEL=="Sommer","S",
                                                                                             "none")))))))))))
Dat$location <- as.factor(Dat$location)
# unique(Dat$location)
# str(Dat)
# unique(Dat$LOCATION_LABEL2) I don?t know what the second label means
Dat$plot <- Dat$PLOT_ID

##### time and year ####
Dat <- Dat %>% group_by(site,location) %>% mutate(time=YEAR_FROM-min(YEAR_FROM)+1)
Dat %>% group_by(site,location) %>% summarise(NPlots=n_distinct(plot))
Dat$year <- Dat$YEAR_FROM

##### correction plant names ####
Dat$spec <- ORIGINAL$FULLNAME
FULLNAME.spec <- ORIGINAL[grep("spec", ORIGINAL$FULLNAME),]
length(unique(FULLNAME.spec$FULLNAME))
Dat$spec <- gsub("Crataegus spec", "Crataegus monogyna",Dat$spec)
Dat$spec <- gsub("Acer spec", "Acer tataricum",Dat$spec)
Dat$spec <- gsub("Cerastium spec", "Cerastium arvense",Dat$spec)
Dat$spec <- gsub("Prunus spec", "Prunus domestica",Dat$spec)
Dat$spec <- gsub("Tilia spec", "Tilia cordata",Dat$spec)
Dat$spec <- gsub("Rubus spec", "Rubus caesius",Dat$spec)

    # species.list <- unique(Dat$spec)
    # # use the national resolution service
    # FULLNAME.check.TNRS <- tnrs(species.list, source = "iPlant_TNRS") # all names with score <1 need to be checked
    # issues <- FULLNAME.check.TNRS[FULLNAME.check.TNRS$score != 1,]
    # issues$submittedname

#Dat$spec <- gsub("Galium x pomeranicum", "Galium pomeranicum", Dat$spec) # The plant list acccepts hybrids
Dat$spec <- gsub("Festuca laeviagata", "Festuca laevigata", Dat$spec)
Dat$spec <- gsub("Lotus pendunculatus", "Lotus pedunculatus", Dat$spec)
Dat$spec <- gsub("Inula conyzae", "Inula conyza", Dat$spec)
Dat$spec <- gsub("Atriplex postrata", "Atriplex prostrata", Dat$spec)
Dat$spec <- gsub("Tilia hollandica", "Tilia cordata", Dat$spec)
Dat$spec <- gsub("Dryopteris filix-femina", "Dryopteris filix-mas", Dat$spec)

##### correction cover ####
COVER.typos <- subset(ORIGINAL,!(COVER %in% c(1:7))) # checked in field recordings on paper
row.names(COVER.typos)
Dat$cover.c <- Dat$COVER
Dat$cover.c[[23578]] <- 1 # accidentally 9 instead 1,Torilis japonica, plot208, year 2007
Dat$cover.c[[74699]] <- 3 # Aethusia cynaperum, plot 224, year1988
Dat$cover.c[[86428]] <- 1 # Daucus carota, plot 224, year 1988
Dat$cover.c[[86433]] <- 1 # Acer, plot 577, Year 2004
# at least BL is measured on different levels -> I need to summarize it to one cover per species per plot at each time
Dat1 <- Dat %>% group_by(site,location,plot,time,year,spec) %>%
  summarise(cover.c=max(cover.c)) %>%
  ungroup %>%
  cvr.cnvrt()# keep only species with more than cover class 1 and transform classes to midpoint percentage cover
              # Dat1 <- cvr.cnvrt(as.data.frame(Dat1)) 

Dat2 <- Dat1 %>% filter(cover.p < 999) %>%
  group_by(site,location,plot,time) %>%
  mutate(cover.tot = sum(cover.p)) %>% 
  ungroup %>%
  mutate(w = round((cover.p/cover.tot),3))

write.csv(Dat, "Output/Plants/Surveys_ORIGINAL_with_corrections.csv", row.names = F)
write.csv(Dat1[,-7], "Output/Plants/Surveys_corrected_simplified.csv", row.names = F)
write.csv(Dat2, "Output/Plants/Surveys_corrected_simplified_weights.csv", row.names = F) # use this for analysis?

#================================
# split into one dataset per site and keep only the locations without treatment "control plots"

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

# ZR contains 4 locations, these need an identifier
ZR.location.index <- unique(ZR.all[c("site","location","plot")])
ZR.location.index$index <- rep(1:4, times=4, each=25)
ZR <- full_join(ZR.all,ZR.location.index)
ZR <- subset(ZR, location=="GU" | location=="AU")
ZR$location <- paste(ZR$location,ZR$index, sep = "_")
ZR <- ZR[,-11] #drop index
unique(ZR$location)

write.csv(BL, "Output/Plants/BL_notreatment_10x10.csv", row.names = F)
write.csv(BR, "Output/Plants/BR_notreatment_10x10.csv", row.names = F)
write.csv(GR, "Output/Plants/GR_notreatment_10x10.csv", row.names = F)
write.csv(ZR, "Output/Plants/ZR_notreatment_10x10.csv", row.names = F)

all_again <- BL %>% rbind(BR) %>% rbind(GR) %>% rbind(ZR)
write.csv(all_again, "Output/Plants/All_notreatment_10x10.csv", row.names = F) # USE THIS ???
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
GR.spread <- GR %>% select("site", "location", "plot",  "time", "year", "spec", "w" ) %>% group_by_at(vars(-w)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%
  spread(key=spec, value=w, fill = 0) %>%
  select(-row_id)%>%
  bind_rows(year.miss) %>% # 19th year was missing 2016, add a row of NA
  arrange(time) #reorder it according to the time

ZR.spread<- ZR %>% select("site", "location", "plot",  "time", "year", "spec", "w" ) %>% group_by_at(vars(-w)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=spec, value=w, fill = 0) %>%    # spread
  select(-row_id)  # drop the index
ZR_AU1.spread <- filter(ZR.spread, location=="AU_1") 
ZR_AU2.spread  <- filter(ZR.spread, location=="AU_2") 
ZR_AU3.spread  <- filter(ZR.spread, location=="AU_3")
ZR_AU4.spread  <- filter(ZR.spread, location=="AU_4") 
ZR_GU1.spread  <- filter(ZR.spread, location=="GU_1")
ZR_GU2.spread  <- filter(ZR.spread, location=="GU_2")
ZR_GU3.spread  <- filter(ZR.spread, location=="GU_3")
ZR_GU4.spread  <- filter(ZR.spread, location=="GU_4")


plot <- c(unique(BR$plot))
year <- rep(2011,25)
time <- rep(18,25)
year.miss <- data.frame(plot,year,time)
BR.spread <- BR %>% select("site", "location", "plot",  "time", "year", "spec", "w" ) %>% group_by_at(vars(-w)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=spec, value=w, fill = 0) %>%    # spread
  select(-row_id)%>%
  bind_rows(year.miss) %>% # 18th year was missing 2011, add rows of NA
  arrange(time)

BL.spread <- BL %>% select("site", "location", "plot",  "time", "year", "spec", "w" ) %>% group_by_at(vars(-w)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=spec, value=w, fill = 0) %>%    # spread
  select(-row_id)  # drop the index

write.csv(BL.spread, "Output/Plants/BL_notreatment_10x10_spec_perc_of_tot_cover.csv", row.names = F)
write.csv(BR.spread,"Output/Plants/BR_notreatment_10x10_spec_perc_of_tot_cover.csv", row.names = F)
write.csv(GR.spread,"Output/Plants/GR_notreatment_10x10_spec_perc_of_tot_cover.csv", row.names = F)
write.csv(ZR_AU1.spread ,"Output/Plants/ZR_AU1_notreatment_10x10_spec_perc_of_tot_cover.csv", row.names = F)
write.csv(ZR_AU2.spread ,"Output/Plants/ZR_AU2_notreatment_10x10_spec_perc_of_tot_cover.csv", row.names = F)
write.csv(ZR_AU3.spread ,"Output/Plants/ZR_AU3_notreatment_10x10_spec_perc_of_tot_cover.csv", row.names = F)
write.csv(ZR_AU4.spread ,"Output/Plants/ZR_AU4_notreatment_10x10_spec_perc_of_tot_cover.csv", row.names = F)
write.csv(ZR_GU1.spread ,"Output/Plants/ZR_GU1_notreatment_10x10_spec_perc_of_tot_cover.csv", row.names = F)
write.csv(ZR_GU2.spread ,"Output/Plants/ZR_GU2_notreatment_10x10_spec_perc_of_tot_cover.csv", row.names = F)
write.csv(ZR_GU3.spread ,"Output/Plants/ZR_GU3_notreatment_10x10_spec_perc_of_tot_cover.csv", row.names = F)
write.csv(ZR_GU4.spread ,"Output/Plants/ZR_GU4_notreatment_10x10_spec_perc_of_tot_cover.csv", row.names = F)


