# Clean and prepare LEDA-traits for later analysis
# Start with LHS

# started version control via GIT 26.02.2020
# adjusted 26.02.2020
# Date: 30.01.2020
# Author: Elisabeth Mollenkopf
# R version 3.5.1 (2018-07-02), platform x86_64-w64-mingw32

# Description:
# LEDA-traits SLA, Height and SeedMass are cleaned for duplicates and summarized via a weighted median per species

# Inputs:
# original txt-files downloaded from LEDA: SLA, Height and SeedMass 
# Outputs:
# csv files for SLA,Height, SeedMass containing always one value per species


library(dplyr)
library(spatstat) # needed for weighted median
library(tidyr)

###################################################
# FUNCTION to summarize numeric LEDA traits
prepare.LEDA.num <-function(Data){
  # get rid of duplicates
  Data$dupl.Index <- base::duplicated(Data)
  Data <- Data[Data$dupl.Index==F,]
  # get unit of measurement
  # try to find a smart way to include this as the new variable name in the format like :
  # SLA2 <- plyr::rename(SLA2, c("MedianValue" = "SLA..mm2.mg"))
  # MedianValue <- Dataname/name of trait..unitname
  Data$unit <- sub("single.value..", "", colnames(Data)[grepl("single.value.." , names(Data))])
  # this column is later discarded
  # rename columns
  colnames(Data)[grepl("single.value", names(Data))==T] <- "value"
  Data <- plyr::rename(Data, c("SBS.name" = "spec", "number.of.replicates" = "replicates"))
  Data$value <- round(Data$value, 2)
  # replicates contains NA, but if there is an entry it must at least be 1
  Data$replicates[is.na(Data$replicates)] <- 1
  
  # summarize per species
  Data <- Data %>% group_by(spec) %>%
    # mutate to keep all columns, summarise to get only one value per spec
    summarise(MedianValue = round(if(sum(replicates)<3){weighted.mean(value, replicates, na.rm=T)}
                                  #you can only calculate a median from more than 3 replicates
                                  else {weighted.median(value, replicates, na.rm=T)}),3)
  
  return(Data)
}
#########################################################

SLA <- read.csv2("C:/Users/sellenri/Nextcloud/Cloud/Data/LEDA_excerpt_21-06-19/SLA_und_geo.txt", sep = ";", dec=".",header = TRUE)
Height <- read.csv2("C:/Users/sellenri/Nextcloud/Cloud/Data/LEDA_excerpt_21-06-19/canopy_height.txt", sep = ";", dec=".",header = TRUE)
SeedMass <- read.csv2("C:/Users/sellenri/Nextcloud/Cloud/Data/LEDA_excerpt_21-06-19/seed_mass.txt", sep = ";", dec=".",header = TRUE)
# summary(SLA)
# summary(Height)
# summary(SeedMass)
# SeedMass[SeedMass$single.value..mg.==max(SeedMass$single.value..mg.),] it's juglans nigra -> similar to a walnut, therefore it might be the correct weight


SLA2 <- prepare.LEDA.num(SLA)
Height2 <- prepare.LEDA.num(Height)
SeedMass2 <- prepare.LEDA.num(SeedMass)

#colnames(SLA2)[26] <- "SLA..mm.2.mg"
SLA2 <- plyr::rename(SLA2, c("MedianValue" = "SLA..mm2.mg"))
Height2 <- plyr::rename(Height2, c("MedianValue" = "Height..m"))
SeedMass2 <- plyr::rename(SeedMass2, c("MedianValue" = "SeedMass..mg"))

# check some calculations by setting different filter parameters
# SLA%>% select(spec,replicates,value,MedianValue) %>% filter(value<0.5)


write.csv2(SLA2, "LEDA_SLA_medians.csv", row.names = F)
write.csv2(Height2, "LEDA_Height_medians.csv", row.names = F)
write.csv2(SeedMass2, "LEDA_SeedMass_medians.csv", row.names = F)
 