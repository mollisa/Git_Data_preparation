# Clean the vegetation-surveys and prepare them for later analyses (match TRY-traits to Species etc)
# 
# adjusted 24.02.2020
# Date: 28.11.2019
# Author: Elisabeth Sellenriek
# Description: The vegetation-surveys contained typos, small mistakes and some old names.
# These were cleaned in consultation of Mr. Klotz and the original field-notes.
# the plantnames are checked using "The National Resolution Service" and "The Plant List" in order to harmonize
# them with the names used by the TRY-database.
# IPNI (Plants Of the World) and TROPICOS were used by TRY (see: Kattge et al 2011)
# the main tables produced in the script are:
# Dat, which contains the original Data and the corrected cover and species names
# Spec.list, which contains translations from species names to TPL_names and IDs from TPL,POW and TROPICOS

library(taxize) # for name resolution
library(ritis) # also needed for name resolution
library(Taxonstand) # for name resolution
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

# get original database excerpt (files from Alex)
Zoeb <- read.csv2("C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/ORIGINAL/Export_20190606_zoeb.csv")
Rest <- read.csv2("C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/ORIGINAL/tmp002.csv")
ORIGINAL <- rbind(Rest,Zoeb)
Dat <- ORIGINAL

####### Correct labelling ####
Dat$site <- Dat$MAINLOCATION
Dat$site <- gsub("Bad Lauchst?dt","BL",Dat$site)
Dat$site <- gsub("Bad Lauchstädt","BL",Dat$site)
Dat$site <- gsub("Bayreuth","BR",Dat$site)
Dat$site <- gsub("Gimritz","GR",Dat$site)
Dat$site <- gsub("Zoeberitz","ZR",Dat$site)
Dat$site <- as.factor(Dat$site)
#unique(Dat$LOCATION_LABEL)
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

#Dat$spec <- gsub("Galium x pomeranicum", "Galium pomeranicum", Dat$spec) # The plant list acccespt hybrids
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
Dat1 <- Dat %>% group_by(site,location,plot,spec,time,year) %>% summarise(cover.c=max(cover.c))

# transform Cover classes to midpoint percentage cover
Dat2 <- cvr.cnvrt(as.data.frame(Dat1))


#### adjust names for matching to TRY manually ####
species.TRY <- read.delim("C:/Users/sellenri/Nextcloud/Cloud/Data/TRY/TryAccSpecies.txt")

species.list <- distinct(Dat,spec) # I´m loosing Taraxacum officinale here! I don´t know why!
species.list <- unique(Dat$spec)  # here it´s there
species.list <- as.data.frame(unique(Dat$spec)) # here it´s lost



sp.list.TRYID <- left_join(species.list, species.TRY[,c(1,2)], by=c("spec"="AccSpeciesName"))
missing <- filter(sp.list.TRYID, is.na(AccSpeciesID))
                                                            # 1            Elytrigia arenosa           NA
                                                            # 2          Hieracium pilosella           NA
                                                            # 3              Cerasus mahaleb           NA
                                                            # 4                Cerasus avium           NA
                                                            # 5             Achillea setacea           NA
                                                            # 6                Arabis glabra           NA
                                                            # 7  Tripleurospermum perforatum           NA
                                                            # 8      Capsella bursa-pastoris           NA
                                                            # 9          Plantago intermedia           NA
                                                            # 10              Solanum nigrum           NA
                                                            # 11        Taraxacum laevigatum           NA
                                                            # 12         Matricaria recutita           NA
                                                            # 13           Conyza canadensis           NA
                                                            # 14      Cerastium holosteoides           NA
                                                            # 15           Apera spica-venti           NA
                                                            # 16        Leontodon autumnalis           NA
                                                            # 17           Galinsoga ciliata           NA
                                                            # 18            Senecio vernalis           NA
                                                            # 19             Epilobium lamyi           NA
                                                            # 20           Festuca brevipila           NA
                                                            # 21            Rubus fruticosus           NA
                                                            # 22          Microrrhinum minus           NA
                                                            # 23           Anagallis foemina           NA

# adjust this names by using The Plant List from Kew Gardens and Floraweb checklist
Dat.TRYconform <- Dat
Dat.TRYconform$spec <- gsub("Elytrigia arenosa", "Elymus repens", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Hieracium pilosella", "Pilosella officinarum", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Cerasus mahaleb", "Prunus mahaleb", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Cerasus avium", "Cerasus avium var. juliana", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Achillea setacea", "Achillea collina", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Arabis glabra", "Turritis glabra", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Tripleurospermum perforatum", "Tripleurospermum", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Capsella bursa-pastoris", "CAPSELLA BURSA-PASTORIS", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Plantago intermedia", "Plantago major", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Solanum nigrum", "Solanum americanum", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Taraxacum laevigatum", "Taraxacum erythrospermum", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Matricaria recutita", "Matricaria chamomilla", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Conyza canadensis", "Erigeron canadensis", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Cerastium holosteoides", "Cerastium fontanum subsp. vulgare", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Apera spica-venti", "APERA SPICA-VENTI", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Leontodon autumnalis", "Scorzoneroides autumnalis", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Galinsoga ciliata", "Galinsoga quadriradiata", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Senecio vernalis", "Senecio leucanthemifolius", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Epilobium lamyi", "Epilobium tetragonum", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Festuca brevipila", "Festuca trachyphylla", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Rubus fruticosus", "Rubus Sec. Rubus", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Microrrhinum minus", "Chaenorhinum minus", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Anagallis foemina", "Anagallis arvensis", Dat.TRYconform$spec)
Dat.TRYconform$spec <- gsub("Taraxacum officinale", "Taraxacum Ruderalia Kirschner H. Ollg. & Stepanek", Dat.TRYconform$spec)

species.list <- distinct(Dat.TRYconform,spec)
sp.list.TRYID <- left_join(species.list, species.TRY[,c(1,2)], by=c("spec"="AccSpeciesName"))
missing <- filter(sp.list.TRYID, is.na(AccSpeciesID))

write.csv2(Dat.TRYconform,"C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Plants/VegSurveys_Plantnames_matching_TRY.csv", row.names = F)
write.csv2(Dat,"C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Plants/VegSurveys_Plantnames_corrected.csv", row.names = F)

#-------

# use The Plant List
Spec.list <- data.frame(species.list) #create a dataframe to save the synonyms/ IDs etc next to my species names(after basic corrections)
names(Spec.list)[names(Spec.list)=="species.list"] <- "Survey_name" 
# resolve names, solve problems of synonyms etc with the plant list from kew gardens, there is a function in R in package taxonstand
Species.TPL <- TPL(splist=species.list)
issuesTPL <- Species.TPL[Species.TPL$Taxonomic.status=="Unresolved" | Species.TPL$Taxonomic.status=="Synonym",]
unresolvedTPL <- Species.TPL[Species.TPL$Taxonomic.status=="Unresolved",] # only Hypochaeris radicata is not found... Ferkelkraut.
                                                                          # But it still gets an ID, Hypochaeris radicata   gcc-31523
# write.csv2(issuesTPL,"C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/Plants_VegSurveys_Issues_ThePlantList.csv", row.names = F)
Spec.list$TPL_name <- paste(Species.TPL$New.Genus, Species.TPL$New.Species, " ")
Spec.list$TPL_ID <- Species.TPL$New.ID
sum(is.na(Spec.list$TPL_ID))
# Spec.list[Spec.list$Survey_name=="Hypochaeris radicata",] check if unresolved species got a number

#write.csv2(Dat,"C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/SurveyData_correctionsandbasicnameresolution.csv", row.names = F)
#write.csv2(Spec.list,"C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/Species_lists/Species_Name_Resolving.csv", row.names = F)
    # Dat <- read.csv2("C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/SurveyData_correctionsandbasicnameresolution.csv")
    # Spec.list <- read.csv2("C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/Species_lists/Species_Name_Resolving.csv")

#IPNI and TROPICOS were used by TRY (see: Kattge et al 2011)
# is IPNI = pow (plants of the world online) by kew gardens?
# get IDs from these to have a matchable number for TRY
Sys.setenv(TROPICOS_KEY = "ae505b06-54c0-4243-87e8-d49621333ca8")
call_service()
 # "itis", "ncbi", "eol", "col", "tropicos", "gbif", "nbn", "pow"
PLANT.IDs <- get_ids(species.list, db = c("tropicos","pow"), searchtype = "scientific", accepted = F, rows = 1, ask = T, messages = T)
Spec.list$TROPICOS_ID <- PLANT.IDs$tropicos
Spec.list$POW_ID <- PLANT.IDs$pow
#write.csv2(Spec.list,"C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/Species_lists/Species_Name_Resolving_1.csv", row.names = F)

  # #IPNI and TROPICOS were used by TRY (see: Kattge et al 2011)
  # # (also provided in Global Names Resolver)
  # # gnr_datasources() to get source IDs
  # # 165 Tropicos - Missouri Botanical Garden
  # # 167  The International Plant Names Index
  # Spec.IPNI_TROPICOS <- gnr_resolve(species.list,data_source_ids = c(165,167))
  # # Spec.IPNI_TROPICOS[Spec.IPNI_TROPICOS$score < 0.98,] # all have a higher score
  # Spec.TPL.IPNI_TROPICOS <- gnr_resolve(Spec.list$TPL_name,data_source_ids = c(165,167))
  # Spec.GNR <- gnr_resolve(species.list)
  # # write.csv2(Spec.GNR,"C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/Synonyms_GlobalNamesResolver.csv", row.names = F)
  # unique(Spec.GNR$data_source_title)
      
      # TROPICOS_KEY = "ae505b06-54c0-4243-87e8-d49621333ca8"
      # Spec.GNR <- gnr_resolve(species)
      # gnr_resolve(names = species, returndf = TRUE)
      # Spec.IPNI <- ipni_search(species)
      # # ? what is the correct command? Spec.IPNI <- ipni(species)
      # Spec.TROPICOS <- get_tpid(species,key=TROPICOS_KEY)
      # #Spec.TROPICOS <- resolve(mynames, db = 'tp') # only "iplant", "gnr", "tnrs" are possible entries for name resolving


# get Taxonomic Serial Number from ITIS, the number is unique to a species, a name is probably not
# with these numbers for the survey data, as well as for the TRY data, I hope to match the species, even if they are recorded under a synonym
# if Input is requested, I always chose the first entry -> 1 
Spec.TSN <- get_tsn(species.list, searchtype = "scientific", accepted = F, rows = 1, ask = T, messages = T)
Spec.TSN.1 <- get_tsn(as.list(unique(Spec.list$TPL_name)), searchtype = "scientific", accepted = F, rows = 1, messages = T)
sum(is.na(Spec.TSN)) # for 25 species, i can´t find a TSN with this method. Maybe because ITIS is focussed on the US?
sum(is.na(Spec.TSN.1))
Spec.list$TSN <- Spec.TSN



# Things I may try later, when matching to TRY failed...
------------------------------------------------------------------------------------------------------------------------
# check the species list against the German Flora list provided on Floraweb (released 2018)
floraweb.checklist <- read.delim("./Floraweb_checkliste.txt")
my_species.list <- read.delim("./all_recorded_species_all_sites_CORRECTED.txt") #from raw to this i lost nearly 100 species?!
my_species.list.raw <- read.delim("./species_list_with_ID_10x10_Data.txt", header = F)

join <- full_join(floraweb.checklist, my_species.list, by = c("WISS_NAME"="spec")) #%>% filter(SPECIES_ID>0)
spec_notin_floraweb <- anti_join(my_species.list,floraweb.checklist,by=c("spec"="WISS_NAME"))
# aggregates?
myset<-filter(join,SPECIES_ID>0 & SYN_FLAG!=1)
join1 <- full_join(floraweb.checklist, my_species.list.raw, by = c("WISS_NAME"="V2"))

# check for aggregated species to request the most likely subspecies in Try to average their trait values???
# (eg Taraxacum officinales or Rubus fruticosus)


