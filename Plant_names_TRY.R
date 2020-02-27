# Adjust the Species´ names to TRY names
# 
# Date: 25.02.2020
# Author: Elisabeth Mollenkopf
# R version 3.5.1 (2018-07-02), platform x86_64-w64-mingw32

# Description:
# not all my plants are found in TRY, so names are checked and translated by using The Plant List from Kew Gardens and Floraweb checklist
# IPNI (Plants Of the World) and TROPICOS were used by TRY (see: Kattge et al 2011) is it equvalent to list of KEW GARDENS ?

# Output:
# Spec.list with my species names, Try names and TRY IDs

library(dplyr)
library(tidyr)

#### load TRY plant list and my species####
species.TRY <- read.delim("C:/Users/sellenri/Nextcloud/Cloud/Data/TRY/TryAccSpecies.txt")
str(species.TRY)
# read in the surveys with corrected names but not simplified or cover-transformed -> contains also the very low abundand species
Dat <- read.csv("Surveys_ORIGINAL_with_corrections.csv")
str(Dat)
# species.list <- distinct(Dat,spec) # I?m loosing Taraxacum officinale here! I don't know why!
# species.list <- unique(Dat$spec)  # here it's there
# species.list <- as.data.frame(unique(Dat$spec)) # here it's lost
my.species <- Dat %>% select("spec") %>% distinct() # keeps hopefully all species and is a tbl/dataframe, so I can do the join
str(my.species)

sp.list.TRYID <- left_join(my.species, species.TRY[,c(1,2)], by=c("spec"="AccSpeciesName")) # coercing factors to characters for join
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
# and check manually the TRY_list, there are mistakes by case-sensitive matching -> CAPITALS ARE NEEDED from time to time
my.species$spec_TRY_name<- my.species$spec
my.species$spec_TRY_name <- gsub("Elytrigia arenosa", "Elymus repens", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Hieracium pilosella", "Pilosella officinarum", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Cerasus mahaleb", "Prunus mahaleb", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Cerasus avium", "Cerasus avium var. juliana", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Achillea setacea", "Achillea collina", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Arabis glabra", "Turritis glabra", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Tripleurospermum perforatum", "Tripleurospermum", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Capsella bursa-pastoris", "CAPSELLA BURSA-PASTORIS", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Plantago intermedia", "Plantago major", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Solanum nigrum", "Solanum americanum", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Taraxacum laevigatum", "Taraxacum erythrospermum", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Matricaria recutita", "Matricaria chamomilla", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Conyza canadensis", "Erigeron canadensis", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Cerastium holosteoides", "Cerastium fontanum subsp. vulgare", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Apera spica-venti", "APERA SPICA-VENTI", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Leontodon autumnalis", "Scorzoneroides autumnalis", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Galinsoga ciliata", "Galinsoga quadriradiata", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Senecio vernalis", "Senecio leucanthemifolius", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Epilobium lamyi", "Epilobium tetragonum", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Festuca brevipila", "Festuca trachyphylla", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Rubus fruticosus", "Rubus Sec. Rubus", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Microrrhinum minus", "Chaenorhinum minus", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Anagallis foemina", "Anagallis arvensis", my.species$spec_TRY_name)
my.species$spec_TRY_name <- gsub("Taraxacum officinale", "Taraxacum Ruderalia Kirschner H. Ollg. & Stepanek", my.species$spec_TRY_name)

sp.list.TRYID <- left_join(my.species, species.TRY[,c(1,2)], by=c("spec_TRY_name"="AccSpeciesName"))
missing <- filter(sp.list.TRYID, is.na(AccSpeciesID))

write.csv(sp.list.TRYID,"SpeciesList_TRY_name_and_ID.csv", row.names = F)

##### not sure if I actually need this part below ####
# the plantnames are checked using "The National Resolution Service" and "The Plant List" in order to harmonize
# them with the names used by the TRY-database.
# 
# # use The Plant List
# Spec.list <- data.frame(species.list) #create a dataframe to save the synonyms/ IDs etc next to my species names(after basic corrections)
# names(Spec.list)[names(Spec.list)=="species.list"] <- "Survey_name" 
# # resolve names, solve problems of synonyms etc with the plant list from kew gardens, there is a function in R in package taxonstand
# Species.TPL <- TPL(splist=species.list)
# issuesTPL <- Species.TPL[Species.TPL$Taxonomic.status=="Unresolved" | Species.TPL$Taxonomic.status=="Synonym",]
# unresolvedTPL <- Species.TPL[Species.TPL$Taxonomic.status=="Unresolved",] # only Hypochaeris radicata is not found... Ferkelkraut.
# # But it still gets an ID, Hypochaeris radicata   gcc-31523
# # write.csv2(issuesTPL,"C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/Plants_VegSurveys_Issues_ThePlantList.csv", row.names = F)
# Spec.list$TPL_name <- paste(Species.TPL$New.Genus, Species.TPL$New.Species, " ")
# Spec.list$TPL_ID <- Species.TPL$New.ID
# sum(is.na(Spec.list$TPL_ID))
# # Spec.list[Spec.list$Survey_name=="Hypochaeris radicata",] check if unresolved species got a number
# 
# #write.csv2(Dat,"C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/SurveyData_correctionsandbasicnameresolution.csv", row.names = F)
# #write.csv2(Spec.list,"C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/Species_lists/Species_Name_Resolving.csv", row.names = F)
# # Dat <- read.csv2("C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/SurveyData_correctionsandbasicnameresolution.csv")
# # Spec.list <- read.csv2("C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/Species_lists/Species_Name_Resolving.csv")
# 
# #IPNI and TROPICOS were used by TRY (see: Kattge et al 2011)
# # is IPNI = pow (plants of the world online) by kew gardens?
# # get IDs from these to have a matchable number for TRY
# Sys.setenv(TROPICOS_KEY = "ae505b06-54c0-4243-87e8-d49621333ca8")
# call_service()
# # "itis", "ncbi", "eol", "col", "tropicos", "gbif", "nbn", "pow"
# PLANT.IDs <- get_ids(species.list, db = c("tropicos","pow"), searchtype = "scientific", accepted = F, rows = 1, ask = T, messages = T)
# Spec.list$TROPICOS_ID <- PLANT.IDs$tropicos
# Spec.list$POW_ID <- PLANT.IDs$pow
# #write.csv2(Spec.list,"C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/Species_lists/Species_Name_Resolving_1.csv", row.names = F)
# 
# # #IPNI and TROPICOS were used by TRY (see: Kattge et al 2011)
# # # (also provided in Global Names Resolver)
# # # gnr_datasources() to get source IDs
# # # 165 Tropicos - Missouri Botanical Garden
# # # 167  The International Plant Names Index
# # Spec.IPNI_TROPICOS <- gnr_resolve(species.list,data_source_ids = c(165,167))
# # # Spec.IPNI_TROPICOS[Spec.IPNI_TROPICOS$score < 0.98,] # all have a higher score
# # Spec.TPL.IPNI_TROPICOS <- gnr_resolve(Spec.list$TPL_name,data_source_ids = c(165,167))
# # Spec.GNR <- gnr_resolve(species.list)
# # # write.csv2(Spec.GNR,"C:/Users/sellenri/Nextcloud/Cloud/Data/Vegetation_Surveys/Synonyms_GlobalNamesResolver.csv", row.names = F)
# # unique(Spec.GNR$data_source_title)
# 
# # TROPICOS_KEY = "ae505b06-54c0-4243-87e8-d49621333ca8"
# # Spec.GNR <- gnr_resolve(species)
# # gnr_resolve(names = species, returndf = TRUE)
# # Spec.IPNI <- ipni_search(species)
# # # ? what is the correct command? Spec.IPNI <- ipni(species)
# # Spec.TROPICOS <- get_tpid(species,key=TROPICOS_KEY)
# # #Spec.TROPICOS <- resolve(mynames, db = 'tp') # only "iplant", "gnr", "tnrs" are possible entries for name resolving
# 
# 
# # get Taxonomic Serial Number from ITIS, the number is unique to a species, a name is probably not
# # with these numbers for the survey data, as well as for the TRY data, I hope to match the species, even if they are recorded under a synonym
# # if Input is requested, I always chose the first entry -> 1 
# Spec.TSN <- get_tsn(species.list, searchtype = "scientific", accepted = F, rows = 1, ask = T, messages = T)
# Spec.TSN.1 <- get_tsn(as.list(unique(Spec.list$TPL_name)), searchtype = "scientific", accepted = F, rows = 1, messages = T)
# sum(is.na(Spec.TSN)) # for 25 species, i can?t find a TSN with this method. Maybe because ITIS is focussed on the US?
# sum(is.na(Spec.TSN.1))
# Spec.list$TSN <- Spec.TSN
# 
# 
# 
# # Things I may try later, when matching to TRY failed...
# ------------------------------------------------------------------------------------------------------------------------
#   # check the species list against the German Flora list provided on Floraweb (released 2018)
#   floraweb.checklist <- read.delim("./Floraweb_checkliste.txt")
# my_species.list <- read.delim("./all_recorded_species_all_sites_CORRECTED.txt") #from raw to this i lost nearly 100 species?!
# my_species.list.raw <- read.delim("./species_list_with_ID_10x10_Data.txt", header = F)
# 
# join <- full_join(floraweb.checklist, my_species.list, by = c("WISS_NAME"="spec")) #%>% filter(SPECIES_ID>0)
# spec_notin_floraweb <- anti_join(my_species.list,floraweb.checklist,by=c("spec"="WISS_NAME"))
# # aggregates?
# myset<-filter(join,SPECIES_ID>0 & SYN_FLAG!=1)
# join1 <- full_join(floraweb.checklist, my_species.list.raw, by = c("WISS_NAME"="V2"))
# 
# # check for aggregated species to request the most likely subspecies in Try to average their trait values???
# # (eg Taraxacum officinales or Rubus fruticosus)