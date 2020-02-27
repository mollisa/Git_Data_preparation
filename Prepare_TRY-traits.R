# cleaner TRY Data-Prep 
#             (There is an older skript in StaRt doing nearly the same with different paths and without double check for duplicates etc)
# 
# Date: 15.11.2019, adjusted 16.12.2019 -> references need to be kept!!
# last edit: 09.01.2020
# Author: Elisabeth Sellenriek
# Description: All requested traits come in one huge table for all requested species, 
# this contains also metadata to each data-set, sometimes duplicates but at least there should be multiple entries per species
# data needs to be summarized in a meaningful manner
# R version 3.5.1 (2018-07-02), platform x86_64-w64-mingw32

setwd("C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation")

library(data.table)
library(dplyr)
library(spatstat)
library(tidyr)
library(taxize)
library(ggplot2)

TRYdata <- fread("C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Traits/6936.txt") #request
TRYdata1 <- fread("C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Traits/6949.txt") #forgotten species
#TRYdata_0 <- fread("C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Traits/7210.txt") #repeated request (same as the two previous ones combined) but only with the public data
TRYdata2 <- fread("C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Traits/7266.txt") #species found with synonyms in try
TRYdat <- rbind(TRYdata, TRYdata1, TRYdata2)

# column 28 is an artefact from database excerpt
# some rows are doupled because of my multiple requests
data<- TRYdat[,-28] %>% filter(!is.na(TraitID))%>% unique() # 6 entries are complete duplicates (Henry Ford Dataset 174) ObservationIDs: 1459509,1495353,1544804,1611716,1611804,1611892
data$OrigUnitStr<-factor(data$OrigUnitStr)
data$UnitName<-factor(data$UnitName)

#unique(data$TraitName)####
# "Mycorrhiza type"                                                                                        
# "Seed dry mass"                                                                                          
# "Plant height vegetative"                                                                                
# "Plant biomass and allometry: Seed number per plant"                                                     
# [5] "Plant woodiness"                                                                                        
# "Pollination syndrome"                                                                                   
# "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded"
# [8] "Species strategy type according to Grime"  


# numerical traits ####
Traits.num <- data[data$TraitID %in% c("26","3106","131","3117"),] # keep numeric traits
  #Traits.num <- data[data$TraitID %in% c("26","3106","131","3117"),c(6,7,10,11,20,21,22)]# keep numeric traits and columns that make sense after summing up per species
Traits.num$Replicates[is.na(Traits.num$Replicates)] <- 1 # NA could mean anything, so let´s weight it by 1
Traits.num$Replicates[Traits.num$Replicates=="0"] <- 1 # there must be at least one "Replicat" if there is a value
Traits.num$Replicates <- as.integer(Traits.num$Replicates) #use as times for repetition
# Marina discovered, that there are hidden duplicates!! in one dataset values round for 2 decimals in another 3 but it is actually the same value
Traits.num.find_hidden_duplicates <- Traits.num[,c("AccSpeciesID","TraitID","OrigValueStr","OrigUnitStr")]
Traits.num.find_hidden_duplicates$OrigValueStr <- round(as.numeric(Traits.num.find_hidden_duplicates$OrigValueStr), 2)

# duplicated marks the first duplicated entry as FALSE and all following identical ones as TRUE
Traits.num$dupl.Index <- base::duplicated(Traits.num.find_hidden_duplicates[,c("AccSpeciesID","TraitID","OrigValueStr","OrigUnitStr")])
Traits.num <- Traits.num[Traits.num$dupl.Index==F & is.na(Traits.num$StdValue)==F, ] #without duplicats and without NAs
# Replicates can´t be 0, if there is an entry

# Traits.num %>% group_by(TraitID, OrigUnitStr) %>% summarise(frequency=n())
# Traits.num %>% group_by(TraitID, UnitName) %>% summarise(frequency=n())
# Traits.num %>% select(OrigValueStr, OrigUnitStr, StdValue, UnitName) %>% filter(OrigUnitStr=="1/pound")
# Thank you!!! The values seem to be standartized 


  Traits.num1 <- Traits.num %>% group_by(AccSpeciesID, TraitID) %>%
    mutate(MedianValue = if(sum(!is.na(Replicates))<3 ){weighted.mean(StdValue, Replicates, na.rm=T)} #you can only calculate a median from more than 3 replicates
                          else {weighted.median(StdValue,Replicates, na.rm=TRUE)},
           # create ID for each calculated mean/median value -> TraitID_Index
           # so the references are kept and i can trace back, which datasets I used and whom I have to contact for instance for coauthership
           ValueID = paste(TraitID,sep="_",AccSpeciesID))
         write.table(Traits.num1,"./Traits/Traits_num_with_references.txt", row.names = F)


Traits.num.mat <- Traits.num %>% group_by(AccSpeciesID, AccSpeciesName, TraitID) %>%
      summarize(MedianValue = if(sum(!is.na(Replicates))<3 ){mean(StdValue, Replicates, na.rm=T)} #you can only calculate a median from more than 3 replicates
             else {weighted.median(StdValue,Replicates, na.rm=TRUE)}) %>%
  
                spread(key=TraitID, value=MedianValue, fill = NA) %>%
                  rename("seed_mass"="26","height"="3106","seed_number"="131","SLA"="3117")
    
write.table(Traits.num.mat,"C:/Users/sellenri/Nextcloud/Cloud/R/LisR/Cleaner_Data_Preparation/Traits/Traits_num_mat.txt", row.names = F )

# Categorical traits ####
# check the sample size (Replicats) before aggregating trait per species!!
Traits.cat <- data[data$TraitID %in% c("7","38","29","196"),] 
unique(Traits.cat[,c("TraitID","TraitName")])

# woodiness
# no number of Replicates given
woodiness <- Traits.cat[Traits.cat$TraitID=="38",]
woodiness$woody<- ifelse(woodiness$OrigValueStr %in% c("0","h","non-woody","H","Grass&Sedges","Herb","Herbaceous"),0,
                         ifelse(woodiness$OrigValueStr %in% c("3","2","w","woody","W","Woody"),1,
                                ifelse(woodiness$OrigValueStr %in% c("1","semi-woody","woody at base","non-woody/woody"),"i",
                                       NA)))
  woody.trait1 <- woodiness %>%
    group_by(AccSpeciesID,AccSpeciesName) %>%
    mutate(woody = names(which.max(table(woody))))
  write.table(woody.trait1,"./Traits/Woody_with_references.txt", row.names = F)

woody.trait <- woodiness %>%
  group_by(AccSpeciesID,AccSpeciesName) %>% 
  summarize (woody = names(which.max(table(woody)))) #woody.trait[woody.trait$woody=="i",] -> only for Artemisia absinthium we get an "i", meaning unclear if woody or not or woody at its base. I use woodiness to distinguisch between herbacious and shrubs/trees, so I consider Beifuss as Herb...
woody.trait[woody.trait$AccSpeciesName=="Artemisia absinthium","woody"] <- 0
write.table(woody.trait,"./Traits/Woodiness.txt", row.names = F)

# pollination type
pollination <- Traits.cat[Traits.cat$TraitID=="29",]
# delete rare pollination types (meeting 12.Sep 19) -> create a class "minor"
# pollination.types.list <- unique(pollination$OrigValueStr)

wind <- c("pollination wind","wind always","wind the rule","wind","anemophil","Wind","1", "anemogamous", "wind pollinated","wind often")
self <- c("selfing at failure of outcrossing","geitonogamy the rule","geitonogamy at failure of outcrossing",
          "selfing the rule", "selfing often","cleistogamy the rule","selfing always", "selfed", "autogamous", "geitonogamy the rule",
          "cleistogamy often")
insects <- c("pollination animals", "beetles, flies, syrphids, wasps, medium tongued bees","syrphids, bees","flies, beetles", "flies",
             "bees, bumblebees, wasps, bombylides, syrphids","bees, bumble bees, wasps, bombylides, syrphids", "hymenopteres",
             "moths","moths, hymenoptera","butterflies","bees","bumble bees" ,"short tongued bees, syrphids, flies, beetles",
             "short tounged bees, syrphids, muscids, beetles","Short tongued bees, syrphids, flies, beetles" ,"insects always",
             "insects the rule", "insects often", "Short tongued bees, syrphids, flies, beetles","Animals",
             "Short tongued bees, syrphids, flies, beetles", "general insect", "entomogamous","insect","2", "bee", "Animals","bio",
             "entomophil", "insect pollinated" )
multiple <-c("Animals Wind","autogamous/entomogamous", "anemogamous/entomogamous")
minor <- c("insects possible","insects rare","wind rare","pseudocleistogamy possible", "selfing possible","geitonogamy possible",
           "selfing rare","wind possible","cleistogamy rare", "cleistogamy possible", "selfing unknown", "insects unknown",
           "slug possible", "selfing never", "wind unknown", "hydrogamous")
# [1] "pollination animals"                                  "pollination wind"                                    
# [3] "beetles, flies, syrphids, wasps, medium tongued bees" "syrphids, bees"
# [5] "flies, beetles"                                       "flies"
# [7] "bees, bumblebees, wasps, bombylides, syrphids"        "bees, bumble bees, wasps, bombylides, syrphids"
# [9] "hymenopteres"                                         "moths"
# [11] "moths, hymenoptera"                                   "butterflies"
# [13] "bees"                                                 "bumble bees"
# [15] "short tongued bees, syrphids, flies, beetles"         "short tounged bees, syrphids, muscids, beetles"
# [17] "Short tongued bees, syrphids, flies, beetles"         "insects always"
# [19] "wind always"                                          #"insects the rule"
# [21] "insects possible"                                     #"selfing often"
# [23] "insects rare"                                        # "selfing the rule"
# [25] "selfing at failure of outcrossing"                    "wind the rule"
# [27] "wind rare"                                            "pseudocleistogamy possible"
# [29] "selfing possible"                                     "geitonogamy possible"
# [31] "selfing rare"                                         "wind possible"
# [33] "cleistogamy the rule"                                 "selfing always"
# [35] "geitonogamy at failure of outcrossing"                
#"cleistogamy rare"
# [37] "cleistogamy possible"                                # "insects often"
# [39] "selfing unknown"                                      "insects unknown"
# [41] "geitonogamy the rule"                                 #"wind often"
# [43] "cleistogamy often"                                    
#"slug possible"
# [45] "selfing never"                                        "wind unknown"
# [47] #"pseudocleistogamy the rule"                           "pseudocleistogamy rare"
# [49] "insect pollinated"                                    "wind pollinated"
# [51] "general insect"                                       "wind"
# [53] "2"                                                    "1"
# [55] "anemogamous"                                          "entomogamous"
# [57] "autogamous/entomogamous"                              "autogamous"
# [59] "anemogamous/entomogamous"                             
# "hydrogamous"
# [61] "Animals"                                              "Wind"
# [63] "Animals Wind"                                         #"insect"
# [65] "selfed"                                               "anemophil"
# [67] "entomophil"                                           "bio"
# [69] "bee"

pollination$poll<- ifelse(pollination$OrigValueStr %in% wind,"wind",
                          ifelse(pollination$OrigValueStr %in% self,"self",
                                 ifelse(pollination$OrigValueStr %in% insects,"insects",
                                        ifelse(pollination$OrigValueStr %in% multiple,"multiple",
                                               ifelse(pollination$OrigValueStr %in% minor,"minor",
                                                      NA)))))
pollination.temp <-pollination[pollination$poll!="minor",]
  summarise(n = n())

pollination.trait <- pollination.temp %>%  group_by(AccSpeciesID,AccSpeciesName)%>% 
  summarise(polli_type= ifelse(n_distinct(poll)>1, "multiple",first(poll))) # it has the last row of NA´s, I don´t know why...
write.table(pollination.trait,"./Traits/Pollination.txt", row.names = F)

    pollination.trait1 <- pollination.temp %>%  group_by(AccSpeciesID,AccSpeciesName)%>%
      mutate(polli_type= ifelse(n_distinct(poll)>1, "multiple",first(poll)))
    write.table(pollination.trait1,"./Traits/Polli_with_references.txt", row.names = F)

# Grime´s strategy type
strategy.all <- Traits.cat[Traits.cat$TraitID=="196",]
# Ingolf and Sonja suggested in September to split CSR strategies into seperate traits with proportions for each plant
# unique(strategy$OrigValueStr)
strategy.biolflor <- strategy.all[c(strategy.all$Dataset=="BiolFlor Database"),]
length(unique(strategy.all.biolflor$AccSpeciesID))
strategy.biolflor$Cstrategy <- grepl("c",strategy.biolflor$OrigValueStr)
strategy.biolflor$Sstrategy <- grepl("s",strategy.biolflor$OrigValueStr)
strategy.biolflor$Rstrategy <- grepl("r",strategy.biolflor$OrigValueStr)
strategy.biolflor <- strategy.biolflor %>% group_by(LastName, FirstName, DatasetID, Dataset, AccSpeciesID, AccSpeciesName, TraitID) %>%
  summarize(Cstrategy= as.integer(any(Cstrategy)), Sstrategy=as.integer(any(Sstrategy)),Rstrategy=as.integer(any(Rstrategy)))%>%
  mutate(sum=Cstrategy+Sstrategy+Rstrategy)%>%
  mutate(Cstrategy= round((Cstrategy/sum),2), Sstrategy= round((Sstrategy/sum),2), Rstrategy= round((Rstrategy/sum),2))


strategy.baseco <- strategy.all[c(strategy.all$Dataset=="BASECO: a floristic and ecological database of Mediterranean French flora"),]
length(unique(strategy.baseco$AccSpeciesID))
strategy.baseco$Cstrategy <- grepl("C",strategy.baseco$OrigValueStr)
strategy.baseco$Sstrategy <- grepl("S",strategy.baseco$OrigValueStr)
strategy.baseco$Rstrategy <- grepl("R",strategy.baseco$OrigValueStr)
strategy.baseco <- strategy.baseco %>% group_by(LastName, FirstName, DatasetID, Dataset, AccSpeciesID, AccSpeciesName, TraitID) %>%
  summarize(Cstrategy= as.integer(any(Cstrategy)), Sstrategy=as.integer(any(Sstrategy)),Rstrategy=as.integer(any(Rstrategy)))%>%
  mutate(sum=Cstrategy+Sstrategy+Rstrategy)%>%
  mutate(Cstrategy= round((Cstrategy/sum),2),Sstrategy= round((Sstrategy/sum),2),Rstrategy= round((Rstrategy/sum),2))

strategy.wildfire <- strategy.all[c(strategy.all$Dataset=="Traits from the Wildfire Project" & strategy$OrigValueStr>1.9),]
length(unique(strategy.wildfire$AccSpeciesID))
strategy.wildfire$Cstrategy <- strategy.wildfire$OriglName == "C"
strategy.wildfire$Sstrategy <- strategy.wildfire$OriglName == "S"
strategy.wildfire$Rstrategy <- strategy.wildfire$OriglName == "R"
strategy.wildfire <- strategy.wildfire %>% group_by(LastName, FirstName, DatasetID, Dataset, AccSpeciesID, AccSpeciesName, TraitID) %>%
  summarize(Cstrategy= as.integer(any(Cstrategy)), Sstrategy=as.integer(any(Sstrategy)),Rstrategy=as.integer(any(Rstrategy)))%>%
  mutate(sum=Cstrategy+Sstrategy+Rstrategy)%>%
  mutate(Cstrategy= round((Cstrategy/sum),2),Sstrategy= round((Sstrategy/sum),2),Rstrategy= round((Rstrategy/sum),2))

# put all together again, BIOLFLOR first, species not in there then BASECO, then wildfire
# conditional rbind? summarize with

# strategy <- strategy.biolflor

#doesn´t work correctly :(
# for (i in 1:nrow(strategy.baseco)){
#   ifelse(strategy.baseco$AccSpeciesID[i] %in% strategy$AccSpeciesID,#print(strategy.baseco[i,]), print("already there"))
#          print("already there"), strategy[nrow(strategy)+1,] <- strategy.baseco[i,] )
#         # strategy <- rbind(strategy, strategy.baseco[i,]), print("already there") )
#   return(strategy)
# }

only_in_baseco <- strategy.baseco[!(strategy.baseco$AccSpeciesID %in% strategy.biolflor$AccSpeciesID),]
only_in_wildfire <- strategy.wildfire[!(strategy.wildfire$AccSpeciesID %in% strategy.biolflor$AccSpeciesID),]

strategy1 <- rbind(strategy.biolflor, only_in_baseco, only_in_wildfire)
write.table(strategy1,"./Traits/GriStrat_with_references.txt", row.names = F)

strategy <- strategy1[,c("AccSpeciesID","AccSpeciesName", "Cstrategy","Sstrategy","Rstrategy")]
write.table(strategy,"./Traits/GriStrat.txt", row.names = F)

My.Traits <- full_join(Traits.num.mat, woody.trait, by=c("AccSpeciesID","AccSpeciesName"))
My.Traits <- full_join(My.Traits,pollination.trait, by=c("AccSpeciesID","AccSpeciesName"))
My.Traits <- full_join(My.Traits,strategy, by=c("AccSpeciesID","AccSpeciesName"))
write.table(My.Traits,"./Traits/mytraits_test.txt", row.names = F)

#------
# Messy, not used!
# mycorrhiza 
# use 3 groups: obligate non mycorrhiza, facultative mycorrhiza, obligate arbuscular mycorrhiza
# actually that is called mycorrhiza status not type! -> it´s available as a different trait in TRY
#
# mycorrhiza type adapted from Marina
# load MycoFlor data additionally
MycoFlor <- read.table("./Traits/MycoFlor.txt", header = TRUE, 
                       stringsAsFactors = FALSE, sep = "\t")
MycoFlor.check_names<-tnrs(MycoFlor$Plant.species,source="iPlant_TNRS")
#setdiff(MycoFlor$Plant.species,MykoFlor.check_names$submittedname) #->0, that means all have at least a partial match
MycoFlor$TNRS_Name <- MycoFlor.check_names$acceptedname
issues<-MycoFlor.check_names[MycoFlor.check_names$score!="1",]# with(MykoFlor.check_names,
MySpec <- read.table("./Surveys/all_recorded_species_all_sites_PROCESSED.txt", header = TRUE, 
                     stringsAsFactors = FALSE, sep = "\t")
Mycoflor.myspec <- left_join(MySpec,MycoFlor, by=c("spec"="TNRS_Name")) %>%
                    select(spec, Mycorrhizal.status, No..Ref) %>% mutate(source = rep("MycoFlor", nrow(.))) %>%
                    rename(AccSpeciesName = spec, StdValue = Mycorrhizal.status, N = No..Ref)

myco<- Traits.cat[Traits.cat$TraitID=="7",]
#myco.types.list <- unique(myco$OrigValueStr) # it hast 1188 different entries coming from 9 different datasets

myco.datasets.list <- unique(myco$DatasetID)
myco.references.list <- unique(myco$Reference)

#"Asem A. Akhmetzhanova, Nadejda A. Soudzilovskaia, Vladimir G. Onipchenko, Will K. Cornwell, Vladimir A. Agafonov, Ivan A. Selivanov, and Johannes H. C. Cornelissen. 2012. A rediscovered treasure  mycorrhizal intensity database for 3000 vascular plants species across the former Soviet Union. Ecology 93: 689; Ecological Archives E093-059-D1."
# has always two entries per species, two different nomenclatures, only keep the modern one
# mycorrhiza_A <- mycorrhiza[mycorrhiza$DatasetID=="162",]
# length(unique(mycorrhiza_A$AccSpeciesID))
# temp1928 <- mycorrhiza_A[mycorrhiza_A$DataID==1928,]
# length(unique(temp1928$AccSpeciesID))
# temp8 <- mycorrhiza_A[mycorrhiza_A$DataID==8,]
# length(unique(temp8$AccSpeciesID))
# both contain 183 species
# "Iversen CM, McCormack ML, Powell AS, Blackwood CB, Freschet GT, Kattge J, Roumet C, Stover DB, Soudzilovskaia NA, Valverde-Barrantes OJ, van Bodegom PM, Violle C (2017) A global Fine-Root Ecology Database to address belowground challenges in plant ecology. New Phytologist. doi:10.1111/nph.14486."
# has two DataIDs too, but I´m not sure what the difference is, the one that is not 8 (which I thought to be the desired one) is much bigger
#"Werner, G. D. A., Cornelissen, J. H. C., Cornwell, W. K., Soudzilovskaia, N. A., Kattge, J., West, S. A., Kiers, E. T. (2018). Symbiont switching and alternative resource acquisition strategies drive mutualism breakdown. Proceedings of the National Academy of Sciences (PNAS) 201721629; DOI: 10.1073/pnas.1721629115"
# has multiple DatasetIDs, mycorrhiza type is saved as different cases not in one variable
# unpublisched dataset contains only a NA for one species? 

myco<- myco[myco$DataID!=1928,] # not needed (Original term for mycorrhizal type given by Selivanov) second name in Data from Aem et al
# myco_A <- myco[myco$DatasetID=="162",]
# length(unique(myco_A$OrigValueStr))


# standardize TRY data
myco$StdValue[myco$OrigValueStr %in% c("E.ch.ect.", "Ecto", "EM", "ectomycorrhizal", "ecto", 
                                               "Ectomycorrhiza", "EC", "ECTO", "ec?")] <- "ECM"
myco$StdValue[myco$OrigValueStr %in% c("E.t.ect.arb.", "Ecto arbut.", 
                                               "AbtM", "Arbutoid")] <- "ABM"
myco$StdValue[myco$OrigValueStr %in% c("E.t.end.", "OrM", "orchid", "Orchid")] <- "ORM"
myco$StdValue[myco$OrigValueStr %in% c("Endo", "Endo-unidentified")] <- "M(endo)"
myco$StdValue[myco$OrigValueStr %in% c("Endo er.", "Ecto er.", "E.t.ect.er.", "ErM", "ericoid", 
                                               "Ericoid", "ER", "ERICOID")] <- "ERM"
myco$StdValue[myco$OrigValueStr %in% c("Ps.end.", "DS")] <- "DS"
myco$StdValue[myco$OrigValueStr %in% c("Ph.th.end.", "VAM", "AM", "arbuscular", "VA", 
                                               "Gram-AM", "Forb-AM", "Wood-AM", "va?")] <- "AM"
myco$StdValue[myco$OrigValueStr %in% c("absent", "no", "0", "Absent", "NM", "non-mycorrhizal", 
                                               "Non-mycorrhizal", "Non", "N")] <- "NM"
myco$StdValue[myco$OrigValueStr %in% c("AMNM", "NM/AM")] <- "AM+NM"
myco$StdValue[myco$OrigValueStr %in% c("mycorrhizal", "2")] <- "Type unknown"
myco$StdValue[myco$OrigValueStr %in% c("EeM", "Ectendo")] <- "EEM"
myco$StdValue[myco$OrigValueStr %in% c("non-ectomycorrhizal")] <- "NECM"
myco$StdValue[myco$OrigValueStr %in% c("EC/AM", "AM + EM")] <- "AM+ECM"
myco$StdValue[myco$OrigValueStr %in% c("pyroloid")] <- "Pyroloid"
myco$StdValue[myco$OrigValueStr %in% c("monotropoid")] <- "Monotropoid"
# unique(myco$StdValue)

mycoTRY <- myco %>% filter(!is.na(StdValue))%>%
  group_by(AccSpeciesName, AccSpeciesID, StdValue) %>%
  summarise(N = length(AccSpeciesName)) %>%
  as.data.frame()%>% mutate(source = rep("TRY", nrow(.)))

# all mycorrhiza data for my species from try and mycoflor together
Mycorrhiza1 <- plyr::rbind.fill(mycoTRY,Mycoflor.myspec)
Mycorrhiza <- tidyr::separate_rows(Mycorrhiza1,StdValue)
#unique(Mycorrhiza$StdValue)
Mycorrhiza <- Mycorrhiza %>% group_by(AccSpeciesName, StdValue) %>% 
                  summarise(N=sum(N)) %>%
                      spread(key=StdValue, value=N, fill =0) %>% select(-`<NA>`) %>%
                        mutate(Ntotal=(AM+ECM+NM+NECM+DS))
Myco.relative <- Mycorrhiza %>% transmute(AM=AM/Ntotal, ECM =ECM/Ntotal, NM=NM/Ntotal, NECM=NECM/Ntotal, DS= DS/Ntotal)
Myco.bool <- Mycorrhiza[,1:6]
Myco.bool[>0,]<- 1
apply(Myco.bool, x>1, "1")
                                  
#Mycorrhiza.test <- apply(Mycorrhiza[,2:6],1,function(x) x/Mycorrhiza[,7])

# Mycorrhiza.test <- apply(Mycorrhiza[,2:6],1, divide(by= Mycorrhiza[,7]))

#Mycorrhiza.test <- transmute_at(c("AM" ,  "ECM" , "NM"  , "NECM" ,"DS"), +1)

# try <- try[-which(try %>% select(AccSpeciesName, StdValue) %>% duplicated()), ]
# try <- try %>% arrange(AccSpeciesName)
# write.table(try, file = "~/Documents/Data/Traits/Mycorrhizae/mycorrhizae.txt")
# try2 <- try %>% group_by(AccSpeciesName) %>%
#   summarise(StdValue = gsub("+", ",", paste(as.character(StdValue), collapse = "+"), fixed = TRUE))
# mycorTypes <- c("AM", "ECM", "ABM", "ERM", "DS", "ORM", "M(endo)", "EEM", "NM")
# df <- as.data.frame(matrix(NA, nrow = nrow(try2), ncol = length(mycorTypes)))
# for (i in 1:nrow(try2)) {
#   temp <- which(mycorTypes %in% unique(unlist(strsplit(try2$StdValue[i], ",")))) 
#   df[i, temp] <- 1
# }
# colnames(df) <- mycorTypes
# df$AccSpeciesName <- try2$AccSpeciesName
# write.table(df, file = "~/Documents/Data/Traits/Mycorrhizae/mycorrhizae_binary.txt")