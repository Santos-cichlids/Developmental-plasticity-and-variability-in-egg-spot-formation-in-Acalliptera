
###packages---------------------------
library(tidyverse)
library(survival)
library(ggsurvfit)

####data----------------------------
#import SL measurements
SL <- read_csv("all standard lengths.csv")
#create unique identifiers for each row for matching to other datasets
SL$Indiv_day <- paste(SL$Individual, SL$Day, sep="_")

#create a column for SL growth
SL$Growth <- (SL$Standard_length_mm)- (SL$Initial_SL)

all_events <- read_csv("individual events for plotting all datasets standardised.csv")
colnames(all_events)

#create unique identifiers for each row matching with SL data
all_events$Indiv_day <- paste(all_events$Individual, all_events$Day, sep="_")

#join datasets by unique identifiers 
all_events_SL <- all_events %>% left_join(SL, by = "Indiv_day")

#check no data lost (number of rows the same, columns increase)
dim(all_events)
dim(all_events_SL)

#calculate growth
all_events_SL$Growth <- (all_events_SL$Standard_length_mm)- (all_events_SL$Initial_SL)



#remove NA values from SL column
SL_skip <- SL[!is.na(SL$Standard_length_mm), ]

###data subsets ----------------
#extract only yolk cohort data
FS_events_SL <- subset(all_events_SL, 
                       Cohort == "c yolk_shallow" | 
                         Cohort == "d yolk_deep")

#add status for survival stats package: all are 1 because the event happened rather than they were censored which would be 0
FS_events_SL$status <- 1
#add columns for sex and time in the format used by the survival package
FS_events_SL$sex <- FS_events_SL$Sex.x
FS_events_SL <- 
  FS_events_SL %>% 
  mutate(
    sex = recode(sex, `M` = 1, `F` = 2))
FS_events_SL$time <- FS_events_SL$Day.x

#make a subset dataframe with only earliest events
ag_start <- subset(FS_events_SL, 
                   Event == "iridophores in fin" | 
                     Event == "xanthophores in fin" |
                     Event == "associations 3-5" |
                     Event == "iridophores 4-5" )

#reorder levels
ag_start$Event <- factor(ag_start$Event, levels=c("associations 3-5",
                                                  "iridophores 4-5", 
                                                  "iridophores in fin",
                                                  "xanthophores in fin"))
#subsets by each event 
xanthfin <- subset(ag_start, 
                   Event == "xanthophores in fin")

iridfin <- subset(ag_start, 
                  Event == "iridophores in fin")

irid45 <- subset(ag_start, 
                 Event == "iridophores 4-5" )

assoc <- subset(ag_start, 
                Event == "associations 3-5")

head(xanthfin[, c("time", "status", "sex")])


#subset each event by morph

littoral_iridfin <- subset(iridfin, 
                           Morph.x == "Littoral")
benthic_iridfin <- subset(iridfin, 
                          Morph.x == "Benthic")

littoral_irid45 <- subset(irid45, 
                          Morph.x == "Littoral")
benthic_irid45 <- subset(irid45, 
                         Morph.x == "Benthic")

littoral_assoc <- subset(assoc, 
                         Morph.x == "Littoral")
benthic_assoc <- subset(assoc, 
                        Morph.x == "Benthic")


###Main text and table S2 - survival stats - medians of each event ------------

survfit(Surv(time, status) ~ 1, data = xanthfin)

survfit(Surv(time, status) ~ 1, data = iridfin)

survfit(Surv(time, status) ~ 1, data = irid45)

survfit(Surv(time, status) ~ 1, data = assoc)




###Table S2 survival stats - compare between morphs ----

# medians by cohort
survfit(Surv(time, status) ~ 1, data = littoral_iridfin)
survfit(Surv(time, status) ~ 1, data = benthic_iridfin)

survfit(Surv(time, status) ~ 1, data = littoral_irid45)
survfit(Surv(time, status) ~ 1, data = benthic_irid45)

survfit(Surv(time, status) ~ 1, data = littoral_assoc)
survfit(Surv(time, status) ~ 1, data = benthic_assoc)

#log rank comparison test
survdiff(Surv(time, status) ~ Cohort, data = iridfin)

survdiff(Surv(time, status) ~ Cohort, data = irid45)

survdiff(Surv(time, status) ~ Cohort, data = assoc)



####Main text survival stats - compare between sexes log rank -----

survdiff(Surv(time, status) ~ sex, data = iridfin)

survdiff(Surv(time, status) ~ sex, data = irid45)

survdiff(Surv(time, status) ~ sex, data = assoc)
