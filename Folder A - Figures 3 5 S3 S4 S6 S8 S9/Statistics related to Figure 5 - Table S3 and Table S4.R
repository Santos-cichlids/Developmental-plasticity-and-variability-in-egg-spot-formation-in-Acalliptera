library(tidyverse)

###data ---------------------

all_events <- read_csv("individual events for plotting all datasets standardised.csv")
colnames(all_events)

SL <- read_csv("all standard lengths.csv")

#create unique identifiers for each row matching between datasets
all_events$Indiv_day <- paste(all_events$Individual, all_events$Day, sep="_")
SL$Indiv_day <- paste(SL$Individual, SL$Day, sep="_")

#join datasets by unque identifiers 
all_events_SL <- all_events %>% left_join(SL, by = "Indiv_day")

#check no data lost (number of rows the same, columns increase)
dim(all_events)
dim(all_events_SL)

all_events_SL$Growth <- (all_events_SL$Standard_length_mm)- (all_events_SL$Initial_SL)


###subsets---------------------------------

all_xanthfin <- subset(all_events_SL, 
                      Event == "xanthophores in fin")

all_iridfin <- subset(all_events_SL, 
                      Event == "iridophores in fin")

all_irid45 <- subset(all_events_SL, 
                     Event == "iridophores 4-5" )

all_assoc <- subset(all_events_SL, 
                    Event == "associations 3-5")

speed <- all_irid45 %>% left_join(all_assoc, by = "Individual.x")


Day.x.y-Day.x.x

speed$Speed <- (speed$Day.x.y-speed$Day.x.x)

###complete useful set of tests--------------------------

pairwise.wilcox.test(all_xanthfin$Day.x,
                     all_xanthfin$Combi_cohort,
                     p.adjust.method="bonferroni",
                     exact = FALSE)

pairwise.wilcox.test(all_xanthfin$Standard_length_mm,
                     all_xanthfin$Combi_cohort,
                     p.adjust.method="bonferroni",
                     exact = FALSE)

pairwise.wilcox.test(all_iridfin$Day.x,
                     all_iridfin$Combi_cohort,
                     p.adjust.method="bonferroni",
                     exact = FALSE)

pairwise.wilcox.test(all_iridfin$Standard_length_mm,
                     all_iridfin$Combi_cohort,
                     p.adjust.method="bonferroni",
                     exact = FALSE)

pairwise.wilcox.test(all_irid45$Day.x,
                     all_irid45$Cohort,
                     p.adjust.method="bonferroni",
                     exact = FALSE)


kruskal.test(Standard_length_mm ~ Combi_cohort,
             data = all_irid45)

pairwise.wilcox.test(all_assoc$Day.x,
                     all_assoc$Cohort,
                     p.adjust.method="bonferroni",
                     exact = FALSE)

kruskal.test(Standard_length_mm ~ Combi_cohort,
             data = all_assoc)


pairwise.wilcox.test(speed$Speed,
                     speed$Cohort.x,
                     p.adjust.method="bonferroni",
                     exact = FALSE)
