###packages---------------------------
library(tidyverse)
library (ggbeeswarm)
library(ggsurvfit)

###settings -------------------------

#for size and appearance of plot geoms
alpha <- 0.7
pointsize <- 2
linesize <- 1
sixteenshapes <- c(15, 16, 17, 18, 15, 16, 17, 18, 15, 16, 17, 18, 15, 16, 17, 18)

#For customising graph to remove standard R plot details
theme <- theme(panel.background = element_rect(fill = "white"),
               legend.position="none", 
               legend.title = element_blank(), 
               plot.title = element_text(hjust = 0.25, size = 12), 
               axis.text = element_text(size = 12, colour = "black"), 
               panel.border = element_blank(), 
               panel.grid.major = element_line(), 
               panel.grid.minor = element_line(), 
               axis.line = element_line(colour = "black", size = 0.5), 
               axis.title.x = element_text(size=12, margin = margin(t=10)))

#colours for plots
littoral <- "#F5C258"
benthic<-"#6896D4"
female<-"#8D8A91"
male<-"#BF675E"
omb<-"plum"
Madingley<-"palegreen3"
iridophore <- "#DDE4EB"

socshall <- "#B38E40" 
socdeep <- "#6EB2C0"
yolkshall <- "#F5C258"
yolkdeep <- "#6896D4"
embshall <- "#E6AC53"
embdeep <- "#7477C7" 

#colour gradient-derived palette for trajectory plots figs 3 and S5
littoral_pal <- colorRampPalette(c("#B38E40", "#F5C258", "#FFA023"))
benthic_pal <- colorRampPalette(c("#5E55D6", "#6896D4", "#6EB2C0"))
morph_pal <- c(benthic_pal(8),littoral_pal(8))
deep_pal <- c(benthic_pal(10))

###common data -------------------------

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

###data for fig 3 ---------------------

melano_numbers <- read.csv("Melanophore_coverage.csv", header = TRUE, sep = ",")


###plot for fig 3-------------------------

#For stats info included in plot,
#see R script "mean iridophore first appearance embryo and yolk",
#and additional csv file "chromatophore_appearance_data.csv" 

melano_plot <- ggplot(data = melano_numbers, 
                      #Use Aesthetic mapping (aes) to assign variables to the x and y axis
                      aes(x=dpf, y=Melanophore_numbers, 
                          colour = Morph))+
    #Add rectangle  for range of dpf iridophores appear for shallow morphs
  geom_rect(aes(xmin=19, xmax=23, ymin=Inf, ymax=-Inf), 
            fill=iridophore, 
            alpha=0.1, 
            inherit.aes = FALSE) +
    #Add a line for mean iridophores appearance dpf in shallow morphs
  geom_vline(xintercept = 21.07, 
             color = embshall, 
             alpha = 0.5, 
             size = 1)+
    #Add a line for mean iridophores appearance dpf in deep morphs
  geom_vline(xintercept = 19.58, 
             color = embdeep, 
             alpha = 0.5, 
             size = 1)+
    #add data points
  geom_point(alpha=1, size =1.5)+
    #Add a linear model polynomial trendline to the melanophore datapoints
  geom_smooth(aes(x = dpf, y = Melanophore_numbers, color = Morph), method = lm, formula = y ~ poly(x, 2), se = FALSE, size = 2)+
  scale_color_manual(values = c(benthic, littoral))+
  labs(title = " ", x ="Days post-fertilisation", y= "Melanophores (n)")+
    #add theme
  theme

#Display 
melano_plot


###data subsets for fig S3 and S4-------------------------------

#extract only yolk cohort data
FS_events_SL <- subset(all_events_SL, 
                       Cohort == "c yolk_shallow" | 
                       Cohort == "d yolk_deep")

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
#further subsets by morph
ag_start_shallow <- subset(ag_start, Morph.x == "Littoral")
ag_start_deep <- subset(ag_start, Morph.x == "Benthic")

#subsets by event
xanthfin <- subset(ag_start, 
                   Event == "xanthophores in fin")

iridfin <- subset(ag_start, 
                  Event == "iridophores in fin")

irid45 <- subset(ag_start, 
                 Event == "iridophores 4-5" )

assoc <- subset(ag_start, 
                Event == "associations 3-5")

###plots for fig S3-----------------------------------

#Trajectory by SL and morph differentiated by indiv with morph palette

traj_SL_indiv <- ggplot(ag_start, 
                        mapping = aes(x=Standard_length_mm, 
                                      y=Event, colour = Individual.x, 
                                      group =Individual.x,
                                      shape=Individual.x))+
  scale_colour_manual("",values = morph_pal)+
  geom_line(alpha = alpha, size=linesize)+
  geom_point(alpha = 1, size = pointsize)+
  scale_shape_manual(values = sixteenshapes)+
  theme+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_SL_indiv

#Trajectory by day and morph differentiated by indiv with morph palette

traj_day_indiv <- ggplot(ag_start, 
                         mapping = aes(x=Day.x, 
                                       y=Event, colour = Individual.x, 
                                       group =Individual.x,
                                       shape=Individual.x))+
  geom_line(alpha = alpha, size = linesize)+
  geom_jitter(alpha = 1, height=0.17, width=0.04, size = pointsize)+
  scale_colour_manual("",values = morph_pal)+
  scale_shape_manual(values = sixteenshapes)+
  theme+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_day_indiv

#Trajectory by growth and morph differentiated by indiv with morph palette

traj_growth_indiv <- ggplot(ag_start, 
                            mapping = aes(x=Growth, 
                                          y=Event, colour = Individual.x, 
                                          group =Individual.x,
                                          shape=Individual.x))+
  geom_line(alpha=alpha, size=linesize)+
  geom_jitter(alpha = 1, height=0.1, width = 0.04, size = pointsize)+
  scale_colour_manual("",values = morph_pal)+
  scale_x_continuous(limits = c(0,6), n.breaks=6 )+
  scale_shape_manual(values=sixteenshapes)+
  theme+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_growth_indiv


####plots for fig s3 Kaplan-Meier----------

###survival stats plots
survfit2(Surv(time, status) ~ 1, data = xanthfin) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall probability that event not yet happened"
  ) +
  add_confidence_interval() +
  add_risktable()+
  ggtitle("Xanthophores in fin")

survfit2(Surv(time, status) ~ 1, data = iridfin) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall probability that event not yet happened"
  ) +
  add_confidence_interval() +
  add_risktable()+
  ggtitle("Iridophores in fin")+
  scale_x_continuous(breaks=c(0,2,3))

survfit2(Surv(time, status) ~ 1, data = irid45) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall probability that event not yet happened"
  ) +
  add_confidence_interval() +
  add_risktable()+
  ggtitle("Iridophores between fin rays 4-5")+
  scale_x_continuous(breaks=c(0,2,3,5,6,8,9))

survfit2(Surv(time, status) ~ 1, data = assoc) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall probability that event not yet happened"
  )+ 
  add_confidence_interval() +
  add_risktable()+
  ggtitle("Iridophore-xanthophore associations")+
  scale_x_continuous(breaks=c(0,2,3,5,6,8,9,12,14,15,19,23,24,29))


###plots for fig S4--------------------

#3 trajectories coloured by sex (day, SL, growth)

traj_SL_indiv_sex <- ggplot(ag_start, 
                        mapping = aes(x=Standard_length_mm, 
                                      y=Event, colour = Sex.x, 
                                      group =Individual.x,
                                      shape=Individual.x))+
  scale_colour_manual("",values = c(female, male))+
  geom_line(alpha = alpha, size=linesize)+
  geom_point(alpha = 1, size = pointsize)+
  scale_shape_manual(values = sixteenshapes)+
  theme+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_SL_indiv_sex

traj_day_indiv_sex <- ggplot(ag_start, 
                         mapping = aes(x=Day.x, 
                                       y=Event, colour = Sex.x, 
                                       group =Individual.x,
                                       shape=Individual.x))+
  geom_line(alpha = alpha, size = linesize)+
  geom_jitter(alpha = 1, height=0.17, width=0.04, size = pointsize)+
  scale_colour_manual("",values = c(female, male))+
  scale_shape_manual(values = sixteenshapes)+
  theme+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_day_indiv_sex

traj_growth_indiv_sex <- ggplot(ag_start, 
                            mapping = aes(x=Growth, 
                                          y=Event, colour = Sex.x, 
                                          group =Individual.x,
                                          shape=Individual.x))+
  geom_line(alpha=alpha, size=linesize)+
  geom_jitter(alpha = 1, height=0.1, width = 0.04, size = pointsize)+
  scale_colour_manual("",values = c(female, male))+
  scale_x_continuous(limits = c(0,6), n.breaks=6 )+
  scale_shape_manual(values=sixteenshapes)+
  theme+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_growth_indiv_sex

#SL day growth correlations

SL_FS <- filter(SL, Group == "benthic_freeswim" |
                  Group == "littoral_freeswim")

SL_day_FS <- ggplot(SL_FS, mapping = aes(x=Day, y=Standard_length_mm, 
                            group_by = Individual, colour = Group))+
  geom_line(size=linesize, alpha=alpha)+
  scale_colour_manual(values=c(benthic, littoral))+
  theme

SL_day_FS

SL_day_FS_sex <- ggplot(SL_FS, mapping = aes(x=Day, y=Standard_length_mm, 
                            group_by = Individual, colour = Sex))+
  geom_line(size=linesize, alpha=alpha)+
  scale_colour_manual(values=c(female, male))+
  theme

SL_day_FS_sex

### data subsets for fig 5 and S9---------------------------------

all_ag_start <- subset(all_events_SL, 
                       Event == "xanthophores in fin" | 
                       Event == "iridophores in fin" | 
                         Event == "associations 3-5" |
                         Event == "iridophores 4-5" )

###plots for fig 5 and S9 ------------------


plasticity_days <-ggplot(data = all_ag_start, 
                         mapping = aes(x=Day.x, 
                                       y=Event,
                                       fill = Cohort,
                                       colour=Cohort))+
  geom_violin(alpha=0.7, size=0.5, trim=FALSE, scale = "width",
              position=position_dodge(width=0.7))+
  geom_quasirandom(dodge.width = 0.7, groupOnX = F, varwidth = TRUE)+
  labs(title = " ", x="Day", y=" ")+
  scale_colour_manual(values=c(socshall, socdeep, yolkshall, yolkdeep, embshall, embdeep))+
  scale_fill_manual(values=c(socshall, socdeep, yolkshall, yolkdeep, embshall, embdeep))+
  theme+
  xlim(0,38)+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

plasticity_days

plasticity_SL <-ggplot(data = all_ag_start, 
                       mapping = aes(x=Standard_length_mm, 
                                     y=Event,
                                     fill = Cohort,
                                     colour=Cohort))+
  geom_violin(alpha=0.7, size=0.5, trim=FALSE, scale = "width",
              position=position_dodge(width=0.7))+
  geom_quasirandom(dodge.width = 0.7, groupOnX = F, varwidth = TRUE)+
  labs(title = " ", x="SL (mm)", y=" ")+
  scale_colour_manual(values=c(socdeep, yolkshall, yolkdeep, embshall, embdeep))+
  scale_fill_manual(values=c(socdeep, yolkshall, yolkdeep, embshall, embdeep))+
  theme+
  xlim(8,21)+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

plasticity_SL

plasticity_growth <-ggplot(data = all_ag_start, 
                           mapping = aes(x=Growth, 
                                         y=Event,
                                         fill = Cohort,
                                         colour=Cohort))+
  geom_violin(alpha=0.7, size=0.5, trim=FALSE, scale = "width",
              position=position_dodge(width=0.7))+
  geom_quasirandom(dodge.width = 0.7, groupOnX = F, varwidth = TRUE)+
  labs(title = " ", x="Growth (mm)", y=" ")+
  scale_colour_manual(values=c(socdeep, yolkshall, yolkdeep, embshall, embdeep))+
  scale_fill_manual(values=c(socdeep, yolkshall, yolkdeep, embshall, embdeep))+
  theme+
  xlim(0,8)+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

plasticity_growth

### data subsets for S6------------------

DA_events_SL <- subset(all_events_SL, 
                       Cohort == "b socialised_deep")

#make a subset dataframe with only earliest events
DAag_start <- subset(DA_events_SL, 
                     Event == "iridophores in fin" | 
                       Event == "xanthophores in fin" |
                       Event == "associations 3-5" |
                       Event == "iridophores 4-5" )

#reorder levels
DAag_start$Event <- factor(DAag_start$Event, levels=c("associations 3-5",
                                                      "iridophores 4-5", 
                                                      "iridophores in fin",
                                                      "xanthophores in fin"))
###plots for S6-------------------------

#Trajectory by SL by individual

traj_SL_indiv <- ggplot(DAag_start, 
                        mapping = aes(x=Standard_length_mm, 
                                      y=Event, colour = Individual.x, 
                                      group =Individual.x,
                                      shape=Individual.x))+
  scale_colour_manual("",values = deep_pal)+
  geom_line(alpha = alpha, size=linesize)+
  geom_point(alpha = 1, size = pointsize)+
  scale_shape_manual(values = sixteenshapes)+
  theme+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_SL_indiv

#Trajectory by day by individual

traj_day_indiv <- ggplot(DAag_start, 
                         mapping = aes(x=Day.x, 
                                       y=Event, colour = Individual.x, 
                                       group =Individual.x,
                                       shape=Individual.x))+
  geom_line(size=linesize, alpha=alpha)+
  geom_jitter(alpha = 1, height=0.17, width=0.04, size = pointsize)+
  scale_colour_manual("",values = deep_pal)+
  scale_shape_manual(values = sixteenshapes)+
  theme+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_day_indiv


#Trajectory by growth by individual

traj_growth_indiv <- ggplot(DAag_start, 
                            mapping = aes(x=Growth, 
                                          y=Event, colour = Individual.x, 
                                          group =Individual.x,
                                          shape=Individual.x))+
  geom_line(size=linesize, alpha = alpha)+
  geom_jitter(alpha = 1, height=0.17, width=0.04, size = pointsize)+
  scale_colour_manual("",values = deep_pal)+
  xlim(0,6)+
  scale_shape_manual(values = sixteenshapes)+
  theme+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_growth_indiv


### by clutch

#Trajectory by SL by clutch

traj_SL_clutch <- ggplot(DAag_start, 
                         mapping = aes(x=Standard_length_mm, 
                                       y=Event, colour = Clutch.x, 
                                       group =Individual.x,
                                       shape=Individual.x))+
  scale_colour_manual("",values = c(Madingley, omb))+
  geom_line(alpha=alpha, size=linesize)+
  geom_point(alpha = 1, size = pointsize)+
  scale_shape_manual(values = sixteenshapes)+
  theme+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_SL_clutch

#Trajectory by day by clutch

traj_day_clutch <- ggplot(DAag_start, 
                          mapping = aes(x=Day.x, 
                                        y=Event, colour = Clutch.x, 
                                        group =Individual.x,
                                        shape=Individual.x))+
  scale_colour_manual("",values = c(Madingley, omb))+
  geom_line(alpha=alpha, size=linesize)+
  geom_jitter(alpha = 1, height=0.17, width=0.04, size = pointsize)+
  scale_shape_manual(values = sixteenshapes)+
  theme+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_day_clutch


#Trajectory by growth by clutch

traj_growth_clutch <- ggplot(DAag_start, 
                             mapping = aes(x=Growth, 
                                           y=Event, colour = Clutch.x, 
                                           group =Individual.x,
                                           shape=Individual.x))+
  scale_colour_manual("",values = c(Madingley, omb))+
  geom_line(size = linesize, alpha = alpha)+
  geom_jitter(alpha = 1, height=0.17, width=0.04, size = pointsize)+
  xlim(0,6)+
  scale_shape_manual(values = sixteenshapes)+
  theme+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_growth_clutch


##by sex

#Trajectory by SL by sex

traj_SL_indiv <- ggplot(DAag_start, 
                        mapping = aes(x=Standard_length_mm, 
                                      y=Event, colour = Sex.x, 
                                      group =Individual.x,
                                      shape=Individual.x))+
  scale_colour_manual("",values = c(female, male))+
  geom_line(alpha = alpha, size=linesize)+
  geom_point(alpha = 1, size = pointsize)+
  scale_shape_manual(values = sixteenshapes)+
  theme+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_SL_indiv

#Trajectory by day by sex

traj_day_indiv <- ggplot(DAag_start, 
                         mapping = aes(x=Day.x, 
                                       y=Event, colour = Sex.x, 
                                       group =Individual.x,
                                       shape=Individual.x))+
  geom_line(size=linesize, alpha=alpha)+
  geom_jitter(alpha = 1, height=0.17, width=0.04, size = pointsize)+
  scale_colour_manual("",values = c(female,male))+
  scale_shape_manual(values = sixteenshapes)+
  theme+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_day_indiv


#Trajectory by growth by sex

traj_growth_indiv <- ggplot(DAag_start, 
                            mapping = aes(x=Growth, 
                                          y=Event, colour = Sex.x, 
                                          group =Individual.x,
                                          shape=Individual.x))+
  scale_colour_manual("",values = c(female,male))+
  geom_line(size=linesize, alpha = alpha)+
  geom_jitter(alpha = 1, height=0.17, width=0.04, size = pointsize)+
  scale_shape_manual(values = sixteenshapes)+
  theme+
  xlim(0,6)+
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

traj_growth_indiv

### subsets and additions for S8-----------------------
SL_skip_emb <- subset (SL_skip,
                       Group == "littoral_emb" |
                         Group == "benthic_emb")
SL_skip_yolk <- subset(SL_skip,
                       Group == "littoral_freeswim" |
                         Group == "benthic_freeswim")
SL_skip_soc <- subset (SL_skip,
                       Group == "benthic_adult")

SL_skip_emb$Dpf <- SL_skip_emb$Day +10
SL_skip_yolk$Dpf<- SL_skip_yolk$Day +20
SL_skip_soc$Dpf <- SL_skip_soc$Day + 62

SL_dpf <- rbind(SL_skip_emb, SL_skip_yolk, SL_skip_soc)

###plots for S8-------------------------

#SL dpf correlations for all cohorts (with measured SLs)
all_SL_dpf <- ggplot(SL_dpf, mapping = aes(x=Dpf, y=Standard_length_mm, 
                                           group_by = Individual, colour = Group))+
  geom_line(size=linesize, alpha=alpha)+
  scale_colour_manual(values = c(socdeep,embshall,yolkdeep,embdeep,yolkshall))+
  theme

all_SL_dpf


#SL day growth correlations for all cohorts (with measured SLs)

all_SL_day <- ggplot(SL_skip, mapping = aes(x=Day, y=Standard_length_mm, 
                              group_by = Individual, colour = Group))+
  geom_line(size=linesize, alpha=alpha)+
  scale_colour_manual(values = c(socdeep,embshall,yolkdeep,embdeep,yolkshall))+
  theme

all_SL_day

all_growth_day <- ggplot(SL_skip, mapping = aes(x=Day, y=Growth, 
                              group_by = Individual, colour = Group))+
  geom_line(size=linesize, alpha=alpha)+
  scale_colour_manual(values = c(socdeep,embshall,yolkdeep,embdeep,yolkshall))+
  theme

all_growth_day

## SL day growth correlations for deep socialised

SL_DA_skip <- filter(SL_skip, Group == "benthic_adult")

SL_day_DA <- ggplot(SL_DA_skip, mapping = aes(x=Day, y=Standard_length_mm, group_by = Individual, colour = Sex))+
  geom_line(size=linesize, alpha=alpha)+
  xlim(0,149)+
  scale_colour_manual(values=c(female,male))+
  theme

SL_day_DA

SL_day_DA_clutch <- ggplot(SL_DA_skip, mapping = aes(x=Day, y=Standard_length_mm, group_by = Individual, colour = Clutch))+
  geom_line(size=linesize, alpha=alpha)+
  xlim(0,149)+
  scale_colour_manual(values=c(Madingley, omb))+
  theme

SL_day_DA_clutch
