
library(ggplot2)
library(nortest)
library(nlme)
library(interplot)
library(ggpubr)



#First set the wd and then read in the data
xanthophore_coverage <- read.csv("Acalliptera_xanthophore_coverage.csv", header = TRUE, sep = ",")

#Define colours for the plots
cols <- c("#8D8A91", "#BF675E")

#Plot theme

theme <- theme (legend.title = element_blank(),
                legend.position = c(.9,.1),
                legend.background = element_rect(colour = "white", fill = "white"), 
                legend.box.background = element_rect(colour = "white", fill = "white"), 
                legend.key = element_rect(colour = "white", fill = "white"),
                legend.text = element_text(size = 12, colour = "black"), 
                plot.title = element_text(hjust = 0.25, size = 12), 
                axis.text = element_text(size = 12, colour = "black"), 
                panel.border = element_blank(), panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                axis.line = element_line(colour = "black", size=1),
                panel.background = element_rect(fill = "white"),
                plot.background = element_rect(colour = "white", fill = "white"), 
                axis.title.y = element_text(size = 12, colour = "black", margin = margin (t = 12)), 
                axis.title.x = element_text(size = 12, colour = "black", margin = margin(t = 10)))


head(xanthophore_coverage)

#subset the data into the deep and shallow morph groups

deep <- subset(xanthophore_coverage, Cohort == "DA")

shallow <- subset(xanthophore_coverage, Cohort == "shallow")


###--------------Data exploration and statistical analysis-----###

#Data contains 2 quantitaive variables - xanthophore area coverage (Area) (test variable) and Standard Length (Day) (covariate)
#Both quantitaive variables are continuous
#Data contains one nominal variable - Sex 

#H0: There is no difference in Area between males and females
#HA: There is a difference in Area between males and females

#Required to test impact of covariate(Day) and interaction between covariate

###------------------Test data for normality deep dataset ---------------------###

ad.test(deep$Area)
hist(deep$Area)


#Area result <0.05 indicating the data area not normal
#Move to log transform the Area data and test for normality

#ggresidpanel

deep$log_area <- log(deep$Area)

deep

ad.test(deep$log_area)
hist(deep$log_area)


ad.test(deep$Day)
hist(deep$Day)

#infer that the Day data follows a normal distribution

###--------------Correlations-----------------###

maledata <- subset(deep, Sex == "M")
femaledata <- subset(deep, Sex == "F")

ggscatter(maledata, x = "Day", y = "log_area", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          color = "#8D8A91",
          xlab = "Day", ylab = "Log Area")

ggscatter(femaledata, x = "Day", y = "log_area", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          color = "#BF675E",
          xlab = "Day", ylab = "Log Area")


###---------------------------Size comparisons-----------------------------###

Last_day <- subset(deep, Day == "141")
Last_day

t.test(data = Last_day, SL~Sex)

#Male Standard Lengths are significantly larger than female standard lenghts on the last day of the study

#Plot results as a boxplot
Size_plot <- ggplot(data = Last_day, aes(x = Sex, y = SL))+
  geom_boxplot(data = Last_day, aes(x = Sex, y = SL, colour = Sex), alpha = 1, size = 2)+
  scale_color_manual(labels = c("female", "male"),values = cols)+
  labs(title = " ", y = "SL (mm)", x = "Sex")+
  theme

Size_plot

###--------------------------Linear regression model------------------------###

#Repeated measures linear regression mixed effect model to test the effect of Sex, SL and the interaction between
#SL and sex on the xanthophore area.

head(deep)
deep_clean <- na.omit(deep)
model.deep <- lme(log_area ~ Sex + Day + Sex*Day, random = ~1 | ID, data = deep_clean)


summary(model.deep)

sjPlot::plot_model(model.deep, 
                   axis.labels = c("Sex[M]*Day", "Day", "Sex"),
                   show.values = TRUE, show.p = TRUE,
                   title = "Effect of Sex, SL and interaction on Xanthophore area")+theme


sjPlot:: tab_model(model.deep)

#Model plot demonstrates the significant large effect due to sex
#Plot also demonstrates the highly significant result due to the interaction between SL and sex but a small effect size
#Effect is entirely due to sex


#plot 

deep_plot <- ggplot(data = deep_clean, aes(x = Day, y = log_area))+
  geom_point(aes(x = Day, y = log_area, color = Sex), alpha = 1, size = 1.5)+
  scale_color_manual(labels = c("female", "male"),values = cols)+
  geom_smooth(aes(x = Day, y = log_area, color = Sex), method = lm, se = FALSE, size = 2)+
  labs(title = " ", x = "Day", y = bquote(Log_Area (mm^2)))+
  theme 

#Display plot
deep_plot


###------------------Test data for normality Shallow dataset ---------------------###

ad.test(shallow$Area)
hist(shallow$Area)


#Area result <0.05 indicating the data area not normal
#Move to log transform the Area data and test for normality

#ggresidpanel

shallow$log_area <- log(shallow$Area)

shallow

ad.test(shallow$log_area)
hist(shallow$log_area)


ad.test(shallow$Day)
hist(shallow$Day)

#infer that the Day data follows a normal distribution

###--------------Correlations-----------------###

maledata <- subset(shallow, Sex == "M")
femaledata <- subset(shallow, Sex == "F")

ggscatter(maledata, x = "Day", y = "log_area", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          color = "#8D8A91",
          xlab = "Day", ylab = "Log Area")

ggscatter(femaledata, x = "Day", y = "log_area", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          color = "#BF675E",
          xlab = "Day", ylab = "Log Area")



###--------------------------Linear regression model------------------------###

#Repeated measures linear regression mixed effect model to test the effect of Sex, Day and the interaction between
#Day and sex on the xanthophore area.

head(shallow)
shallow_clean <- na.omit(shallow)
model.shallow <- lme(log_area ~ Sex + Day + Sex*Day, random = ~1 | ID, data = shallow_clean)


summary(model.shallow)

sjPlot::plot_model(model.shallow, 
                   axis.labels = c("Sex[M]*Day", "Day", "Sex"),
                   show.values = TRUE, show.p = TRUE,
                   title = "Effect of Sex, Day and interaction on Xanthophore area")+theme


sjPlot:: tab_model(model.shallow)

#Model plot demonstrates the significant large effect due to sex
#Plot also demonstrates the highly significant result due to the interaction between Day and sex but a small effect size
#Effect is entirely due to sex


#plot 

shallow_plot <- ggplot(data = shallow_clean, aes(x = Day, y = log_area))+
  geom_point(aes(x = Day, y = log_area, color = Sex), alpha = 1, size = 1.5)+
  scale_color_manual(labels = c("female", "male"),values = cols)+
  geom_smooth(aes(x = Day, y = log_area, color = Sex), method = lm, se = FALSE, size = 2)+
  labs(title = " ", x = "Day", y = bquote(Log_Area (mm^2)))+
  theme 

#Display plot
shallow_plot




