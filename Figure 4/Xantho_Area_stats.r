
library(ggplot2)
library(nortest)
library(nlme)
library(interplot)



#First set the wd and then read in the data
xantho_scatter <- read.csv("xantho_trial_deep_adults.csv", header = TRUE, sep = ",")

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


head(xantho_scatter)

###--------------Data exploration and statistical analysis-----###

#Data contains 2 quantitaive variables - xanthophore area coverage (Area) (test variable) and Standard Length (SL) (covariate)
#Both quantitaive variables are continuous
#Data contains one nominal variable - Sex 

#H0: There is no difference in Area between males and females
#HA: There is a difference in Area between males and females

#Required to test impact of covariate(SL) and interaction between covariate

###------------------Test data for normality---------------------###

ad.test(xantho_scatter$Area)
hist(xantho_scatter$Area)


#Area result <0.05 indicating the data area not normal
#Move to log transform the Area data and test for normality

#ggresidpanel

ad.test(xantho_scatter$Log_Area)
hist(xantho_scatter$Log_Area)

#Log transformed data p = 0.03638


ad.test(xantho_scatter$SL)
hist(xantho_scatter$SL)

#infer that the SL data follows a normal distribution

###--------------------------Linear regression model------------------------###

#Repeated measures linear regression mixed effect model to test the effect of Sex, SL and the interaction between
#SL and sex on the xanthophore area.

model.1 <- lme(Log_Area ~ Sex + SL+ Sex*SL, random = ~1 |id, data = xantho_scatter)
summary(model.1)

sjPlot::plot_model(model.1, 
                   axis.labels=c("Sex[M]*SL", "SL", "Sex"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of Sex, SL and interaction on Xanthophore area")+theme


sjPlot:: tab_model(model.1)


#Define colours for the plots
cols <- c("#8D8A91", "#BF675E")


#plot 

xantho_plot <- ggplot(data = xantho_scatter, aes(x = SL, y = Log_Area))+
  geom_point(aes(x = SL, y = Log_Area, color = Sex), alpha = 1, size = 1.5)+
  scale_color_manual(labels = c("female", "male"),values = cols)+
  geom_smooth(aes(x = SL, y = Log_Area, color = Sex), method = lm, se = FALSE, size = 2)+
  labs(title = " ", x = "SL", y = bquote(Log_Area (mm^2)))+
  theme 

#Display plot
xantho_plot

