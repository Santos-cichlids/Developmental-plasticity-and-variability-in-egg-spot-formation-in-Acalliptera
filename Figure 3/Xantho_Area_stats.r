install.packages("biomaRT")
install.packages("carData")
install.packages("sm")
install.packages("interplot")
install.packages("multcomp")
install.packages("postHoc")

library(ggplot2)
library(ggpubr)
library(car)
library(nortest)
library(sm)
library(interplot)
library(multcomp)
library(postHoc)

#First set the wd and then read in the data
xantho_scatter <- read.csv("xantho_trial_deep_adults.csv", header = TRUE, sep = ",")

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

#SL p-value result is >0.05. We reject the null hypothesis and 
#infer that the SL data follows a normal distribution

###--------------------------Linear regression model------------------------###

#Linear regression model to test the effect of SL, Sex and the interaction between
#SL and sex on the xanthophore area.

model.1 <- lm(Log_Area ~ SL + Sex + SL:Sex,
               data = xantho_scatter)
summary(model.1)

#results
#Interaction interpretation: The effect of sex on xanthophore area coverage is significantly stronger than that of 
#SL which has a no significant impact on xanthophore area coverage. 


#Check results
hist(residuals(model.1),
     col = "darkgray")

plot(fitted(model.1),
     residuals(model.1))

#Interaction plot to visualise the correlation between two interaction terms
interplot(m = model.1, var1 = 'SexM', var2 = 'SL')+
  labs(x = "Sex", y = "coefficient for SL")

#Due to the significant difference in slopes we can reject the H0
#There is a difference in slopes

#Posthoc test required to assess pairwise comparisons between the variables to further investigate effect of Sex on xanthophore area.
Posthoc_Male <- posthoc(Model = model.1, EffectLabels = xantho_scatter$SexM, digits = 2)
summary(Posthoc_Male)
barplot(Posthoc_Male)


#Define colours for the plots
cols <- c("goldenrod2", "#fff700")
#variable with ggplot to display the model fitted on a scatterplot in figure format.
xantho_plot <- ggplot(data = xantho_scatter, aes(x = SL, y = Log_Area))+
  geom_point(aes(x = SL, y = Log_Area, color = Sex, fill = Sex), alpha = 1, size = 1.5)+
  scale_color_manual(values = cols)+
  geom_smooth(aes(x = SL, y = Log_Area, color = Sex), method = lm, se = FALSE, size = 2)+
  labs(title = " ", x = "SL", y = bquote(Log_Area (mm^2)))+
  theme (legend.title = element_blank(), 
         legend.background = element_rect(colour = "white", fill = "white"), 
         legend.box.background = element_rect(colour = "white", fill = "white"), 
         legend.key = element_rect(colour = "white", fill = "white"),
         legend.text = element_text(size = 12, colour = "black"), 
         plot.title = element_text(hjust = 0.25, size = 12), 
         axis.text = element_text(size = 12, colour = "black"), 
         panel.border = element_blank(), panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black", size=0.5),
         panel.background = element_rect(fill = "white"),
         plot.background = element_rect(colour = "white", fill = "white"), 
         axis.title.y = element_text(size = 12, colour = "black", margin = margin (t = 12)), 
         axis.title.x = element_text(size = 12, colour = "black", margin = margin(t = 10)))

#display the plot
xantho_plot


