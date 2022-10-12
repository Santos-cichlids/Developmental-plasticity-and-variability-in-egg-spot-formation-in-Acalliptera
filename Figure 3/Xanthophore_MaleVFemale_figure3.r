
library(ggplot2)
library(ggpubr)

xantho_scatter <- read.csv("xanthoArea_sex_data.csv", header = TRUE, sep = ",")

cols <- c("goldenrod2", "#fff700")

xantho_plot <- ggplot(data = xantho_scatter, aes(x = Day, y = Coverage))+
  geom_point(aes(x=Day, y=Coverage, color = Sex, fill = Sex),alpha=1, size =1.5)+
  scale_color_manual(values = cols)+
  geom_smooth(aes(x=Day, y = Coverage, color = Sex), method = lm, formula = y ~ splines::bs(x, 3), se = FALSE,size = 2)+
  labs(title = " ", x ="Day", y= expression("Area (mm^2)"))+
  theme (legend.title = element_blank(), 
         legend.background = element_rect( colour = "white", fill = "white"), 
         legend.box.background = element_rect(colour= "white", fill = "white"), 
         legend.key = element_rect( colour = "white", fill = "white"),
         legend.text = element_text(size = 12, colour = "black"), 
         plot.title = element_text(hjust = 0.25, size=12), 
         axis.text = element_text(size = 12, colour="black"), 
         panel.border = element_blank(), panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black", size=0.5),
         panel.background = element_rect(fill = "white"),
         plot.background = element_rect(colour = "white", fill = "white"), 
         axis.title.y=element_text(size = 12, colour = "black", margin = margin (t=12)), 
         axis.title.x = element_text(size=12, colour = "black", margin = margin(t = 10)))

  
xantho_plot