library(ggplot2)
library(ggpubr)
library(dplyr)
library(forcats)


group_v_isolated <- read.csv("group_v_isolated.csv", header = TRUE, sep = ",")

head(group_v_isolated)

  Social_v_isolated_plot <-ggplot(data = group_v_isolated, aes(x= SL, y= reorder(Stage, Graph), colour=Stage, fill = Stage))+
  geom_violin(alpha=0.7, size=0.75, trim=FALSE, scale = "count", colour="black")+
  geom_point(alpha=1, size=2)+
  stat_summary(fun=mean, geom="point", size=2, colour="#999999")+
  scale_color_manual(values = c("steelblue2", "steelblue2", "steelblue2"))+
  scale_fill_manual(values=c("steelblue2", "steelblue2", "steelblue2"))+
  labs(title = " ", x="SL (mm)", y=" ")+
  theme(legend.position="none",
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.25, size = 12),
        axis.text = element_text(size = 12, colour = "black"),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 1),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(size=12, margin = margin(t=10)))

Social_v_isolated_plot
