library(ggplot2)

# create a variable which is the raw data - see csv file in figure 2 directory "Melanophore_coverage.csv"
melano_numbers <- read.csv("Melanophore_coverage.csv", header = TRUE, sep = ",")

#Create variable for figure colours
cols <- c("#6896D4", "#F5C258")


#ggplot using melano_numbers variable 

melano_plot <- ggplot(data = melano_numbers, 
                      #Use Aesthetic mapping (aes) to assign variables to the x and y axis
                      aes(x = dpf, y = Melanophore_numbers, 
                          colour = "black"))+
  geom_point(alpha = 1, size = 1.5, colour = "black")+
  
  #See additional csv file entitled "chromatophore_appearance_data.csv" for stats info included in plot
  
  #Add rectangle to the plot which represents the range of dpf iridophores appear in the dataset for shallow morphs
  geom_rect(aes(xmin = 20, xmax = 23, ymin = Inf, ymax = -Inf), 
            fill = "#F5C258", 
            alpha = 0.01, 
            inherit.aes = FALSE) +
  #Add rectangle to the plot which represents the range of dpf iridophores appear in the dataset for deep morphs
  geom_rect(aes(xmin = 18, xmax = 21, ymin = Inf, ymax = -Inf), 
            fill = "#6896d4", 
            alpha = 0.01, 
            inherit.aes = FALSE) +
  
  #Add a line to the plot representing the mean iridophores appearance dpf in the dataset in the shallow morphs
  geom_vline(xintercept = 21.4, 
             color = "#F5C258", 
             alpha = 1, 
             size = 1)+
  #Add a line to the plot representing the mean iridophores appearance dpf in the dataset in the deep morphs
  geom_vline(xintercept = 19.14, 
             color = "#6896D4", 
             alpha = 1, 
             size = 1)+
  
  #Add a linear model polynomial trendline to the melanophore datapoints
  geom_smooth(aes(x = dpf, y = Melanophore_numbers, color = Morph), method = lm, formula = y ~ poly(x, 2), se = FALSE, size = 2)+
  scale_color_manual(values = cols)+
  labs(title = " ", x ="dpf", y= "Melanophores (n)")+
  
  #Customize graph to remove standard R plot details
  theme(legend.title = element_blank(), 
        legend.position = c(.8, .700),
        legend.background = element_rect(colour = "white", fill = "white"), 
        legend.box.background = element_rect(colour = "white", fill = "white"), 
        legend.key = element_rect(colour = "white", fill = "white"),
        legend.text = element_text(size = 12, colour = "black"), 
        plot.title = element_text(hjust = 0.5, size=12),
        axis.text = element_text(size = 12, colour="black"), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size=1),
        panel.background = element_rect(fill = "white"), 
        axis.title.x = element_text(size=12, margin = margin(t = 10)))               

#Diaplay the plot
melano_plot
