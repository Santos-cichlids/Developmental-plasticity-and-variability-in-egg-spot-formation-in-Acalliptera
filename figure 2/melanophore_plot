library(ggplot2)

melano_numbers <- read.csv("Melanophore_coverage.csv", header = TRUE, sep = ",")


melano_plot <- ggplot(data = melano_numbers, 
                      aes(x=dpf, y=Melanophore_numbers, 
                          colour = "black", 
                          fill = "black"))+
  geom_point(alpha=1, size =1.5, colour = "black")+
  geom_rect(aes(xmin=18, xmax=21, ymin=Inf, ymax=-Inf), 
            fill="steelblue2", 
            alpha=0.01, 
            inherit.aes = FALSE) +
  geom_vline(xintercept = 20.17, 
             color = "#999999", 
             alpha = 0.5, 
             size = 1)+
  geom_smooth(method = lm, formula = y ~ poly(x, 3), se = FALSE, colour="black", size = 2)+
  labs(title = " ", x ="dpf", y= "Melanophores (n)")+
  theme (legend.position="none", 
         legend.title = element_blank(), 
         plot.title = element_text(hjust = 0.5, size=12), 
         axis.text = element_text(size = 12, colour="black"), 
         panel.border = element_blank(), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black", size=1), 
         panel.background = element_rect(fill = "white"), 
         axis.title.x = element_text(size=12, margin = margin(t = 10)))

melano_plot
