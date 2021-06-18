library(tidyverse)
library(ggforce)
library(gridExtra)
library(grid)
model_results<-read.csv("../results/PBKresults.csv")

#data preparation figure
figure_data_Kpad <-model_results %>% 
  group_by(compound, method.partition) %>%
  #mean Kpad based on different fup values
  mutate(Kpad = log10(mean(Kpad))) %>%
  ungroup() %>%
  distinct(compound, Kpad, method.partition) %>%
  spread(method.partition, Kpad) %>%
  as.data.frame()

Berezhkovskiy_RodgersRowland <- ggscatter(figure_data_Kpad, x = "Berezhkovskiy", y = "RodgersRowland", add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           size =6, 
           label.x.npc = 0, 
           label.y.npc = 1, hjust = 0) +
  geom_text(data = figure_data_Kpad, 
            aes(x = Berezhkovskiy, 
                y = RodgersRowland, label = compound), 
            size = 6, vjust = 0, hjust =0) +
  labs(tag = "A.")+
  xlab("Kpad Berezhkovskiy")+
  ylab("Kpad RodgersRowland")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 18),
        plot.tag.position = "topleft",
        legend.key = element_rect(colour = NA, fill = NA),
        axis.text.x = element_text(size=18,angle = 45, vjust = 1.05, hjust=1.05),
        axis.text.y = element_text(size=18))
Berezhkovskiy_RodgersRowland

Berezhkovskiy_Schmitt <- ggscatter(figure_data_Kpad, x = "Berezhkovskiy", y = "Schmitt", add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           size =6, 
           label.x.npc = 0, 
           label.y.npc = 1, hjust = 0) +
  geom_text(data = figure_data_Kpad, 
            aes(x = Berezhkovskiy, 
                y = Schmitt, label = compound), 
            size = 6, vjust = 0, hjust =0) +
  labs(tag = "B.")+
  xlab("Kpad Berezhkovskiy")+
  ylab("Kpad Schmitt")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 18),
        plot.tag.position = "topleft",
        legend.key = element_rect(colour = NA, fill = NA),
        axis.text.x = element_text(size=18,angle = 45, vjust = 1.05, hjust=1.05),
        axis.text.y = element_text(size=18))
Berezhkovskiy_Schmitt

Schmitt_RodgersRowland <- ggscatter(figure_data_Kpad, x = "Schmitt", y = "RodgersRowland", add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           size =6, 
           label.x.npc = 0, 
           label.y.npc = 1, hjust = 0) +
  geom_text(data = figure_data_Kpad, 
            aes(x = Schmitt, 
                y = RodgersRowland, label = compound), 
            size = 6, vjust = 0, hjust =0) +
  labs(tag = "C.")+
  xlab("Kpad Schmitt")+
  ylab("Kpad RodgersRowland")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 18),
        plot.tag.position = "topleft",
        legend.key = element_rect(colour = NA, fill = NA),
        axis.text.x = element_text(size=18,angle = 45, vjust = 1.05, hjust=1.05),
        axis.text.y = element_text(size=18))
Schmitt_RodgersRowland


tiff("FigureS4_Kpad.tiff", units="in", width=30, height=15, res=300)

grid.arrange(Berezhkovskiy_RodgersRowland,Berezhkovskiy_Schmitt,Schmitt_RodgersRowland, 
             layout_matrix = rbind(c(1,1,2,2),
                                   c(NA,3,3,NA)))
dev.off()
