library(tidyverse)
library(ggpubr)
library(gridExtra)
library(grid)
model_results<-read.csv("../model/results/PBKresults.csv")

#data preparation figure
figure_dataFup <-model_results %>% 
  group_by(compound, method.Fup) %>%
  #mean Fup based on different phys chem values
  mutate(Fup = log10(mean(Fup))) %>%
  distinct(compound, Fup, method.Fup) %>%
  spread(method.Fup, Fup) %>%
  as.data.frame()%>%
  rename(in.silico = "in silico")%>%
  rename(in.vitro = "in vitro")

in.vitro_in.silicoFup <- ggscatter(figure_dataFup, x = "in.vitro", y = "in.silico", add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           size =6, 
           label.x.npc = 0, 
           label.y.npc = 1, hjust = 0) +
  geom_text(data = figure_dataFup, 
            aes(x = in.vitro, 
                y = in.silico, label = compound), 
            size = 6, vjust = 0, hjust =0) +
  #labs(tag = "A.")+
  xlab("In vitro Fup")+
  ylab("In silico Fup")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 18),
        plot.tag.position = "topleft",
        legend.key = element_rect(colour = NA, fill = NA),
        axis.text.x = element_text(size=18,angle = 45, vjust = 1.05, hjust=1.05),
        axis.text.y = element_text(size=18))
in.vitro_in.silicoFup


tiff("FigureS3_Fup.tiff", units="in", width=15, height=7, res=300)

grid.arrange(in.vitro_in.silicoFup,
             layout_matrix = rbind(c(1,1,NA,NA),
                                   c(NA,NA,NA,NA)))
dev.off()
