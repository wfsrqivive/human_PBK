library(tidyverse)
library(ggpubr)
library(gridExtra)
library(grid)
model_results<-read.csv("../model/results/PBKresults.csv")

#data preparation figure
figure_data <-model_results %>% 
  mutate(Clint = ifelse(Clint == 0, log10(0.1), log10(Clint))) %>%
  distinct(compound, Clint, method.Clint) %>%
  spread(method.Clint, Clint) %>%
  as.data.frame()%>%
  rename(in.silico = "in silico")%>%
  mutate(ratio =  (10^in.silico)/(10^hep))

hep_in.silico <- ggscatter(figure_data, x = "hep", y = "in.silico", add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           size =6, 
           label.x.npc = 0, 
           label.y.npc = 1, hjust = 0) +
  geom_text(data = figure_data, 
            aes(x = hep, 
                y = in.silico, label = compound), 
            size = 6, vjust = 0, hjust =0) +
  labs(tag = "A.")+
  xlab("Hepatocytes (logClint scaled to L/hr)")+
  ylab("In silico (logClint scaled to L/hr)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 18),
        plot.tag.position = "topleft",
        legend.key = element_rect(colour = NA, fill = NA),
        axis.text.x = element_text(size=18,angle = 45, vjust = 1.05, hjust=1.05),
        axis.text.y = element_text(size=18))
hep_in.silico

rankCorrhep_in.silico <- cor.test(figure_data$hep, 
                                  figure_data$in.silico,  
                                  method = "spearman",exact=FALSE)
rankCorrhep_in.silico

S9_in.silico <- ggscatter(figure_data, x = "S9", y = "in.silico", add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           size =6, 
           label.x.npc = 0, 
           label.y.npc = 1, hjust = 0) +
  geom_text(data = figure_data, 
            aes(x = S9, 
                y = in.silico, label = compound), 
            size = 6, vjust = 0, hjust =0) +
  labs(tag = "B.")+
  xlab("Hepatocytes (logClint scaled to L/hr)")+
  ylab("In silico (logClint scaled to L/hr)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 18),
        plot.tag.position = "topleft",
        legend.key = element_rect(colour = NA, fill = NA),
        axis.text.x = element_text(size=18,angle = 45, vjust = 1.05, hjust=1.05),
        axis.text.y = element_text(size=18))
S9_in.silico

hep_S9 <- ggscatter(figure_data, x = "hep", y = "S9", add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           size =6, 
           label.x.npc = 0, 
           label.y.npc = 1, hjust = 0) +
  geom_text(data = figure_data, 
            aes(x = hep, 
                y = S9, label = compound), 
            size = 6, vjust = 0, hjust =0) +
  labs(tag = "C.")+
  xlab("Hepatocytes (logClint scaled to L/hr)")+
  ylab("In silico (logClint scaled to L/hr)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 18),
        plot.tag.position = "topleft",
        legend.key = element_rect(colour = NA, fill = NA),
        axis.text.x = element_text(size=18,angle = 45, vjust = 1.05, hjust=1.05),
        axis.text.y = element_text(size=18))
hep_S9


tiff("FigureS5_Clint.tiff", units="in", width=30, height=15, res=300)

grid.arrange(hep_in.silico,S9_in.silico,hep_S9, 
             layout_matrix = rbind(c(1,1,2,2),
                                   c(NA,3,3,NA)))
dev.off()
