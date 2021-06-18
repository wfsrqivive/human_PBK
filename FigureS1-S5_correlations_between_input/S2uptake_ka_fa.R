library(tidyverse)
library(ggforce)
library(gridExtra)
library(grid)
model_results<-read.csv("../results/PBKresults.csv")

#data preparation figure
figure_dataKa <-model_results %>% 
  #mutate(Ka = ifelse(Ka == 0, log10(0.1), log10(Ka)))%>%
  distinct(compound, Ka, method.KaFa) %>%
  spread(method.KaFa, Ka) %>%
  as.data.frame()%>%
  rename(in.silico = "in silico")%>%
  rename(in.vitro = "in vitro")

in.vitro_in.silicoKa <- ggscatter(figure_dataKa, x = "in.vitro", y = "in.silico", add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           size =6, 
           label.x.npc = 0, 
           label.y.npc = 1, hjust = 0) +
  geom_text(data = figure_dataKa, 
            aes(x = in.vitro, 
                y = in.silico, label = compound), 
            size = 6, vjust = 0, hjust =0) +
  #labs(tag = "A.")+
  xlab("In vitro Ka (/h)")+
  ylab("In silico Ka (/h)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 18),
        plot.tag.position = "topleft",
        legend.key = element_rect(colour = NA, fill = NA),
        axis.text.x = element_text(size=18,angle = 45, vjust = 1.05, hjust=1.05),
        axis.text.y = element_text(size=18))
in.vitro_in.silicoKa

#data preparation figure
figure_dataFa <-model_results %>% 
  distinct(compound, Fa, method.KaFa) %>%
  spread(method.KaFa, Fa) %>%
  as.data.frame()%>%
  rename(in.silico = "in silico")%>%
  rename(in.vitro = "in vitro")

in.vitro_in.silicoFa <- ggscatter(figure_dataFa, x = "in.vitro", y = "in.silico") +
  #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
  #         size =6, 
  #         label.x.npc = 0, 
  #         label.y.npc = 1, hjust = 0) +
  geom_text(data = figure_dataFa, 
            aes(x = in.vitro, 
                y = in.silico, label = compound), 
            size = 6, vjust = 0, hjust =0) +
  #labs(tag = "B.")+
  xlab("In vitro fraction absorbed")+
  ylab("In silico fraction absorbed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 18),
        plot.tag.position = "topleft",
        legend.key = element_rect(colour = NA, fill = NA),
        axis.text.x = element_text(size=18,angle = 45, vjust = 1.05, hjust=1.05),
        axis.text.y = element_text(size=18))
in.vitro_in.silicoFa


rankCorrKa <- cor.test(figure_dataKa$in.vitro, 
                       figure_dataKa$in.silico,  
                                    method = "spearman",exact=FALSE)
rankCorrKa
rankCorrFa <- cor.test(figure_dataFa$in.vitro, 
                       figure_dataFa$in.silico,  
                       method = "spearman",exact=FALSE)
rankCorrFa

tiff("FigureS2_uptake.tiff", units="in", width=15, height=7, res=300)

grid.arrange(in.vitro_in.silicoKa,
             layout_matrix = rbind(c(1,1,NA,NA),
                                   c(NA,NA,NA,NA)))
dev.off()
