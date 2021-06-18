library(tidyverse)
library(ggpubr)
library(gridExtra)
library(grid)
model_results<-read.csv("../model/results/PBKresults.csv")

#data preparation figure
figure_dataLogP <-model_results %>% 
  distinct(compound, logP, method.phys.chem) %>%
  spread(method.phys.chem, logP) 

ADMET_ChemAxonlogP <- ggscatter(figure_dataLogP, x = "ADMET", y = "ChemAxon", add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           size =6, 
           label.x.npc = 0, 
           label.y.npc = 1, hjust = 0) +
  geom_text(data = figure_dataLogP, 
            aes(x = ADMET, 
                y = ChemAxon, label = compound), 
            size = 6, vjust = 0, hjust =0) +
  #labs(tag = "A.")+
  xlab("ADMET logP")+
  ylab("ChemAxon logP")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 18),
        plot.tag.position = "topleft",
        legend.key = element_rect(colour = NA, fill = NA),
        axis.text.x = element_text(size=18,angle = 45, vjust = 1.05, hjust=1.05),
        axis.text.y = element_text(size=18))
ADMET_ChemAxonlogP

#data preparation figure
datafrac.ionized.acid <-model_results %>% 
  distinct(compound, frac.ionized.acid, method.phys.chem) %>%
  spread(method.phys.chem, frac.ionized.acid) 

rankCorrIonizedAcid <- cor.test(datafrac.ionized.acid$ADMET, 
                                datafrac.ionized.acid$ChemAxon,  
                                method = "spearman",exact=FALSE)
rankCorrIonizedAcid

datafrac.ionized.base<-model_results %>% 
  distinct(compound, frac.ionized.base, method.phys.chem) %>%
  spread(method.phys.chem, frac.ionized.base) 

rankCorrIonizedBase <- cor.test(datafrac.ionized.base$ADMET, 
                                datafrac.ionized.base$ChemAxon,  
                                method = "spearman",exact=FALSE)
rankCorrIonizedBase

datafrac.unionized<-model_results %>% 
  distinct(compound, frac.unionized, method.phys.chem) %>%
  spread(method.phys.chem, frac.unionized) 

rankCorrIonizedUnionize <- cor.test(datafrac.unionized$ADMET, 
                                    datafrac.unionized$ChemAxon,  
                                    method = "spearman",exact=FALSE)
rankCorrIonizedUnionize


tiff("FigureS1_physChem.tiff", units="in", width=15, height=7, res=300)

grid.arrange(ADMET_ChemAxonlogP,
             layout_matrix = rbind(c(1,1,NA,NA),
                                   c(NA,NA,NA,NA)))
dev.off()
