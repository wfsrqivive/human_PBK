library(tidyverse)
library(ggforce)
model_results<-read.csv("../model/results/PBKresults.csv")

#calculating per method the number of compounds for which the median predicted Cmax is within 5-fold of the observed Cmax
numberWithin<-model_results %>%
  mutate(pred.obs.ratio = Cplasmavenous/Cmax) %>%
  group_by(compound, method.phys.chem) %>%
  mutate(med.pred.obs.ratio = median(pred.obs.ratio)) %>%
  mutate(color = ifelse(med.pred.obs.ratio>5,"Above 5-fold", "Within 5-fold"))%>%
  mutate(color = ifelse(med.pred.obs.ratio<0.2,"Below 5-fold", color)) %>%
  distinct(method.phys.chem, compound, med.pred.obs.ratio, color) %>%
  group_by(method.phys.chem,color) %>%
  tally() %>%
  filter(color == "Within 5-fold")%>%
  select(-color)

#data preparation figure (calculating the median predicted abserved ratio per compound and adding the number predicted within 5-fold)
figure_data <- model_results %>%
  mutate(pred.obs.ratio = Cplasmavenous/Cmax)   %>%
  group_by(compound) %>%
  mutate(med.pred.obs.ratio = median(pred.obs.ratio)) %>%
  mutate(color = ifelse(med.pred.obs.ratio>5,"Above 5-fold", "Within 5-fold"))%>%
  mutate(color = ifelse(med.pred.obs.ratio<0.2,"Below 5-fold", color))
  
#selection fo the compound for which the input approach results in >3-fold difference in median Cmax 
compounds_Cmax_differs_between_input <- figure_data%>%
  #calculate per input approach the median predicted Cmax
  group_by(compound, method.phys.chem) %>%
  summarise(medianCmaxPerMethod = median(Cplasmavenous))%>%
  #Which input approach has the highest median Cmax and which the lowest, and deterimine the ratio
  mutate(max = max(medianCmaxPerMethod),
         min = min(medianCmaxPerMethod),
         foldDifference = max/min) %>% 
  #If the difference between the input methods is more than 3-fold the results are highlighted in the graph 
  filter(foldDifference>3) %>%
  pull(compound)

difference_between_approaches_selected <- figure_data %>%
  filter(compound %in% compounds_Cmax_differs_between_input) %>%
  left_join(., numberWithin, by = "method.phys.chem") %>%
  mutate(method.phys.chem = paste0(method.phys.chem," ","(",n, ")"))

tiff("Figure3_PhysChem_no_differences.tiff", units="in", width=15, height=5, res=150)

p <- ggplot(figure_data, 
            aes(x=compound, y=log10(pred.obs.ratio),
                color = color)) +
  geom_jitter(alpha = 0.3,  show.legend = FALSE) +
  scale_color_manual(values = c("#bdbdbd","#525252")) + 
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) + 
  geom_hline(yintercept=log10(5), color="black", linetype="dashed")+
  geom_hline(yintercept=log10(0.2), color="black", linetype="dashed")+
  scale_y_continuous(name = "Log10 (PBK predicted Cmax /\nobserved Cmax)",
                     limits = c(-5, 5))+
  guides(color = FALSE) +
  labs(tag = "Physicochemical characteristics") +
  geom_mark_rect(data =difference_between_approaches_selected, aes(x=compound, y=log10(pred.obs.ratio),
                    fill = method.phys.chem), alpha = 0.6, expand = -1) +
  geom_point(data = figure_data,aes(x=compound, 
                                            y=log10(med.pred.obs.ratio)), 
             color = "black", 
             shape = 19, 
             size =3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 18),
        plot.tag.position = "top",
        legend.position="top",
        legend.title = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        axis.text.x = element_text(size=18,angle = 45, vjust = 1.05, hjust=1.05),
        axis.text.y = element_text(size=18),
        axis.title.x = element_blank())
p

dev.off()

  

