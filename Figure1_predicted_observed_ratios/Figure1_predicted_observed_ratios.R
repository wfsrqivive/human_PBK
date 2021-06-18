library(tidyverse)
library(ggforce)
model_results<-read.csv("../results/PBKresults.csv")

#data preparation figure (calculating the median predicted abserved ratio per compound and adding the number predicted within 5-fold)
figure_data <- model_results %>%
  mutate(pred.obs.ratio = Cplasmavenous/Cmax)   %>%
  group_by(compound) %>%
  mutate(med.pred.obs.ratio = median(pred.obs.ratio)) %>%
  mutate(color = ifelse(med.pred.obs.ratio>5,"Above 5-fold", "Within 5-fold"))%>%
  mutate(color = ifelse(med.pred.obs.ratio<0.2,"Below 5-fold", color))
  

tiff("Figure1_predicted_observed_ratios.tiff", units="in", width=15, height=7, res=150)

p <- ggplot(figure_data, 
            aes(x=compound, y=log10(pred.obs.ratio),
                color = color)) +
  geom_jitter(alpha = 0.6,  show.legend = FALSE) +
  scale_color_manual(values = c("#bdbdbd","#525252")) + 
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) + 
  geom_hline(yintercept=log10(5), color="black", linetype="dashed")+
  geom_hline(yintercept=log10(0.2), color="black", linetype="dashed")+
  scale_y_continuous(name = "Log10 (PBK predicted Cmax/\nobserved Cmax)",
                     limits = c(-4.5, 4.5))+
  guides(color = FALSE) +
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

  

