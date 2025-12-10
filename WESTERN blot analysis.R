#################################Libraries########################
packages= c("FSA","tidyverse","car","plotly","ggsignif","forcats")

for(i in packages){
  require(i, character.only = TRUE)
}

setwd("D:/MscR - Neuroinflammation/Results/R coding/DATA/WESTERN")
blot = read.csv("11225 WB AUC values.csv")

###################################mess with it######################

blot = blot %>%
  mutate(condition = factor(condition,
                            levels = c("CONTROL", "DMSO","PROTAC 6","PROTAC 3",
                                       "PROTAC 1")
  )
  )


blot_all_workable = blot %>%
  group_by(condition) %>% 
  mutate(Normalised = case_when(experiment == 1 ~ value / value[experiment == 3],
                                experiment == 2 ~ value / value[experiment == 3],
                                experiment == 3 ~ 1)) %>%
  ungroup()


nrf2 = blot_all_workable %>%
  filter(experiment == 1)


keap1 = blot_all_workable %>%
  filter(experiment == 2)

################################nrf2 graph#####################################

nrf2 %>%
  ggplot(aes(x=condition, y=Normalised))+
  geom_bar(stat = "identity")+
  #geom_point(data = tnf_all_workable, mapping = aes(x=Condition, y=Normalised))+
  #geom_text(aes(label = Normalised), 
  #          vjust = -0.5, 
  #          size = 3.5, 
  #          fontface = "bold", 
  #          color = "black") +
  scale_x_discrete(labels = c("CONTROL" = "THP-1 control\n 6 hours",
                              "DMSO" = "DMSO 1\u00B5L\n6 hours",
                              "PROTAC 6" = "PROTAC 100 \u00B5M\n6 hours",
                              "PROTAC 3" = "PROTAC 100 \u00B5M\n3 hours",
                              "PROTAC 1" = "PROTAC 100 \u00B5M\n1 hours"))+
  theme_classic()+
  theme(axis.text = element_text(size=11,
                                 colour = "black",
                                 face= "bold"),
        axis.title.y = element_text(size=12,
                                    colour = "black",
                                    face="bold"),
        axis.title.x = element_blank(),
        axis.line = element_line(linewidth = 1),
        axis.ticks = element_line(linewidth = 1),
        axis.text.x = element_text(angle=45,
                                   hjust = 1),
        plot.title = element_text(colour="black",
                                  size=12,
                                  face="bold",
                                  hjust = 0.5))+
  ylab("Relative NFE2L2 protein expression\nnormalised to \u03B2-tubulin expression")+
  scale_y_continuous(limits = c(0,4),expand = c(0,0))+
  ggtitle("NRF2 expression")



##########################################keap1 graph####################################


keap1 %>%
  ggplot(aes(x=condition, y=Normalised))+
  geom_bar(stat = "identity")+
  #geom_point(data = tnf_all_workable, mapping = aes(x=Condition, y=Normalised))+
  #geom_text(aes(label = Normalised), 
  #          vjust = -0.5, 
  #          size = 3.5, 
  #          fontface = "bold", 
  #          color = "black") +
  scale_x_discrete(labels = c("CONTROL" = "THP-1 control\n 6 hours",
                              "DMSO" = "DMSO 1\u00B5L\n6 hours",
                              "PROTAC 6" = "PROTAC 100 \u00B5M\n6 hours",
                              "PROTAC 3" = "PROTAC 100 \u00B5M\n3 hours",
                              "PROTAC 1" = "PROTAC 100 \u00B5M\n1 hours"))+
  theme_classic()+
  theme(axis.text = element_text(size=11,
                                 colour = "black",
                                 face= "bold"),
        axis.title.y = element_text(size=12,
                                    colour = "black",
                                    face="bold"),
        axis.title.x = element_blank(),
        axis.line = element_line(linewidth = 1),
        axis.ticks = element_line(linewidth = 1),
        axis.text.x = element_text(angle=45,
                                   hjust = 1),
        plot.title = element_text(colour="black",
                                  size=12,
                                  face="bold",
                                  hjust = 0.5))+
  ylab("Relative Keap1 protein expression\nnormalised to \u03B2-tubulin expression")+
  scale_y_continuous(limits = c(0,3),expand = c(0,0))+
  ggtitle("Keap1 expression")
