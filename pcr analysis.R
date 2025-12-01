#################################Libraries########################

packages= c("FSA","tidyverse","car","plotly","ggsignif","forcats")

for(i in packages){
  require(i, character.only = TRUE)
}

setwd("D:/MscR - Neuroinflammation/Results/R coding")
pcr = read.csv("NRF2 HO1 NQO1 191125.csv")

###################################mess with it######################

pcr = pcr %>%
  mutate(condition = factor(condition,
                            levels = c("DMSO", "PROTAC 2","PROTAC 4","PROTAC 6",
                                       "PROTAC 8", "Omaveloxolone")
  )
  )

DMSO_average = function(data, Exp){
  output = (data %>%
              filter(Experiment == Exp, condition == "DMSO") %>%
              summarise(av = mean(ct)))$av
  return(output)
}

# Create the DMSO average objects
for (i in 1:3) { assign(paste0("DMSO_av_", i), 
                        DMSO_average(data = pcr,
                                     Exp = as.character(i))) }

pcr_all_workable = pcr %>%
  #Normalise each experiment to their respective DMSO
  mutate(Normalised = case_when(Experiment == 1 ~ (ct / DMSO_av_1),
                                Experiment == 2 ~ (ct / DMSO_av_2),
                                Experiment == 3 ~ (ct / DMSO_av_3)
  ))

nrf2 = pcr_all_workable %>%
  filter(Experiment == 1)


ho1 = pcr_all_workable %>%
  filter(Experiment == 2)


nqo1 = pcr_all_workable %>%
  filter(Experiment == 3)

#tnf_all_workable = tnf_all_workable %>% 
#  mutate(Normalised = round(Normalised, 4))

#tnf_workable$Condition = forcats::fct_rev(IL8_workable$Condition)

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
  scale_x_discrete(labels = c("DMSO" = "DMSO 1 \u00B5L\n 8 hours",
                              "PROTAC 2" = "PROTAC 100 \u00B5M\n2 hours",
                              "PROTAC 4" = "PROTAC 100 \u00B5M\n4 hours",
                              "PROTAC 6" = "PROTAC 100 \u00B5M\n6 hours",
                              "PROTAC 8" = "PROTAC 100 \u00B5M\n8 hours",
                              "Omaveloxolone" = "Omaveloxolone 1 \u00B5M\n6 hours"))+
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
  ylab("Fold induction NFE2L2 mRNA\nnormalised to DMSO")+
  #xlab("Second Extracts")+
  scale_y_continuous(limits = c(0,3),expand = c(0,0))+
  ggtitle("NRF2 expression")



##########################################HO1 graph####################################

ho1 %>%
  ggplot(aes(x=condition, y=Normalised))+
  geom_bar(stat = "identity")+
  #geom_point(data = tnf_all_workable, mapping = aes(x=Condition, y=Normalised))+
  #geom_text(aes(label = Normalised), 
  #          vjust = -0.5, 
  #          size = 3.5, 
  #          fontface = "bold", 
  #          color = "black") +
  scale_x_discrete(labels = c("DMSO" = "DMSO 1 \u00B5L\n 8 hours",
                              "PROTAC 2" = "PROTAC 100 \u00B5M\n2 hours",
                              "PROTAC 4" = "PROTAC 100 \u00B5M\n4 hours",
                              "PROTAC 6" = "PROTAC 100 \u00B5M\n6 hours",
                              "PROTAC 8" = "PROTAC 100 \u00B5M\n8 hours",
                              "Omaveloxolone" = "Omaveloxolone 1 \u00B5M\n6 hours"))+
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
  ylab("Fold induction HO-1 mRNA\nnormalised to DMSO")+
  #xlab("Second Extracts")+
  scale_y_continuous(limits = c(0,100),expand = c(0,0))+
  ggtitle("HO-1 expression")


###################################NQO1 graph#########################


nqo1 %>%
  ggplot(aes(x=condition, y=Normalised))+
  geom_bar(stat = "identity")+
  #geom_point(data = tnf_all_workable, mapping = aes(x=Condition, y=Normalised))+
  #geom_text(aes(label = Normalised), 
  #          vjust = -0.5, 
  #          size = 3.5, 
  #          fontface = "bold", 
  #          color = "black") +
  scale_x_discrete(labels = c("DMSO" = "DMSO 1 \u00B5L\n 8 hours",
                              "PROTAC 2" = "PROTAC 100 \u00B5M\n2 hours",
                              "PROTAC 4" = "PROTAC 100 \u00B5M\n4 hours",
                              "PROTAC 6" = "PROTAC 100 \u00B5M\n6 hours",
                              "PROTAC 8" = "PROTAC 100 \u00B5M\n8 hours",
                              "Omaveloxolone" = "Omaveloxolone 1 \u00B5M\n6 hours"))+
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
  ylab("Fold induction NQO1 mRNA\nnormalised to DMSO")+
  #xlab("Second Extracts")+
  scale_y_continuous(limits = c(0,3),expand = c(0,0))+
  ggtitle("NQO1 expression")
