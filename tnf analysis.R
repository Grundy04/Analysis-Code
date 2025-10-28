#################################Libraries########################

packages= c("FSA","tidyverse","car","plotly","ggsignif","forcats")

for(i in packages){
  require(i, character.only = TRUE)
}

setwd("D:/MscR - Neuroinflammation/Results/R coding")
tnf = read.csv("TNF2.csv")

###################################fuck with it######################

tnf = tnf %>%
  mutate(Condition = factor(Condition,
                            levels = c("DMSO", "BAY","TAT14 100um","Omaveloxolone 1um",
                                       "PROTAC 100um", "PROTAC 200um")
  )
  )

DMSO_average = function(data, Exp){
  output = (data %>%
              filter(Experiment == Exp, Condition == "DMSO") %>%
              summarise(av = mean(Standardised)))$av
  return(output)
}

# Create the DMSO average objects
for (i in 1:2) { assign(paste0("DMSO_av_", i), 
                        DMSO_average(data = tnf,
                                     Exp = as.character(i))) }

tnf_all_workable = tnf %>%
  #Normalise each experiment to their respective DMSO
  mutate(Normalised = case_when(Experiment == 1 ~ (Standardised / DMSO_av_1)*100
))
                                
tnf_all_workable = tnf_all_workable %>% 
  mutate(Normalised = round(Normalised, 4))

#tnf_workable$Condition = forcats::fct_rev(IL8_workable$Condition)

################################graph#####################################

tnf_all_workable %>%
  ggplot(aes(x=Condition, y=Normalised))+
  geom_bar(stat = "identity")+
  geom_point(data = tnf_all_workable, mapping = aes(x=Condition, y=Normalised))+
  geom_text(aes(label = Normalised), 
            vjust = -0.5, 
            size = 3.5, 
            fontface = "bold", 
            color = "black") +
  theme_classic()+
  theme(axis.text = element_text(size=11,
                                 colour = "black",
                                 face= "bold"),
        axis.title.y = element_text(size=12,
                                    colour = "black",
                                    face="bold"),
        axis.title.x = element_text(size=12,
                                    colour = "black",
                                    face = "bold"),
        axis.line = element_line(linewidth = 1),
        axis.ticks = element_line(linewidth = 1),
        axis.text.x = element_text(angle=45,
                                   hjust = 1),
        plot.title = element_text(colour="black",
                                  size=12,
                                  face="bold",
                                  hjust = 0.5))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1)+
  ylab("Tnf-alpha inhibtion (%)\nnormalised to DMSO")+
  xlab("Conditions")+
  scale_y_continuous(limits = c(0,250),expand = c(0,0))+
  ggtitle("Tnf-alpha ELISA")
