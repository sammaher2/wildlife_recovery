################Recovery Data Analysis##########################

#First need to run all the data cleaning in gabeSpreading_Mitch. Need to end with "Data"



#Load libraries
library(aod)
library(ggplot2)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(gtsummary)
library(jtools)
library(JointAI)
library(multcomp)
library(factorplot)
library(tidyverse)


#Some Basic Summary Stuff. Will need to do more here later
summary(Data$Goals_indicator)
summary(Data$Goals_indicator_outreach)
summary(Data$adminLogistical_Goals_indicator)
summary(Data$species_Goals_indicator)
summary(Data$ecological_Goals_indicator)
summary(Data$socioeconomic_difficulty)
summary(Data$socioeconomic_lessonLearned)


###Global model to see what factors impact the inclusion of HD

#First make sure everything is numeric 1/0s
Data$`IUCN Threats_Biological resource use` <- as.numeric(Data$`IUCN Threats_Biological resource use`)
Data$`IUCN Threats_Agriculture or aquaculture` <- as.numeric(Data$`IUCN Threats_Agriculture or aquaculture`)
Data$`IUCN Threats_Climate Change or Severe Weather` <- as.numeric(Data$`IUCN Threats_Climate Change or Severe Weather`)
Data$`IUCN Threats_Energy production or mining` <- as.numeric(Data$`IUCN Threats_Energy production or mining`)
Data$`IUCN Threats_Human Intrusion and Disturbance` <- as.numeric(Data$`IUCN Threats_Human Intrusion and Disturbance`)
Data$`IUCN Threats_Invasives or Disease` <- as.numeric(Data$`IUCN Threats_Invasives or Disease`)
Data$`IUCN Threats_Natural System Modifications` <- as.numeric(Data$`IUCN Threats_Natural System Modifications`)
Data$`IUCN Threats_Pollution` <- as.numeric(Data$`IUCN Threats_Pollution`)
Data$`IUCN Threats_Residential/commercial development` <- as.numeric(Data$`IUCN Threats_Residential/commercial development`)
Data$`IUCN Threats_Transportation and service corridors` <- as.numeric(Data$`IUCN Threats_Transportation and service corridors`)
Data$`Groups involved_Academic` <- as.numeric(Data$`Groups involved_Academic`)
Data$`Groups involved_Government` <- as.numeric(Data$`Groups involved_Government`)
Data$`Groups involved_Local community/indigenous groups` <- as.numeric(Data$`Groups involved_Local community/indigenous groups`)
Data$`Groups involved_Non-profit` <- as.numeric(Data$`Groups involved_Non-profit`)
Data$`Groups involved_Private company` <- as.numeric(Data$`Groups involved_Private company`)
Data$`Groups involved_Private landowners` <- as.numeric(Data$`Groups involved_Private landowners`)
Data$`Groups involved_Zoo` <- as.numeric(Data$`Groups involved_Zoo`)
Data$`Site Land Type_Protected Area` <- as.numeric(Data$`Site Land Type_Protected Area`)
Data$`Site Land Type_Suburban/Rural` <- as.numeric(Data$`Site Land Type_Suburban/Rural`)
Data$`Site Land Type_Urban` <- as.numeric(Data$`Site Land Type_Urban`)
Data$`Site Land Type_Working Land` <- as.numeric(Data$`Site Land Type_Working Land`)


#Run big global model: I didn't actually used this though
HD_logit <- glm(Goals_indicator ~ Continent + Taxonomy + `Local Conservation Status` + `IUCN Threats_Agriculture or aquaculture` + `IUCN Threats_Biological resource use` + `IUCN Threats_Climate Change or Severe Weather` + `IUCN Threats_Energy production or mining` + `IUCN Threats_Human Intrusion and Disturbance` + `IUCN Threats_Invasives or Disease` + `IUCN Threats_Natural System Modifications` + `IUCN Threats_Pollution` + `IUCN Threats_Residential/commercial development` + `IUCN Threats_Transportation and service corridors` + `Groups involved_Academic` + `Groups involved_Government` + `Groups involved_Local community/indigenous groups` + `Groups involved_Non-profit` + `Groups involved_Private company` + `Groups involved_Private landowners` + `Groups involved_Zoo` + `Site Land Type_Protected Area` +  `Site Land Type_Suburban/Rural` + `Site Land Type_Urban` + `Site Land Type_Working Land` , data = Data, family = "binomial")
summary(HD_logit)


#logistic regression for continents
continent_logit <- glm(Goals_indicator ~ as.factor(Continent), data = Data, family = "binomial")
summary(continent_logit)
confint(contient_logit)
c <- factorplot(continent_logit)
print(c, digits=3, sig = FALSE)
plot(c)

continent_logit2 <- glm(outcome_binary ~ as.factor(Continent), data = Data, family = "binomial")
summary(continent_logit2)
confint(continent_logit2)
c <- factorplot(continent_logit2)
print(c, digits=3, sig = FALSE)
plot(c)


#If you want to relevel anything (but not needed with the pairwise comps)
Data$Continent<-factor(Data$Continent, levels=c("Oceania", "Africa", "Asia", "Europe", "North America", "South America"))


#Plot
cont_plot <- effect_plot(continent_logit, pred = Continent, interval = TRUE, x.label = "Continent", y.label = "Probability of a Including HD", colors = "seagreen", line.thickness = 2)


cont_plot <- data.frame(test[1])

ggplot(cont_plot, aes(y=data.Goals_indicator, x=data.Continent))+
  geom_point(color = "purple", size = 10)+ ## This adds the line
  geom_errorbar(aes(ymax=data.ymax, ymin=data.ymin), width=0.2, color = "black", size = 4)+
  xlab("")+ ## This adds x label
  ylab("Probability of Including HD")+ ## This adds y axis
  theme(text = element_text(size = 15)) + ylim(0,1)



#logistic regression for taxonomy
taxa_logit <- glm(Goals_indicator ~ as.factor(Taxonomy), data = Data, family = "binomial")
summary(taxa_logit)
confint(taxa_logit)
t <- factorplot(taxa_logit)
print(t, digits=3, sig = FALSE)
plot(t)

taxa_logit2 <- glm(outcome_binary ~ as.factor(Taxonomy), data = Data, family = "binomial")
summary(taxa_logit2)
confint(taxa_logit2)
t <- factorplot(taxa_logit2)
print(t, digits=3, sig = FALSE)
plot(t)


#Relevel if you want...again didn't use this
Data$Taxonomy<-factor(Data$Taxonomy, levels=c("Fish", "Reptile", "Amphibian", "Bird", "Mammal"))

#Plot Taxa
plot_taxa <- effect_plot(taxa_logit, pred = Taxonomy, interval =TRUE, x.label = "Taxa", y.label = "Probability of a Including HD", colors = "seagreen", line.thickness = 2)

plot_taxa2 <- data.frame(plot_taxa[1])

ggplot(plot_taxa2, aes(y=data.Goals_indicator, x=data.Taxonomy))+
  geom_point(color = "purple", size = 10)+ ## This adds the line
  geom_errorbar(aes(ymax=data.ymax, ymin=data.ymin), width=0.2, color = "black", size = 4)+
  xlab("")+ ## This adds x label
  ylab("Probability of Including HD")+ ## This adds y axis
  theme(text = element_text(size = 15)) + ylim(0,1)


#Logistic regression to see if case studies with a history of conflict are more likely to include HD
Data$`Conflict/tolerance issue?` <- ifelse(Data$`Conflict/tolerance issue?`=="Yes",1,0)
conflict_logit <- glm(Goals_indicator ~ `Conflict/tolerance issue?`, data = Data, family = "binomial")
summary(conflict_logit)
confint(conflict_logit)


#Logistic regression looking at temporal trends (might want to remove case study from the 20s)
temporal_logit <- glm(Goals_indicator ~ `Reintroduction start year`, data = Data, family = "binomial")
summary(temporal_logit)
confint(temporal_logit)

effect_plot(temporal_logit, pred = `Reintroduction start year`, interval = TRUE, y.label = "Probability of Including HD")


##Logistic Regression looking at the inclusion of HD by positive/negative outcome

outcome3_logit <- glm(outcome_binary ~ Goals_indicator, data = Data, family = "binomial")
summary(outcome3_logit)
confint(outcome3_logit)


outcome_plot <- effect_plot(outcome3_logit, pred = Goals_indicator, interval =TRUE, x.label = "Inclusion of HD", y.label = "Probability of a Positive Outcome", colors = "seagreen", line.thickness = 2)

outcome_plot <- data.frame(outcome_plot[1])

ggplot(outcome_plot, aes(y=data.outcome_binary, x=data.Goals_indicator))+
  geom_point(color = "purple", size = 5)+ ## This adds the line
  geom_errorbar(aes(ymax=data.ymax, ymin=data.ymin), width=0.2, color = "black", size = 2)+
  xlab("Inclusion of HD")+ ## This adds x label
  ylab("Probability of a Positive Outcome")+ ## This adds y axis
  theme(text = element_text(size = 15)) + ylim(.5,1)



#make groups involved long-form. Need to do this because most case studies have multiple groups involved 

groups_involved <- select(Data, outcome_binary, Goals_indicator, `Groups involved`)
groups_involved <- separate_rows(groups_involved, `Groups involved`, sep = ",")
groups_involved$outcome_binary <- ifelse(groups_involved$outcome_binary=="TRUE",1,0)
groups_involved <- groups_involved %>% mutate(across(where(is.character), str_trim))


#logisitc regression for inclusion of HD by groups involved
group_logit <- glm(Goals_indicator ~ as.factor(`Groups involved`), data = groups_involved, family = "binomial")
summary(group_logit)
confint(group_logit)
c <- factorplot(group_logit)
print(c, digits=5, sig = TRUE)
plot(c)

#logisitc regression for outcome by groups involved
group_logit2 <- glm(outcome_binary ~ as.factor(`Groups involved`), data = groups_involved, family = "binomial")
summary(group_logit2)
confint(group_logit2)
c <- factorplot(group_logit2)
print(c, digits=5, sig = TRUE)
plot(c)


#make threats long form. Need to do this because most species have multiple threats
threats <- select(Data, Goals_indicator, `IUCN Threats`, outcome_binary)
threats <- separate_rows(threats, `IUCN Threats`, sep = ",")
threats$Goals_indicator <- ifelse(threats$Goals_indicator=="TRUE",1,0)
threats <- threats %>% mutate(across(where(is.character), str_trim))


threats_logit <- glm(Goals_indicator ~ as.factor(`IUCN Threats`), data = threats, family = "binomial")
summary(threats_logit)
confint(threats_logit)
c <- factorplot(threats_logit)
print(c, digits=5, sig = TRUE)
plot(c)

threats_logit2 <- glm(outcome_binary ~ as.factor(`IUCN Threats`), data = threats, family = "binomial")
summary(threats_logit2)
confint(threats_logit2)
c <- factorplot(threats_logit2)
print(c, digits=5, sig = TRUE)
plot(c)