#including socioeconomic goals, indicators by taxa
goalsIndicator_continent <- Data %>%
  group_by(Continent, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n))

goalsIndicator_continent <- goalsIndicator_continent[-c(1,3,5,7,9,11), ]


relevel(goalsIndicator_continent$Continent, ref = "North America")

continent_logit <- glm(Goals_indicator ~ Continent, data = Data, family = "binomial")
summary(continent_logit)

ggplot(goalsIndicator_continent, aes(y=proportion, x=Continent)) + 
  geom_bar(position="dodge", stat="identity", color = "green", fill = "purple") + ylab("Proportion of Studies that Include HD") + theme(text = element_text(size = 30))




#including socioeconomic goals, indicators by taxa
goalsIndicator_taxa <- Data %>%
  group_by(Taxonomy, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n))

goalsIndicator_taxa <- goalsIndicator_taxa[-c(1,3,5,7,9), ]

taxa_logit <- glm(Goals_indicator ~ Taxonomy, data = Data, family = "binomial")
summary(taxa_logit)

ggplot(goalsIndicator_taxa, aes(y=proportion, x=Taxonomy)) + 
  geom_bar(position="dodge", stat="identity", color = "green", fill = "purple") + ylab("Proportion of Studies that Include HD") + theme(text = element_text(size = 30))





#including socioeconomic goals, indicators, or outreach by zoo groups
goalsIndicator_zoo <- Data %>%
  group_by(`Groups involved_Zoo`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_zoo <- goalsIndicator_zoo[-c(1,2), ]
goalsIndicator_zoo <- mutate(goalsIndicator_zoo, Group = "Zoo", posOutPercent= 0.8961039)



#including socioeconomic goals, indicators, or outreach by academic groups
goalsIndicator_academic <- Data %>%
  group_by(`Groups involved_Academic`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_academic <- goalsIndicator_academic[-c(1,2), ]
goalsIndicator_academic <- mutate(goalsIndicator_academic, Group = "Academic", posOutPercent= 0.8561644)


#including socioeconomic goals, indicators, or outreach by government groups
goalsIndicator_government <- Data %>%
  group_by(`Groups involved_Government`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_government <- goalsIndicator_government[-c(1,2), ]
goalsIndicator_government <- mutate(goalsIndicator_government, Group = "Government", posOutPercent= 0.8947368)

#including socioeconomic goals, indicators, or outreach by indigenous groups
goalsIndicator_indigenous <- Data %>%
  group_by(`Groups involved_Local community/indigenous groups`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_indigenous <- goalsIndicator_indigenous[-c(1,2), ]
goalsIndicator_indigenous <- mutate(goalsIndicator_indigenous, Group = "Indigenous", posOutPercent= 0.96875)

#including socioeconomic goals, indicators, or outreach by non-profit
goalsIndicator_non <- Data %>%
  group_by(`Groups involved_Non-profit`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_non <- goalsIndicator_non[-c(1,2), ]
goalsIndicator_non <- mutate(goalsIndicator_non, Group = "NonProfit", posOutPercent= 0.8734177)

#including socioeconomic goals, indicators, or outreach by private company
goalsIndicator_company <- Data %>%
  group_by(`Groups involved_Private company`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_company <- goalsIndicator_company[-c(1,2), ]
goalsIndicator_company <- mutate(goalsIndicator_company, Group = "PrivateCompany", posOutPercent= 0.8333333)

#including socioeconomic goals, indicators, or outreach by private landowner
goalsIndicator_landowner <- Data %>%
  group_by(`Groups involved_Private landowners`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_landowner <- goalsIndicator_landowner[-c(1,2), ]
goalsIndicator_landowner <- mutate(goalsIndicator_landowner, Group = "PrivateLandowner", posOutPercent= 0.9318182)


AllGroups <- rbind(goalsIndicator_landowner, goalsIndicator_company, goalsIndicator_non, goalsIndicator_indigenous, goalsIndicator_government, goalsIndicator_zoo, goalsIndicator_academic)
AllGroups <- AllGroups[-c(1,3,5,7,9,11,13), ]


ggplot(AllGroups, aes(y=freq, x=reorder(Group, -posOutPercent), fill = as.character(100*round(posOutPercent, 3)))) + 
  geom_bar(position="dodge", stat="identity") + 
  ylab("Proportion of Studies that Include HD") + theme(text = element_text(size = 20)) +
  scale_fill_brewer("Positive Outcome %", palette = "YlOrRd") + xlab("")


#Logistic regression looking at temporal trends
temporal_logit <- glm(Goals_indicator ~ `Reintroduction start year`, data = Data, family = "binomial")
summary(temporal_logit)
confint(temporal_logit)

startYear <- Data[-c(129),]


temporal_logit2 <- glm(Goals_indicator ~ `Reintroduction start year`, data = startYear, family = "binomial")
summary(temporal_logit2)

ggplot(startYear, aes(x=startYear$`Reintroduction start year`, y=startYear$Goals_indicator)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) + xlab("Project Start Year") +
  ylab("Binary Inclusion of HD") + theme(text = element_text(size = 30))



#Threat Type
#including socioeconomic goals, indicator by IUCN threat (climate change)

#subset by IUCN threat (resource use)
resource_use <- subset(Data, `IUCN Threats_Biological resource use` == "TRUE")

resourceUse_outcome <- resource_use %>%
  group_by(Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


#subset by IUCN threat (ag)
agriculture <- subset(Data, `IUCN Threats_Agriculture or aquaculture` == "TRUE")

agriculture_outcome <- agriculture %>%
  group_by(Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


#subset by IUCN threat (climate)
climate <- subset(Data, `IUCN Threats_Climate Change or Severe Weather` == "TRUE")

climate_outcome <- climate %>%
  group_by(Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))



#subset by IUCN threat (energy)
energy <- subset(Data, `IUCN Threats_Energy production or mining` == "TRUE")

energy_outcome <- energy %>%
  group_by(Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))



#subset by IUCN threat (human intrusion)
disturbance <- subset(Data, `IUCN Threats_Human Intrusion and Disturbance` == "TRUE")

disturbance_outcome <- disturbance %>%
  group_by(Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))



#subset by IUCN threat (human intrusion)
invasive <- subset(Data, `IUCN Threats_Invasives or Disease` == "TRUE")

invasive_outcome <- invasive %>%
  group_by(Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))



#subset by IUCN threat (modification)
modification <- subset(Data, `IUCN Threats_Natural System Modifications` == "TRUE")

modification_outcome <- modification %>%
  group_by(Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


#subset by IUCN threat (transportation)
transportation <- subset(Data, `IUCN Threats_Transportation and service corridors` == "TRUE")

transportation_outcome <- transportation %>%
  group_by(Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

#subset by IUCN threat (development)
development <- subset(Data, `IUCN Threats_Residential/commercial development` == "TRUE")

development_outcome <- development %>%
  group_by(Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

#subset by IUCN threat (pollution)
pollution <- subset(Data, `IUCN Threats_Pollution` == "TRUE")

pollution_outcome <- pollution %>%
  group_by(Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))





AllThreats <- rbind(modification_outcome, invasive_outcome, disturbance_outcome, energy_outcome, climate_outcome, agriculture_outcome, resourceUse_outcome, transportation_outcome, development_outcome, pollution_outcome)
AllThreats <- AllThreats[-c(1,3,5,7,9,11,13,15,17,19), ]
Threat <- c("Modification", "Invasives", "Human Disturbance", "Energy Production", "Climate Change", "Agriculture", "Biological Resource Use", "Transportation", "Development", "Pollution")
AllThreats <- cbind(AllThreats, Threat)


ggplot(AllThreats, aes(y=freq, x=Threat)) + 
  geom_bar(position="dodge", stat="identity", color = "green", fill = "purple") + ylab("Proportion of Case Studies that include HD") + xlab("") + theme(text = element_text(size = 15))





#Binary Outcome
goalsIndicator_outcome <- Data %>%
  group_by(outcome_binary, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_outcome <- goalsIndicator_outcome[-c(1,3),]
Outcome <- c("Negative Outcome", "Positive Outcome")
goalsIndicator_outcome <- cbind(Outcome, goalsIndicator_outcome)

ggplot(goalsIndicator_outcome, aes(y=freq, x=...1)) + 
  geom_bar(position="dodge", stat="identity", color = "green", fill = "purple") + ylab("Proportion of Case Studies that include HD") + xlab("") + theme(text = element_text(size = 30))
