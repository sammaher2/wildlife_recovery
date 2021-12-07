library(data.table)
library(tidyverse)
library(Hmisc)
library(ggplot2)

setwd("C:/Users/mitch/OneDrive/Desktop")

#temporary datasheet without 2021 case studies
datasheet <- fread("edited_recovery2.csv")

library(splitstackshape)

#list from data cleaning document
colsToSplit <- c("IUCN Threats", "Goals (Ecological)", "Goals (Socioeconomic)", "Goals (Admin/Logistical)",
                 "Success Indicators (Ecological)", "Success Indicators (Socioeconomic)", 
                 "Success Indicators (Admin/Logistical)",  "Translocation Type", 
                 "Captive/wild/rehab", "Post Release","Land ownership", "Site Land Type", "Groups involved", "Outreach", 
                 "Major difficulties faced (Ecological)",      "Major difficulties faced (Socioeconomic)" , 
                 "Major difficulties faced (Admin/Logistic)" , "Lessons learned (Ecological)" ,             
                 "Lessons learned (Socioeconomic)",  "Lessons learned (Admin/Logistic)",  "Reasons cited for success (Ecological)", 
                 "Reasons Cited for Success (Socioeconomic)", "Reasons cited for success (Admin/Logistic)",
                 "Local threats", "Outcome")

#function to split column into multiple columns containing 0/1 values for each unique value in original column
splitColumn <- function (colName) {
  datasheet %>% select(colName) %>% cSplit_e(1, ",", type = "character", fill = 0, drop = TRUE)
}

#merged all split up columns, changed to T/F for easier summary column generation,
#will convert back to 1/0 at the end
datasheetSplit <- map_dfc(colsToSplit, splitColumn) %>% mutate(across(everything(), as.logical))

#TO DO: decide what informational columns and columns not part of "colsToSplit" to include in final sheet


##GOALS

#Species Level Goal: “Reinforce Population”, “Restore Population” “Connect Populations” 
#“Population Growth” “Behavior” “Genetics” “Distribution” “Survival” “Reproduction”
datasheetSplit <- datasheetSplit %>%
  mutate(speciesLevelGoal = ifelse(`Goals (Ecological)_Reinforce Population` |
                                     `Goals (Ecological)_Restore Population` |
                                     `Goals (Ecological)_Connect Populations` |
                                     `Goals (Ecological)_Population Growth` |
                                     `Goals (Ecological)_Behavior` |
                                     `Goals (Ecological)_Genetics` |
                                     `Goals (Ecological)_Distribution` |
                                     `Goals (Ecological)_Survival` |
                                     `Goals (Ecological)_Reproduction`, T, F))

#Ecological Goal: “Habitat”, “Interaction”, “Function”
datasheetSplit <- datasheetSplit %>%
  mutate(ecologicalGoal = ifelse(`Goals (Ecological)_Habitat` |
                                   `Goals (Ecological)_Interaction` |
                                   `Goals (Ecological)_Function`, T, F))

#Socioeconomic Goal: if anything is included (including other)
datasheetSplit <- datasheetSplit %>%
  mutate(socioeconomicGoal = ifelse(`Goals (Socioeconomic)_Cultural Benefit` |
                                      `Goals (Socioeconomic)_Economic Benefit` |
                                      `Goals (Socioeconomic)_Education` |
                                      `Goals (Socioeconomic)_Engage Locals in Conservation` |
                                      `Goals (Socioeconomic)_Increase awareness` |
                                      `Goals (Socioeconomic)_Increase Social Tolerance` |
                                      `Goals (Socioeconomic)_Law Enforcement` |
                                      `Goals (Socioeconomic)_other`, T, F))

#Admin/Logistical Goal: if anything is included (including other)
datasheetSplit <- datasheetSplit %>%
  mutate(adminLogisticalGoal = ifelse(`Goals (Admin/Logistical)_Funding` |
                                        `Goals (Admin/Logistical)_Husbandry` |
                                        `Goals (Admin/Logistical)_Institutional` |
                                        `Goals (Admin/Logistical)_Procedural` |
                                        `Goals (Admin/Logistical)_Reduce Human Intervention`, T, F))

##INDICATORS

#Species Level Indicator: “Reinforce Population”, “Restore Population”,
#“Connected Populations” “Increase Population Growth” “Behavior”,
#“Genetics” “Distribution” “Survival” “Reproduction”
datasheetSplit <- datasheetSplit %>%
  mutate(speciesLevelIndicator = ifelse(`Success Indicators (Ecological)_Reinforce Population` |
                                          `Success Indicators (Ecological)_Restore Population` |
                                          `Success Indicators (Ecological)_Connected Populations` |
                                          `Success Indicators (Ecological)_Increase Population Growth` |
                                          `Success Indicators (Ecological)_Behavior` |
                                          `Success Indicators (Ecological)_Genetics` |
                                          `Success Indicators (Ecological)_Distribution` |
                                          `Success Indicators (Ecological)_Survival` |
                                          `Success Indicators (Ecological)_Reproduction`, T, F))

#Ecological Indicator: “Habitat”, “Interaction”, “Function”
datasheetSplit <- datasheetSplit %>%
  mutate(ecologicalIndicator = ifelse(`Success Indicators (Ecological)_Habitat` |
                                        `Success Indicators (Ecological)_Interaction` |
                                        `Success Indicators (Ecological)_Function`, T, F))

#Socioeconomic Indicator: if anything is included (including other)
datasheetSplit <- datasheetSplit %>%
  mutate(socioeconomicIndicator = ifelse(`Success Indicators (Socioeconomic)_Cultural Benefit` |
                                           `Success Indicators (Socioeconomic)_Economic Benefit` |
                                           `Success Indicators (Socioeconomic)_Increase Public Awareness` |
                                           `Success Indicators (Socioeconomic)_Increase Social Tolerance` |
                                           `Success Indicators (Socioeconomic)_Legal Protection` |
                                           `Success Indicators (Socioeconomic)_Public Involvement`, T, F))

#Admin/Logistical Indicator: if anything is included (including other)
datasheetSplit <- datasheetSplit %>%
  mutate(adminLogisticalIndicator = ifelse(`Success Indicators (Admin/Logistical)_Husbandry` |
                                             `Success Indicators (Admin/Logistical)_Institutional` |
                                             `Success Indicators (Admin/Logistical)_Procedural` |
                                             `Success Indicators (Admin/Logistical)_Reduce Human Intervention`, T, F))

#Species Level Difficulty: “Reinforce Population”, “Restore Population”, “Connect Populations”
#"Population Growth” “Behavior” “Genetics” “Distribution” “Survival” “Reproduction”
datasheetSplit <- datasheetSplit %>%
  mutate(speciesLevelDifficulty = ifelse(`Major difficulties faced (Ecological)_Reinforce Population` |
                                           `Major difficulties faced (Ecological)_Restore Population` |
                                           `Major difficulties faced (Ecological)_Connect Populations` |
                                           `Major difficulties faced (Ecological)_Population Growth` |
                                           `Major difficulties faced (Ecological)_Behavior` |
                                           `Major difficulties faced (Ecological)_Genetics` |
                                           `Major difficulties faced (Ecological)_Distribution` |
                                           `Major difficulties faced (Ecological)_Survival` |
                                           `Major difficulties faced (Ecological)_Reproduction`, T, F))

#Ecological Difficulty: “Habitat”, “Interaction”, “Function”
datasheetSplit <- datasheetSplit %>%
  mutate(ecologicalDifficulties = ifelse(`Major difficulties faced (Ecological)_Habitat` |
                                           `Major difficulties faced (Ecological)_Interaction` |
                                           `Major difficulties faced (Ecological)_Function`, T, F))

#Socioeconomic Difficulty: if anything is included (including other)
datasheetSplit <- datasheetSplit %>%
  mutate(socioeconomicDifficulty = ifelse(`Major difficulties faced (Socioeconomic)_Acceptance/Social Tolerance` |
                                            `Major difficulties faced (Socioeconomic)_Economic Benefit` |
                                            `Major difficulties faced (Socioeconomic)_Human disturbance` |
                                            `Major difficulties faced (Socioeconomic)_human-caused mortality` |
                                            `Major difficulties faced (Socioeconomic)_human-casued mortality` |
                                            `Major difficulties faced (Socioeconomic)_Legal Involvement` |
                                            `Major difficulties faced (Socioeconomic)_Legal Protection` |
                                            `Major difficulties faced (Socioeconomic)_other` |
                                            `Major difficulties faced (Socioeconomic)_Public Awareness` |
                                            `Major difficulties faced (Socioeconomic)_Public Involvement`, T, F))

##LESSONS LEARNED

#Species Level Lesson Learned: “Reinforce Population”, “Restore Population”,
#“Connect Populations” “Population Growth” “Behavior” “Genetics” “Distribution”,
#“Survival” “Reproduction”
datasheetSplit <- datasheetSplit %>%
  mutate(speciesLevelLessons = ifelse(`Lessons learned (Ecological)_Reinforce Population` |
                                        `Lessons learned (Ecological)_Restore Population` |
                                        `Lessons learned (Ecological)_Connect Populations` |
                                        `Lessons learned (Ecological)_Population Growth` |
                                        `Lessons learned (Ecological)_Behavior` |
                                        `Lessons learned (Ecological)_Genetics` |
                                        `Lessons learned (Ecological)_Distribution` |
                                        `Lessons learned (Ecological)_Survival` |
                                        `Lessons learned (Ecological)_Reproduction`, T, F))

#Ecological Lesson Learned: “Habitat”, “Interaction”, “Function”
datasheetSplit <- datasheetSplit %>%
  mutate(ecologicalLessons = ifelse(`Lessons learned (Ecological)_Habitat` |
                                      `Lessons learned (Ecological)_Interaction` |
                                      `Lessons learned (Ecological)_Function`, T, F))

#Socioeconomic Lesson Learned: if anything is included (including other)
datasheetSplit <- datasheetSplit %>%
  mutate(socioeconomicLessons = ifelse(`Lessons learned (Socioeconomic)_Increase Acceptance/Social Tolerance` |
                                         `Lessons learned (Socioeconomic)_Economic Benefit` |
                                         `Lessons learned (Socioeconomic)_Investing in human-wildlife conflict mitigation is critical` |
                                         `Lessons learned (Socioeconomic)_Cultural Benefit` |
                                         `Lessons learned (Socioeconomic)_Legal Protection` |
                                         `Lessons learned (Socioeconomic)_other` |
                                         `Lessons learned (Socioeconomic)_Increase Public Awareness` |
                                         `Lessons learned (Socioeconomic)_Public Involvement`, T, F))

#Admin/Logistical Lesson Learned: if anything is included (including other)
datasheetSplit <- datasheetSplit %>%
  mutate(adminLogisticalLessons = ifelse(`Lessons learned (Admin/Logistic)_Husbandry` |
                                           `Lessons learned (Admin/Logistic)_devoted leader` |
                                           `Lessons learned (Admin/Logistic)_Funding` |
                                           `Lessons learned (Admin/Logistic)_Institutional` |
                                           `Lessons learned (Admin/Logistic)_Procedural` |
                                           `Lessons learned (Admin/Logistic)_Reduce Human Intervention`, T, F))

##REASONED CITED FOR SUCCESS

#Species Level Success: “Reinforce Population”, “Restore Population”, “Connected Populations”,
#“Population Growth” “Restored Behavior” “Restored Genetics” “Increased Distribution”
#“Survival” “Reproduction”
datasheetSplit <- datasheetSplit %>%
  mutate(speciesLevelReasons = ifelse(`Reasons cited for success (Ecological)_Reinforce Population` |
                                        `Reasons cited for success (Ecological)_Restore Population` |
                                        `Reasons cited for success (Ecological)_Connected Populations` |
                                        `Reasons cited for success (Ecological)_Population Growth` |
                                        `Reasons cited for success (Ecological)_Restored Behavior` |
                                        `Reasons cited for success (Ecological)_Restored Genetics` |
                                        `Reasons cited for success (Ecological)_Increased Distribution` |
                                        `Reasons cited for success (Ecological)_Survival` |
                                        `Reasons cited for success (Ecological)_Reproduction`, T, F))

#Ecological Success: “Restored Habitat”, “Restored Interaction”, “ Restored Function”
datasheetSplit <- datasheetSplit %>%
  mutate(ecologicalReasons = ifelse(`Reasons cited for success (Ecological)_Restored Habitat` |
                                      `Reasons cited for success (Ecological)_Restored Interaction` |
                                      `Reasons cited for success (Ecological)_Restored Function`, T, F))

#Socioeconomic Success: if anything is included (including other)
datasheetSplit <- datasheetSplit %>%
  mutate(socioeconomicReasons = ifelse(`Reasons Cited for Success (Socioeconomic)_Cattle production` |
                                         `Reasons Cited for Success (Socioeconomic)_Cultural Benefit` |
                                         `Reasons Cited for Success (Socioeconomic)_Economic Benefit` |
                                         `Reasons Cited for Success (Socioeconomic)_Increase Acceptance/Social Tolerance` |
                                         `Reasons Cited for Success (Socioeconomic)_Increase Public Awareness` |
                                         `Reasons Cited for Success (Socioeconomic)_Legal Protection` |
                                         `Reasons Cited for Success (Socioeconomic)_litigation against the program` |
                                         `Reasons Cited for Success (Socioeconomic)_other` |
                                         `Reasons Cited for Success (Socioeconomic)_political difficulties` |
                                         `Reasons Cited for Success (Socioeconomic)_Public Involvement`, T, F))

#Admin/Logistical Success: if anything is included (including other)
datasheetSplit <- datasheetSplit %>%
  mutate(adminLogisticalReasons = ifelse(`Lessons learned (Admin/Logistic)_Husbandry` |
                                           `Lessons learned (Admin/Logistic)_Funding` |
                                           `Lessons learned (Admin/Logistic)_Institutional` |
                                           `Lessons learned (Admin/Logistic)_Procedural` |
                                           `Lessons learned (Admin/Logistic)_Reduce Human Intervention`, T, F))

##SOCIOECONOMIC SUMMARIES

#Goals_indicator: If the case study included a socio-economic goal or success indicator
datasheetSplit <- datasheetSplit %>%
  mutate(Goals_indicator = ifelse(socioeconomicGoal | socioeconomicIndicator, T, F))

#Outreach: any outreach
datasheetSplit <- datasheetSplit %>%
  mutate(outreach = ifelse(`Outreach_Education/Capacity Building` |
                             `Outreach_Indigenous groups engagement` |
                             `Outreach_landowner engagement` |
                             `Outreach_landowner incentives` |
                             `Outreach_Local Outreach` |
                             `Outreach_Media Campaign`, T, F))

#Goals_indicator_outreach: if the case study included a socio-economic goal, 
#success indicator, or had any outreach
datasheetSplit <- datasheetSplit %>%
  mutate(Goals_indicator_outreach = ifelse(socioeconomicGoal | socioeconomicIndicator | 
                                             outreach, T, F))


#socioeconomic_difficulty: if the case study did not include a socio-economic goal 
#or success indicator, but cited a socio-economic difficulty 
datasheetSplit <- datasheetSplit %>%
  mutate(socioeconomic_difficulty = ifelse(!socioeconomicGoal & !socioeconomicIndicator &
                                             socioeconomicDifficulty, T, F))

#Socioeconomic2_difficulty: if the case study did not include a socio-economic goal, 
#indicator and did not have any outreach, but cited a socio-economic difficulty
datasheetSplit <- datasheetSplit %>%
  mutate(socioeconomic2_difficulty = ifelse(!socioeconomicGoal & !socioeconomicIndicator &
                                              !outreach & socioeconomicDifficulty, T, F))

#socioeconomic_lessonLearned: if the case study did not include a socio-economic goal 
#or success indicator, but cited a socio-economic lesson learned  
datasheetSplit <- datasheetSplit %>%
  mutate(socioeconomic_lessonLearned = ifelse(!socioeconomicGoal & !socioeconomicIndicator &
                                                socioeconomicLessons, T, F))

#Socioeconomic2_lessonLearned: if the case study did not include a socio-economic goal, 
#indicator and did not have any outreach, but cited a socio-economic lesson learned
datasheetSplit <- datasheetSplit %>%
  mutate(socioeconomic2_lessonLearned = ifelse(!socioeconomicGoal & !socioeconomicIndicator &
                                                 !outreach & socioeconomicLessons, T, F))

#socioeconomic_success: if the case study did not include a socio-economic goal or 
#success indicator, but cited a socio-economic reason for success  
datasheetSplit <- datasheetSplit %>%
  mutate(socioeconomic_success = ifelse(!socioeconomicGoal & !socioeconomicIndicator &
                                          socioeconomicReasons, T, F))

#Socioeconomic2_success: if the case study did not include a socio-economic goal, 
#indicator and did not have any outreach, but cited a socio-economic reason for success
datasheetSplit <- datasheetSplit %>%
  mutate(socioeconomic2_success = ifelse(!socioeconomicGoal & !socioeconomicIndicator &
                                           !outreach & socioeconomicReasons, T, F))


#Outcome summaries
datasheetSplit <- datasheetSplit %>%
  mutate(outcome_binary = ifelse(`Outcome_Reproduction` |
                             `Outcome_Viable population` |
                             `Outcome_Population increase` |
                             `Outcome_Survival`, T, F))



#Make species-level, ecological, and admin summaries 
datasheetSplit <- datasheetSplit %>%
  mutate(species_Goals_indicator = ifelse(speciesLevelGoal | speciesLevelIndicator, T, F))

datasheetSplit <- datasheetSplit %>%
  mutate(ecological_Goals_indicator = ifelse(ecologicalGoal | ecologicalIndicator, T, F))

datasheetSplit <- datasheetSplit %>%
  mutate(adminLogistical_Goals_indicator = ifelse(adminLogisticalGoal | adminLogisticalIndicator, T, F))




#DATA VIZZZZZZ

#combine two dataframes
Data <- cbind(datasheet, datasheetSplit)

#Change T/F to 1/0
Data$Goals_indicator <- as.factor(Data$Goals_indicator)
Data$Goals_indicator <- ifelse(Data$Goals_indicator=="TRUE",1,0)



#including socioeconomic goals or indicators by success 
goalsIndicator_success <- Data %>%
  group_by(Success, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(goalsIndicator_success, aes(fill=Goals_indicator, y=freq, x=Success)) + 
  geom_bar(position="dodge", stat="identity") 


#including socioeconomic goals or indicators by outcome
goalsIndicator_outcome <- Data %>%
  group_by(Outcome, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(goalsIndicator_outcome, aes(fill=Goals_indicator, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity") 

#including population goals by outcome
population_goals_outcome <- Data %>%
  group_by(Outcome, speciesLevelGoal) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(population_goals_outcome, aes(fill=speciesLevelGoal, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity") 

#including population indicators by outcome
population_indicator_outcome <- Data %>%
  group_by(Outcome, speciesLevelIndicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(population_indicator_outcome, aes(fill=speciesLevelIndicator, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity")

#including ecological goals by outcome
ecological_goals_outcome <- Data %>%
  group_by(Outcome, ecologicalGoal) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(ecological_goals_outcome, aes(fill=ecologicalGoal, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity") 

#including ecological indicator by outcome
ecological_indicator_outcome <- Data %>%
  group_by(Outcome, ecologicalIndicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(ecological_indicator_outcome, aes(fill=ecologicalIndicator, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity") 

#including admin/logistic goals by outcome
logistical_goals_outcome <- Data %>%
  group_by(Outcome, adminLogisticalGoal) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(logistical_goals_outcome, aes(fill=adminLogisticalGoal, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity")

#including admin/logistic goals by indicator
logistical_indicator_outcome <- Data %>%
  group_by(Outcome, adminLogisticalIndicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(logistical_indicator_outcome, aes(fill=adminLogisticalIndicator, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity") 

#incluing socioeconomic goals, indicators or outreach by success
goalsIndicatorOutreach_success <- Data %>%
  group_by(Success, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(goalsIndicatorOutreach_success, aes(fill=Goals_indicator_outreach, y=freq, x=Success)) + 
  geom_bar(position="dodge", stat="identity") 

#incluing socioeconomic goals, indicators or outreach by outcome
goalsIndicatorOutreach_outcome <- Data %>%
  group_by(Outcome, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(goalsIndicatorOutreach_outcome, aes(fill=Goals_indicator_outreach, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity") 

#including socioeconomic goals, indicators, or outreach by taxa
goalsIndicator_taxa <- Data %>%
  group_by(Taxonomy, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n))

goalsIndicator_taxa <- goalsIndicator_taxa[-c(1,3,5,7,9), ]

taxa_logit <- glm(Goals_indicator ~ Taxonomy, data = Data, family = "binomial")
summary(taxa_logit)

ggplot(goalsIndicator_taxa, aes(y=proportion, x=Taxonomy)) + 
  geom_bar(position="dodge", stat="identity", color = "green", fill = "purple") + ylab("Proportion of Studies that Include Human Dimensions") + theme(text = element_text(size = 30))


#including socioeconomic goals, indicators, or outreach by continent
goalsIndicatorOutreach_contient <- Data %>%
  group_by(Continent, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


ggplot(goalsIndicatorOutreach_continent, aes(fill=Goals_indicator_outreach, y=freq, x=Continent)) + 
  geom_bar(position="dodge", stat="identity") 

#including socioeconomic goals, indicators, or outreach by conflict
goalsIndicatorOutreach_conflict <- Data %>%
  group_by(`Conflict/tolerance issue?`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(goalsIndicatorOutreach_conflict, aes(fill=Goals_indicator_outreach, y=freq, x=`Conflict/tolerance issue?`)) + 
  geom_bar(position="dodge", stat="identity")


#did not include human dimensions, but cited as a difficulty by outcome
goalsIndicatorOutreach_difficulty <- Data %>%
  group_by(Outcome, socioeconomic2_difficulty) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(goalsIndicatorOutreach_difficulty, aes(fill=socioeconomic2_difficulty, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity")


#did not include human dimensions, but cited as a lesson learned by outcome
goalsIndicatorOutreach_lesson <- Data %>%
  group_by(Outcome, socioeconomic2_lessonLearned) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(goalsIndicatorOutreach_lesson, aes(fill=socioeconomic2_lessonLearned, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity")


#including socioeconomic goals, indicators, or outreach by zoo groups
goalsIndicator_zoo <- Data %>%
  group_by(`Groups involved_Zoo`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_zoo <- goalsIndicator_zoo[-c(1,2), ]

zoo <- ggplot(goalsIndicator_zoo, aes(fill=Goals_indicator, y=freq, x=`Groups involved_Zoo`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, or outreach by academic groups
goalsIndicator_academic <- Data %>%
  group_by(`Groups involved_Academic`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_academic <- goalsIndicator_academic[-c(1,2), ]

academic <- ggplot(goalsIndicator_academic, aes(fill=Goals_indicator, y=freq, x=`Groups involved_Academic`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, or outreach by government groups
goalsIndicator_government <- Data %>%
  group_by(`Groups involved_Government`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_government <- goalsIndicator_government[-c(1,2), ]

government <- ggplot(goalsIndicator_government, aes(fill=Goals_indicator, y=freq, x=`Groups involved_Government`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, or outreach by indigenous groups
goalsIndicator_indigenous <- Data %>%
  group_by(`Groups involved_Local community/indigenous groups`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_indigenous <- goalsIndicator_indigenous[-c(1,2), ]

indigenous <- ggplot(goalsIndicator_indigenous, aes(fill=Goals_indicator, y=freq, x=`Groups involved_Local community/indigenous groups`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, or outreach by non-profit
goalsIndicator_non <- Data %>%
  group_by(`Groups involved_Non-profit`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_non <- goalsIndicator_non[-c(1,2), ]

non_profit <- ggplot(goalsIndicator_non, aes(fill=Goals_indicator, y=freq, x=`Groups involved_Non-profit`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, or outreach by private company
goalsIndicator_company <- Data %>%
  group_by(`Groups involved_Private company`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_company <- goalsIndicator_company[-c(1,2), ]

private_company <- ggplot(goalsIndicator_company, aes(fill=Goals_indicator, y=freq, x=`Groups involved_Private company`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, or outreach by private landowner
goalsIndicator_landowner <- Data %>%
  group_by(`Groups involved_Private landowners`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_landowner <- goalsIndicator_landowner[-c(1,2), ]

landowner <- ggplot(goalsIndicator_landowner, aes(fill=Goals_indicator, y=freq, x=`Groups involved_Private landowners`)) + 
  geom_bar(position="dodge", stat="identity")

#combine plots of groups involved 
library(ggpubr)
ggarrange(landowner, private_company, zoo, academic, non_profit, indigenous, government )


#SITE LAND TYPE VIZ

#including socioeconomic goals, indicators, outreach for protected areas
goalsIndicatorOutreach_pa <- Data %>%
  group_by(`Site Land Type_Protected Area`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_pa <- goalsIndicatorOutreach_pa[-c(1,2), ]

pa <- ggplot(goalsIndicatorOutreach_pa, aes(fill=Goals_indicator_outreach, y=freq, x=`Site Land Type_Protected Area`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, outreach for working lands
goalsIndicatorOutreach_working <- Data %>%
  group_by(`Site Land Type_Working Land`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_working  <- goalsIndicatorOutreach_working [-c(1,2), ]

working <- ggplot(goalsIndicatorOutreach_working , aes(fill=Goals_indicator_outreach, y=freq, x=`Site Land Type_Working Land`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, outreach for suburban/rural 
goalsIndicatorOutreach_rural <- Data %>%
  group_by(`Site Land Type_Suburban/Rural`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_rural  <- goalsIndicatorOutreach_rural [-c(1,2), ]

rural <- ggplot(goalsIndicatorOutreach_rural , aes(fill=Goals_indicator_outreach, y=freq, x=`Site Land Type_Suburban/Rural`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, outreach for urban
goalsIndicatorOutreach_urban <- Data %>%
  group_by(`Site Land Type_Urban`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_urban  <- goalsIndicatorOutreach_urban [-c(1,2), ]

urban <- ggplot(goalsIndicatorOutreach_urban , aes(fill=Goals_indicator_outreach, y=freq, x=`Site Land Type_Urban`)) + 
  geom_bar(position="dodge", stat="identity")

#combine all plots site land type plots
ggarrange(pa, working, urban, rural)

#LAND OWNERSHIP FIGURES

#including socioeconomic goals, indicators, outreach by community ownership
goalsIndicatorOutreach_community <- Data %>%
  group_by(`Land ownership_Community (multiple private individuals or entities)`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_community  <- goalsIndicatorOutreach_community [-c(1,2), ]

community <- ggplot(goalsIndicatorOutreach_community , aes(fill=Goals_indicator_outreach, y=freq, x=`Land ownership_Community (multiple private individuals or entities)`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, outreach by federal ownership
goalsIndicatorOutreach_federal <- Data %>%
  group_by(`Land ownership_Federal`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_federal  <- goalsIndicatorOutreach_federal [-c(1,2), ]

federal <- ggplot(goalsIndicatorOutreach_federal , aes(fill=Goals_indicator_outreach, y=freq, x=`Land ownership_Federal`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, outreach by federal ownership
goalsIndicatorOutreach_federal <- Data %>%
  group_by(`Land ownership_Federal`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_federal  <- goalsIndicatorOutreach_federal [-c(1,2), ]

federal <- ggplot(goalsIndicatorOutreach_federal , aes(fill=Goals_indicator_outreach, y=freq, x=`Land ownership_Federal`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, outreach by State ownership
goalsIndicatorOutreach_state <- Data %>%
  group_by(`Land ownership_State`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_state   <- goalsIndicatorOutreach_state  [-c(1,2), ]

state <- ggplot(goalsIndicatorOutreach_state , aes(fill=Goals_indicator_outreach, y=freq, x=`Land ownership_State`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, outreach by State ownership
goalsIndicatorOutreach_private <- Data %>%
  group_by(`Land ownership_Private (1 owner)`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_private   <- goalsIndicatorOutreach_private  [-c(1,2), ]

private <- ggplot(goalsIndicatorOutreach_private , aes(fill=Goals_indicator_outreach, y=freq, x=`Land ownership_Private (1 owner)`)) + 
  geom_bar(position="dodge", stat="identity")



#Subset amphibians from case studies
no_amp <- subset(Data, Taxonomy != "Amphibian")

goalsIndicatorOutreach_outcome <- no_amp %>%
  group_by(Outcome, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(goalsIndicatorOutreach_outcome, aes(fill=Goals_indicator_outreach, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity") 


#Quick Logistic Regression and plot by years
goalsIndicatorOutreach_year <- Data %>%
  group_by(`Publication Year`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(goalsIndicatorOutreach_year, aes(fill=Goals_indicator_outreach, y=freq, x=`Publication Year`)) + 
  geom_bar(position="dodge", stat="identity")

model <- glm(Goals_indicator_outreach ~ `Reintroduction start year`, data = Data, family = binomial)
summary(model)


newdata <- data.frame(Year=seq(min(Data$'Reintroduction start year'), max(Data$'Reintroduction start year'),len=500))


#VIZ BY LOCAL THREAT (human dimensions)

#including socioeconomic goals, indicators, outreach by threat (ag)
goalsIndicatorOutreach_ag <- Data %>%
  group_by(`Local threats_Agriculture or aquaculture`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_ag   <- goalsIndicatorOutreach_ag  [-c(1,2), ]

ag <- ggplot(goalsIndicatorOutreach_ag , aes(fill=Goals_indicator_outreach, y=freq, x=`Local threats_Agriculture or aquaculture`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, outreach by threat (resource use)
goalsIndicatorOutreach_use <- Data %>%
  group_by(`Local threats_Biological resource use`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_use   <- goalsIndicatorOutreach_use  [-c(1,2), ]

use <- ggplot(goalsIndicatorOutreach_use , aes(fill=Goals_indicator_outreach, y=freq, x=`Local threats_Biological resource use`)) + 
  geom_bar(position="dodge", stat="identity")


#including socioeconomic goals, indicators, outreach by threat (climate change)
goalsIndicatorOutreach_climate <- Data %>%
  group_by(`Local threats_Climate Change or Severe Weather`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_climate   <- goalsIndicatorOutreach_climate  [-c(1,2), ]

climate <- ggplot(goalsIndicatorOutreach_climate , aes(fill=Goals_indicator_outreach, y=freq, x=`Local threats_Climate Change or Severe Weather`)) + 
  geom_bar(position="dodge", stat="identity")


#including socioeconomic goals, indicators, outreach by threat (mining)
goalsIndicatorOutreach_mining <- Data %>%
  group_by(`Local threats_Energy production or mining`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_mining   <- goalsIndicatorOutreach_mining  [-c(1,2), ]

mining <- ggplot(goalsIndicatorOutreach_mining , aes(fill=Goals_indicator_outreach, y=freq, x=`Local threats_Energy production or mining`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, outreach by threat (disturbance)
goalsIndicatorOutreach_disturbance <- Data %>%
  group_by(`Local threats_Human Intrusion and Disturbance`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_disturbance   <- goalsIndicatorOutreach_disturbance  [-c(1,2), ]

disturbance <- ggplot(goalsIndicatorOutreach_disturbance , aes(fill=Goals_indicator_outreach, y=freq, x=`Local threats_Human Intrusion and Disturbance`)) + 
  geom_bar(position="dodge", stat="identity")


#including socioeconomic goals, indicators, outreach by threat (invasives)
goalsIndicatorOutreach_invasive <- Data %>%
  group_by(`Local threats_Invasives or Disease`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_invasive   <- goalsIndicatorOutreach_invasive  [-c(1,2), ]

invasive <- ggplot(goalsIndicatorOutreach_invasive , aes(fill=Goals_indicator_outreach, y=freq, x=`Local threats_Invasives or Disease`)) + 
  geom_bar(position="dodge", stat="identity")


#including socioeconomic goals, indicators, outreach by threat (natural system modification)
goalsIndicatorOutreach_modification <- Data %>%
  group_by(`Local threats_Natural System Modifications`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_modification   <- goalsIndicatorOutreach_modification  [-c(1,2), ]

modification <- ggplot(goalsIndicatorOutreach_modification , aes(fill=Goals_indicator_outreach, y=freq, x=`Local threats_Natural System Modifications`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, outreach by threat (pollution)
goalsIndicatorOutreach_pollution <- Data %>%
  group_by(`Local threats_Pollution`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_pollution   <- goalsIndicatorOutreach_pollution  [-c(1,2), ]

pollution <- ggplot(goalsIndicatorOutreach_pollution , aes(fill=Goals_indicator_outreach, y=freq, x=`Local threats_Pollution`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, outreach by threat (development)
goalsIndicatorOutreach_development <- Data %>%
  group_by(`Local threats_Residential/commercial development`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_development   <- goalsIndicatorOutreach_development  [-c(1,2), ]

development <- ggplot(goalsIndicatorOutreach_development , aes(fill=Goals_indicator_outreach, y=freq, x=`Local threats_Residential/commercial development`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators, outreach by threat (transportation)
goalsIndicatorOutreach_transportation <- Data %>%
  group_by(`Local threats_Transportation and service corridors`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_transportation   <- goalsIndicatorOutreach_transportation  [-c(1,2), ]

transportation <- ggplot(goalsIndicatorOutreach_transportation , aes(fill=Goals_indicator_outreach, y=freq, x=`Local threats_Transportation and service corridors`)) + 
  geom_bar(position="dodge", stat="identity")


#combine all plots site land type plots
library(ggpubr)
ggarrange(transportation, climate, ag, development, pollution, modification, invasive, disturbance, mining, use)


#VIZ BY LOCAL THREAT (goals or indicator)

#including socioeconomic goals, indicators by threat (ag)
goalsIndicator_ag <- Data %>%
  group_by(`Local threats_Agriculture or aquaculture`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_ag   <- goalsIndicator_ag  [-c(1,2), ]

ag <- ggplot(goalsIndicator_ag , aes(fill=Goals_indicator, y=freq, x=`Local threats_Agriculture or aquaculture`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators by threat (resource use)
goalsIndicator_use <- Data %>%
  group_by(`Local threats_Biological resource use`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_use   <- goalsIndicator_use  [-c(1,2), ]

use <- ggplot(goalsIndicator_use , aes(fill=Goals_indicator, y=freq, x=`Local threats_Biological resource use`)) + 
  geom_bar(position="dodge", stat="identity")


#including socioeconomic goals, indicators threat (climate change)
goalsIndicator_climate <- Data %>%
  group_by(`Local threats_Climate Change or Severe Weather`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_climate   <- goalsIndicator_climate  [-c(1,2), ]

climate <- ggplot(goalsIndicator_climate , aes(fill=Goals_indicator, y=freq, x=`Local threats_Climate Change or Severe Weather`)) + 
  geom_bar(position="dodge", stat="identity")


#including socioeconomic goals, indicators, outreach by threat (mining)
goalsIndicatorOutreach_mining <- Data %>%
  group_by(`Local threats_Energy production or mining`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_mining   <- goalsIndicatorOutreach_mining  [-c(1,2), ]

mining <- ggplot(goalsIndicatorOutreach_mining , aes(fill=Goals_indicator_outreach, y=freq, x=`Local threats_Energy production or mining`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators by threat (disturbance)
goalsIndicator_disturbance <- Data %>%
  group_by(`Local threats_Human Intrusion and Disturbance`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_disturbance   <- goalsIndicator_disturbance  [-c(1,2), ]

disturbance <- ggplot(goalsIndicator_disturbance , aes(fill=Goals_indicator, y=freq, x=`Local threats_Human Intrusion and Disturbance`)) + 
  geom_bar(position="dodge", stat="identity")


#including socioeconomic goals, indicators by threat (invasives)
goalsIndicator_invasive <- Data %>%
  group_by(`Local threats_Invasives or Disease`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_invasive   <- goalsIndicator_invasive  [-c(1,2), ]

invasive <- ggplot(goalsIndicator_invasive , aes(fill=Goals_indicator, y=freq, x=`Local threats_Invasives or Disease`)) + 
  geom_bar(position="dodge", stat="identity")


#including socioeconomic goals, indicators, outreach by threat (natural system modification)
goalsIndicatorOutreach_modification <- Data %>%
  group_by(`Local threats_Natural System Modifications`, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicatorOutreach_modification   <- goalsIndicatorOutreach_modification  [-c(1,2), ]

modification <- ggplot(goalsIndicatorOutreach_modification , aes(fill=Goals_indicator_outreach, y=freq, x=`Local threats_Natural System Modifications`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators by threat (pollution)
goalsIndicator_pollution <- Data %>%
  group_by(`Local threats_Pollution`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_pollution   <- goalsIndicator_pollution  [-c(1,2), ]

pollution <- ggplot(goalsIndicator_pollution , aes(fill=Goals_indicator, y=freq, x=`Local threats_Pollution`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators by threat (development)
goalsIndicator_development <- Data %>%
  group_by(`Local threats_Residential/commercial development`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_development   <- goalsIndicator_development  [-c(1,2), ]

development <- ggplot(goalsIndicator_development , aes(fill=Goals_indicator, y=freq, x=`Local threats_Residential/commercial development`)) + 
  geom_bar(position="dodge", stat="identity")

#including socioeconomic goals, indicators threat (transportation)
goalsIndicator_transportation <- Data %>%
  group_by(`Local threats_Transportation and service corridors`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_transportation   <- goalsIndicator_transportation  [-c(1,2), ]

transportation <- ggplot(goalsIndicator_transportation , aes(fill=Goals_indicator, y=freq, x=`Local threats_Transportation and service corridors`)) + 
  geom_bar(position="dodge", stat="identity")


#combine all plots site land type plots
library(ggpubr)
ggarrange(transportation, climate, ag, development, pollution, modification, invasive, disturbance, mining, use)









#including socioeconomic goals, indicator by IUCN threat (climate change)
goalsIndicator_climate <- Data %>%
  group_by(`IUCN Threats_Climate Change or Severe Weather`, Goals_indicator) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

goalsIndicator_climate   <- goalsIndicator_climate  [-c(1,2), ]

climate <- ggplot(goalsIndicator_climate , aes(fill=Goals_indicator, y=freq, x=`IUCN Threats_Climate Change or Severe Weather`)) + 
  geom_bar(position="dodge", stat="identity")


#subset by IUCN threat (resource use)
resource_use <- subset(Data, `IUCN Threats_Biological resource use` == "TRUE")

resourceUse_outcome <- resource_use %>%
  group_by(Outcome, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(resourceUse_outcome, aes(fill=Goals_indicator_outreach, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity") 

#subset by IUCN threat (ag)
agriculture <- subset(Data, `IUCN Threats_Agriculture or aquaculture` == "TRUE")

agriculture_outcome <- agriculture %>%
  group_by(Outcome, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(agriculture_outcome, aes(fill=Goals_indicator_outreach, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity") 

#subset by IUCN threat (climate)
climate <- subset(Data, `IUCN Threats_Climate Change or Severe Weather` == "TRUE")

climate_outcome <- agriculture %>%
  group_by(Outcome, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(climate_outcome, aes(fill=Goals_indicator_outreach, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity") 

#subset by IUCN threat (energy)
energy <- subset(Data, `IUCN Threats_Energy production or mining` == "TRUE")

energy_outcome <- agriculture %>%
  group_by(Outcome, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(energy_outcome, aes(fill=Goals_indicator_outreach, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity") 

#subset by IUCN threat (human intrusion)
disturbance <- subset(Data, `IUCN Threats_Human Intrusion and Disturbance` == "TRUE")

disturbance_outcome <- disturbance %>%
  group_by(Outcome, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(disturbance_outcome, aes(fill=Goals_indicator_outreach, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity") 

#subset by IUCN threat (human intrusion)
invasive <- subset(Data, `IUCN Threats_Invasives or Disease` == "TRUE")

invasive_outcome <- invasive %>%
  group_by(Outcome, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(invasive_outcome, aes(fill=Goals_indicator_outreach, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity")

#subset by IUCN threat (modification)
modification <- subset(Data, `IUCN Threats_Natural System Modifications` == "TRUE")

modification_outcome <- modification %>%
  group_by(Outcome, Goals_indicator_outreach) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(invasive_outcome, aes(fill=Goals_indicator_outreach, y=freq, x=Outcome)) + 
  geom_bar(position="dodge", stat="identity")
