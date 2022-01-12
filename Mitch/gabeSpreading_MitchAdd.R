library(data.table)
library(tidyverse)
library(Hmisc)
library(ggplot2)

setwd("C:/Users/mitch/OneDrive/Desktop")

#temporary datasheet without 2021 case studies
datasheet <- fread("edited_recovery2.csv")

datasheet <- read.csv("edited_recovery2.csv", check.names=FALSE)

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


#socioeconomic_lesson_difficulty: if the case study included either a socio-economic lesson learned or  
#difficulty
datasheetSplit <- datasheetSplit %>%
  mutate(socioeconomic_lesson_difficulty = ifelse(socioeconomicLessons | socioeconomicDifficulty, T, F))


#socioeconomic_noHD_Yesproblems: If the case study did not include a HD in Goals or Indicators but cited it as either
#lesson learned or major difficulty
datasheetSplit <- datasheetSplit %>%
  mutate(socioeconomic_noHD_Yesproblems = ifelse(!Goals_indicator & socioeconomic_lesson_difficulty, T, F))

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



#combine datasets
Data <- cbind(datasheet, datasheetSplit)
