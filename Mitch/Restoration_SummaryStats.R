####Summary Stats for Wildlife Restoration Project 

library(tidyverse)


#Number of case studies 
nrow(Data)

#Number of case studies that include HD in Goals
summary(Data$socioeconomicGoal)

#Number of case studies that include HD in Indicators
summary(Data$socioeconomicIndicator)

#Number of case studies that include HD in goals or indicators
summary(Data$Goals_indicator)

#Number of case studies that include HD in lesson learned or difficulty
summary(Data$socioeconomic_lesson_difficulty)

#Number of case studies that did not include HD in goals or indicators
#but included it in either their lessons learned or major difficulty
summary(Data$socioeconomic_noHD_Yesproblems)

#number of case studies with a positive outcome
summary(Data$outcome_binary)

#Number case studies per taxa
Data %>%
  group_by(Taxonomy) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n))

#Number of case studies per continent
Data %>%
  group_by(Continent) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n))

#Number of case studies that included HD in 2021 book
book_2021 <- subset(Data, `Publication Year` == "2021")
summary(book_2021$Goals_indicator)

#Number of case studies resulting in positive outcome
summary(Data$outcome_binary)

#Number of case studies per group 
summary(Data$`Groups involved_Local community/indigenous groups`)

summary(Data$`Groups involved_Private landowners`)


#Looking in-depth at specific human dimensions
summary(Data$`Goals (Socioeconomic)_Cultural Benefit`)
summary(Data$`Goals (Socioeconomic)_Economic Benefit`)
summary(Data$`Goals (Socioeconomic)_Education`)
summary(Data$`Goals (Socioeconomic)_Engage Locals in Conservation`)
summary(Data$`Goals (Socioeconomic)_Increase awareness`)
summary(Data$`Goals (Socioeconomic)_Law Enforcement`)
summary(Data$`Goals (Socioeconomic)_other`)

summary(Data$`Success Indicators (Socioeconomic)_Cultural Benefit`)
summary(Data$`Success Indicators (Socioeconomic)_Economic Benefit`)
summary(Data$`Reasons Cited for Success (Socioeconomic)_Increase Acceptance/Social Tolerance`)
summary(Data$`Success Indicators (Socioeconomic)_Legal Protection`)
summary(Data$`Reasons Cited for Success (Socioeconomic)_Increase Public Awareness`)
summary(Data$`Success Indicators (Socioeconomic)_Public Involvement`)

unqiue 