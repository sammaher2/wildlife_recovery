library(data.table)
library(tidyverse)
library(Hmisc)

setwd("/Users/gzuckerman/Desktop/middletonLab/wildlife_recovery")

#temporary datasheet without 2021 case studies
datasheet <- fread("../Edited Wildlife recovery case study data - Form Responses 1.csv")

library(splitstackshape)

#list from data cleaning document
colsToSplit <- c("IUCN Threats", "Goals (Ecological)", "Goals (Socioeconomic)", "Goals (Admin/Logistical)",
                  "Success Indicators (Ecological)", "Success Indicators (Socioeconomic)", 
                  "Success Indicators (Admin/Logistical)",  "Translocation Type", 
                  "Captive/wild/rehab", "Post Release", "Groups involved", "Outreach", 
                 "Major difficulties faced (Ecological)",      "Major difficulties faced (Socioeconomic)" , 
                 "Major difficulties faced (Admin/Logistic)" , "Lessons learned (Ecological)" ,             
                 "Lessons learned (Socioeconomic)",  "Lessons learned (Admin/Logistic)",  "Reasons cited for success (Ecological)", 
                  "Reasons Cited for Success (Socioeconomic)", "Reasons cited for success (Admin/Logistic)",
                  "Local threats")

#function to split column into multiple columns containing 0/1 values for each unique value in original column
splitColumn <- function (colName) {
  datasheet %>% select(colName) %>% cSplit_e(1, ",", type = "character", fill = 0, drop = TRUE)
}

#merged all split up columns
datasheetSplit <- map_dfc(colsToSplit, splitColumn)

#next step is to decide what other columns we want to keep (eg outcome, locations, ...)
#also need to decide on summary columns, ie any mention of outreach
