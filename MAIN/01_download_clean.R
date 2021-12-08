# Wildlife Recovery Project
## Middleton Lab
### 2021

######################################################################################
############## 01 Downloading and Cleaning Data from Google Sheet ####################
######################################################################################
library(data.table)
library(tidyverse)
library(Hmisc)
library(googledrive)
library(googlesheets4)
library(splitstackshape)


setwd("")

#### Download Google Sheet Data from Middleton Lab Google Drive and CLEAN THAT DIRTY DATA####
# Notes: To make this easy, I've made the sheet public. Otherwise you need to set up a token etc and it's annoying w/ the Berkeley security system.

#This let's us download the public spreadsheet without having to authorize your account:
drive_deauth()  
#Download edited data directly from google sheet "Edited Wildlife recovery case study data" in Datasheets folder:
drive_download("https://docs.google.com/spreadsheets/d/15yYexIiG3WSeTfnAwUJBgrwrDk9zPtsRqdlAlVcm6fI/edit?usp=sharing",
               overwrite = TRUE,
               path = "data/goog_raw.csv")
#Save locally:
datasheet <- fread('data/goog_raw.csv')

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
datasheetSplit <- map_dfc(colsToSplit, splitColumn) %>% cbind()









