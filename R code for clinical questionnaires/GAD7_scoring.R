#load packages -----------------------------------------------------------
library(tidyverse)
library(conflicted)
library(dplyr)
library(stringr)
library(readxl)
# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/Elena/Downloads/")


#load ASRS dataset and clean------------
GAD_7_data <- read.csv('C:/Users/Elena/Downloads/SUP_PRF_p22_vid_bigPE_nohist_newjud_2anxQ_v1_gad7.csv')
GAD_7_data_clean <- GAD_7_data[, c(33, 35, 37, 39, 41, 43, 45, 49)]
# Identify columns to subtract from (excluding 'Random_ID')
columns_to_subtract <- setdiff(names(GAD_7_data_clean), "Random_ID")

# Subtract 1 from selected columns
GAD_7_data_clean <- GAD_7_data_clean %>%
  mutate(across(all_of(columns_to_subtract), ~ . - 1))

GAD_7_data_clean <- GAD_7_data_clean %>%
  mutate(GAD_Total = rowSums(select(., 1:7)))

#Assign the severity of GAD for each pt
#(0-4=No GAD, 5-9= Mild, 10-14= Moderate, 15-21=Severe)
# Loop through the rows
for (i in 1:nrow(GAD_7_data_clean)) {
  score <- GAD_7_data_clean$GAD_Total[i]
  if (score >= 0 && score <= 4) {
    GAD_7_data_clean$GAD_severity[i] <- "None"
  } else if (score >= 5 && score <= 9) {
    GAD_7_data_clean$GAD_severity[i] <- "Mild"
  } else if (score >= 10 && score <= 14) {
    GAD_7_data_clean$GAD_severity[i] <- "Moderate"
  } else if (score >= 15 && score <= 21) {
    GAD_7_data_clean$GAD_severity[i] <- "Severe"
  }
}