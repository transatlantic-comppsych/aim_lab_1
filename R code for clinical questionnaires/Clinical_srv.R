#load packages -----------------------------------------------------------
library(tidyverse)
library(conflicted)
library(dplyr)
library(stringr)
library(readxl)
# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/Elena/Downloads")
clinical_srv_data <- read.csv("SUP_PRF_pilot_vid_big_PE_avatars_v9_clinical_srv.csv")
# Assuming your dataframe is named df
selected_columns <- c(31, 33, 35, 37, 38, 40, 41, 42, 43, 44, 45, 47, 49, 50, 53, 55, 56, 57, 59, 60)  # Selecting columns 31 through 59 skipping every other column
clinical_srv_data_clean <- clinical_srv_data[, selected_columns]  # Subset your dataframe with selected columns

# Generate new names for the columns
new_names <- c("Age", "Sex", "Gender_same_as_birth", "Gender_other", "MH_diagnosis", "Depression", "Anxiety", "ADHD", "MH_other", "Medication_status", "fluoxetine_prozac", "flueoxtine_dosage", "sertraline_zoloft", "sertraline_dosage", "ritaln", "ritalin_dosage", "other_medications","Ethnicity", "Ethnicity_other", "Random_ID")

# Rename the columns
colnames(clinical_srv_data_clean)[1:20] <- new_names
