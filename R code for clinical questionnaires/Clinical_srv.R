#load packages -----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(readxl)
# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/Elena/Downloads")
clinical_srv_data <- read.csv("SUP_PRF_pilot_vid_big_PE_avatars_v9_clinical_srv.csv")
names(clinical_srv_data)
# Assuming your dataframe is named df
selected_columns <- c("Dropdown.object.19.Response",
                      "Sex.object.25.Response",
                      "Gender.object.24.Response",
                      "Gender.object.24.Other",                                
                      "mental_health_diagnosis.object.3.Response",  
                      "other_mental_health_diagnoses.object.7.Depression",     
                      "other_mental_health_diagnoses.object.7.Anxiety.disorder",
                      "other_mental_health_diagnoses.object.7.ADHD",            
                      "other_mental_health_diagnoses.object.7.Other",      
                      "medication_status.object.5.Response",           
                      "fluoxetine_prozac.object.10.Response",
                      "dose_fluoxetine_prozac.object.11.Value",                
                      "sertraline_zoloft.object.12.Response",                   
                      "dose_sertraline_zoloft.object.13.Value",                
                      "Ritalin.object.14.Response",
                      "dose_Ritalin.object.15.Value",                           
                      "other_medication.object.16.Value",                       
                      "Ethnicity..object.28.Response",                      
                      "Ethnicity..object.28.Other",                             
                      "Random_ID")                            

  
clinical_srv_data_clean <- clinical_srv_data[, selected_columns]  # Subset your dataframe with selected columns

# Generate new names for the columns
new_names <- c("Age", "Sex", "Gender_same_as_birth", "Gender_other", "MH_diagnosis", "Depression", "Anxiety", "ADHD", "MH_other", "Medication_status", "fluoxetine_prozac", "flueoxtine_prozac_dosage", "sertraline_zoloft", "sertraline_zoloft_dosage", "ritaln", "ritalin_dosage", "other_medications", "Ethnicity", "Ethnicity_other", "Random_ID")

# Rename the columns
colnames(clinical_srv_data_clean)[1:20] <- new_names
