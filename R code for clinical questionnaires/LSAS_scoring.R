#load packages -----------------------------------------------------------
library(tidyverse)
library(conflicted)
library(dplyr)
library(stringr)
library(readxl)
# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/Elena/Downloads/copy_data_exp_144636-v5")
LSAS_data <- read.csv("data_exp_144636-v5_questionnaire-xzz2.csv")
LSAS_data_clean <- LSAS_data[, c(31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51,
                                 53, 55, 57, 59, 61, 63, 65, 67, 69, 71, 73, 75, 
                                 77, 79, 81, 83, 85, 87, 89, 91, 93, 94, 97, 99,
                                 101, 103, 104, 107, 109, 111, 113, 115, 117, 119,
                                 121, 123, 125, 127)]
#rows_to_remove <- c(1, nrow(LSAS_data_clean))
#LSAS_data_clean <- LSAS_data_clean[-rows_to_remove, ]
# Identify non-ID variable columns
non_id_cols <- setdiff(names(LSAS_data_clean), "Random_ID")

# Convert non-ID variables to numeric
LSAS_data_clean[, non_id_cols] <- lapply(LSAS_data_clean[, non_id_cols], as.numeric)

# Scoring -----------------------------------------------------------------
LSAS_scores <-  numeric(length(LSAS_data_clean$Random_ID))

LSAS_scores <- 0
Questions <- c(1:48)

for (i in 1:nrow(LSAS_data_clean)) {
  LSAS_score <- sum(LSAS_data_clean[i, Questions])
  LSAS_scores[i] <- LSAS_score
}


LSAS_data_clean$LSAS_score <- LSAS_scores


#To meet a SAD diagnosis, pt need to score 30 and above
#To meet a GAD diagnosis, pt need to score 60 and above 


# Create a new columns SAD and GAD and set default as 'no' 
LSAS_data_clean$SAD<- 0
LSAS_data_clean$GAD<- 0
LSAS_data_clean$SAD_severity <- 

#Assign "1" if they meet SAD threshold 
for (i in 1:nrow(LSAS_data_clean)) {
  if (LSAS_data_clean$LSAS_score[i] >= 30) {
    LSAS_data_clean$SAD[i] <- 1
  }
}

#Assign "1" if they meet GAD threshold 
for (i in 1:nrow(LSAS_data_clean)) {
  if (LSAS_data_clean$LSAS_score[i] >= 60) {
    LSAS_data_clean$GAD[i] <- 1
  }
}

#Assign the severity of SAD for each pt
#(0-29=No SAD, 30-49= Mild, 50-64= Moderate, 65-79= Marked,80-94= Severe, >95= Very severe)
# Loop through the rows
for (i in 1:nrow(LSAS_data_clean)) {
  score <- LSAS_data_clean$LSAS_score[i]
  if (score >= 0 && score <= 29) {
    LSAS_data_clean$SAD_severity[i] <- "None"
  } else if (score >= 30 && score <= 49) {
    LSAS_data_clean$SAD_severity[i] <- "Mild"
  } else if (score >= 50 && score <= 64) {
    LSAS_data_clean$SAD_severity[i] <- "Moderate"
  } else if (score >= 65 && score <= 79) {
    LSAS_data_clean$SAD_severity[i] <- "Marked"
  } else if (score >= 80 && score <= 94) {
    LSAS_data_clean$SAD_severity[i] <- "Severe"
  } else if (score >= 95) {
    LSAS_data_clean$SAD_severity[i] <- "Very Severe"
  }
}



