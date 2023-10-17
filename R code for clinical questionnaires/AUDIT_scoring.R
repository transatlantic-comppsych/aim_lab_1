#load packages -----------------------------------------------------------
library(tidyverse)
library(conflicted)
library(dplyr)
library(stringr)
library(readxl)

# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/Elena/Downloads/copy_data_exp_144636-v5")
#load AUDIT dataset and clean------------
AUDIT_data <- read.csv("data_exp_144636-v5_questionnaire-chkm.csv")
AUDIT_data_clean <- AUDIT_data[, c(32, 34, 36, 38, 40, 42, 44, 46,48,50, 51)]
#rows_to_remove <- c(1, nrow(AUDIT_data_clean))
#AUDIT_data_clean <- AUDIT_data_clean[-rows_to_remove, ]

# Identify non-ID variable columns
non_id_cols <- setdiff(names(AUDIT_data_clean), "Random_ID")

# Convert non-ID variables to numeric
AUDIT_data_clean[, non_id_cols] <- lapply(AUDIT_data_clean[, non_id_cols], as.numeric)

#change numbers to match AUDIT scoring system
excluded_columns <-  c("Random_ID", "Rating.Scale.object.18.Quantised","Rating.Scale.object.19.Quantised" )

AUDIT_data_clean<- AUDIT_data_clean %>%
  mutate(across(-excluded_columns , ~ case_when(
    . == 1 ~ 0,
    . == 2 ~ 1,
    . == 3 ~ 2,
    . == 4 ~ 3,
    . == 5 ~ 4,
    TRUE ~ .
  )))

last_2_columns <- c("Rating.Scale.object.18.Quantised","Rating.Scale.object.19.Quantised")
AUDIT_data_clean<- AUDIT_data_clean %>%
  mutate(across(last_2_columns , ~ case_when(
    . == 1 ~ 0,
    . == 2 ~ 2,
    . == 3 ~ 4,
    TRUE ~ .
  )))

#scoring----
AUDIT_scores <-  numeric(length(AUDIT_data_clean$Random_ID))
AUDIT_scores <- 0
Questions <- c(1:10)

for (i in 1:nrow(AUDIT_data_clean)) {
  AUDIT_score <- sum(AUDIT_data_clean[i, Questions])
  AUDIT_scores[i] <- AUDIT_score
}

AUDIT_data_clean$AUDIT_score <-  AUDIT_scores

AUDIT_data_clean$AUD<- 0

#Assign "1" if they meet AUD threshold 
for (i in 1:nrow(AUDIT_data_clean)) {
  if (AUDIT_data_clean$AUDIT_score[i] >= 8) {
    AUDIT_data_clean$AUD[i] <- 1
  }
}

#A score above 15 indicates alcohol dependence 
AUDIT_data_clean$Alcohol_Dependence<- 0

#Assign "1" if they meet AUD threshold 
for (i in 1:nrow(AUDIT_data_clean)) {
  if (AUDIT_data_clean$AUDIT_score[i] >= 15) {
    AUDIT_data_clean$Alcohol_Dependence[i] <- 1
  }
}



