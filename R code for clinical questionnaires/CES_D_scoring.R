#load packages -----------------------------------------------------------
library(tidyverse)
library(conflicted)
library(dplyr)
library(stringr)
library(readxl)

# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/Elena/Downloads/copy_data_exp_142752-v5")
#load CES-D dataset and clean------------
CESD_data <- read.csv("data_exp_142752-v5_questionnaire-rlck.csv")
CESD_data_clean <- CESD_data[, c(32, 34, 36, 38, 40, 42, 44, 46,48,50,
                                 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 71 )]
# Identify non-ID variable columns
non_id_cols <- setdiff(names(CESD_data_clean), "Random_ID")

# Convert non-ID variables to numeric
CESD_data_clean[, non_id_cols] <- lapply(CESD_data_clean[, non_id_cols], as.numeric)

#change numbers to match AUDIT scoring system
excluded_columns <-  c("Random_ID", "Multiple.Choice.Grid.object.3.4...I.felt.I.was.just.as.good.as.other.people..Quantised",
                       "Multiple.Choice.Grid.object.3.8...I.felt.hopeful.about.the.future..Quantised",
                       "Multiple.Choice.Grid.object.3.12...I.was.happy..Quantised",
                       "Multiple.Choice.Grid.object.3.16...I.enjoyed.life..Quantised")
                  

CESD_data_clean<- CESD_data_clean %>%
  mutate(across(-excluded_columns , ~ case_when(
    . == 1 ~ 0,
    . == 2 ~ 1,
    . == 3 ~ 2,
    . == 4 ~ 3,
    TRUE ~ .
  )))


other_columns <- c("Multiple.Choice.Grid.object.3.4...I.felt.I.was.just.as.good.as.other.people..Quantised",
                   "Multiple.Choice.Grid.object.3.8...I.felt.hopeful.about.the.future..Quantised",
                   "Multiple.Choice.Grid.object.3.12...I.was.happy..Quantised",
                   "Multiple.Choice.Grid.object.3.16...I.enjoyed.life..Quantised")
CESD_data_clean<- CESD_data_clean %>%
  mutate(across(other_columns , ~ case_when(
    . == 1 ~ 3,
    . == 2 ~ 2,
    . == 3 ~ 1,
    . == 4 ~ 0,
    TRUE ~ .
  )))

#scoring----
CESD_scores <-  numeric(length(CESD_data_clean$Random_ID))
CESD_scores <- 0
Questions <- c(1:20)

for (i in 1:nrow(CESD_data_clean)) {
  CESD_score <- sum(CESD_data_clean[i, Questions])
  CESD_scores[i] <- CESD_score
}

CESD_data_clean$CESD_score <-  CESD_scores

CESD_data_clean$Depression_Threshold<- 0

#Assign "1" if they meet Depression threshold 
for (i in 1:nrow(CESD_data_clean)) {
  if (CESD_data_clean$CESD_score[i] >= 16) {
    CESD_data_clean$Depression_Threshold[i] <- 1
  }
}
