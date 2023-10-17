#load packages -----------------------------------------------------------
library(tidyverse)
library(conflicted)
library(dplyr)
library(stringr)
library(readxl)

# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/Elena/Downloads/copy_data_exp_142752-v5")
#load ARI dataset and clean------------
ARI_data <- read.csv("data_exp_142752-v5_questionnaire-97cc.csv")
ARI_data_clean <- ARI_data[, c(32, 34, 36, 38, 40, 42,44, 45)]

# Identify non-ID variable columns
non_id_cols <- setdiff(names(ARI_data_clean), "Random_ID")

# Convert non-ID variables to numeric
ARI_data_clean[, non_id_cols] <- lapply(ARI_data_clean[, non_id_cols], as.numeric)

#change numbers to match ARI scoring system
excluded_columns <-  c("Random_ID")

ARI_data_clean<- ARI_data_clean %>%
  mutate(across(-excluded_columns , ~ case_when(
    . == 1 ~ 0,
    . == 2 ~ 1,
    . == 3 ~ 2,
    TRUE ~ .
  )))

#scoring----
ARI_scores <-  numeric(length(ARI_data_clean$Random_ID))
ARI_scores <- 0
Questions <- c(1:6)

for (i in 1:nrow(ARI_data_clean)) {
  ARI_score <- sum(ARI_data_clean[i, Questions])
  ARI_scores[i] <- ARI_score
}

ARI_data_clean$ARI_score <-  ARI_scores
