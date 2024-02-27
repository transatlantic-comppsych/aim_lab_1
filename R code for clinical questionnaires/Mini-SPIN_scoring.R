#load packages -----------------------------------------------------------
library(tidyverse)
library(conflicted)
library(dplyr)
library(stringr)
library(readxl)
# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/Elena/Downloads/copy_data_exp_167771-v4")
Mini_SPIN_data <- read.csv("SUP_PRF_p17_vid_bigPE_nar2_earlypred_chk_cbal_v4_mini_spin.csv")
Mini_SPIN_data_clean <- Mini_SPIN_data[, c(32, 34, 36, 39)]
rows_to_remove <- c(1, nrow(Mini_SPIN_data_clean))
Mini_SPIN_data_clean <- Mini_SPIN_data_clean[-rows_to_remove, ]

# Identify non-ID variable columns
non_id_cols <- setdiff(names(Mini_SPIN_data_clean), "Random_ID")

# Convert non-ID variables to numeric
Mini_SPIN_data_clean[, non_id_cols] <- lapply(Mini_SPIN_data_clean[, non_id_cols], as.numeric)
Mini_SPIN_scores <-  numeric(length(Mini_SPIN_data_clean$Random_ID))
Mini_SPIN_scores <- 0
Questions <- c(1:3)

for (i in 1:nrow(Mini_SPIN_data_clean)) {
  Mini_SPIN_score <- sum(Mini_SPIN_data_clean[i, Questions])
  Mini_SPIN_scores[i] <- Mini_SPIN_score
}


Mini_SPIN_data_clean$Mini_SPIN_score <-  Mini_SPIN_scores

Mini_SPIN_data_clean$SAD<- 0

#Assign "1" if they meet SAD threshold 
for (i in 1:nrow(Mini_SPIN_data_clean)) {
  if (Mini_SPIN_data_clean$Mini_SPIN_score[i] >= 6) {
    Mini_SPIN_data_clean$SAD[i] <- 1
  }
}

