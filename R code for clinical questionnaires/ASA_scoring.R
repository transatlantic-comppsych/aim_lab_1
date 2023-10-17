#load packages -----------------------------------------------------------
library(tidyverse)
library(conflicted)
library(dplyr)
library(stringr)
library(readxl)

#load de-identified data from screener (qualtrics) and clean------------
ASA_data <- read.csv("C:/Users/Elena/Downloads/mturk_screener_with_randomID.csv")
ASA_data_clean <- ASA_data[, c(36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 52)]

#scoring----
ASA_scores <-  numeric(length(ASA_data_clean$Random_ID))
ASA_scores <- 0
Questions <- c(1:14)

for (i in 1:nrow(ASA_data_clean)) {
  ASA_score <- sum(ASA_data_clean[i, Questions])
  ASA_scores[i] <- ASA_score
}

ASA_data_clean$ASA_score <-  ASA_scores
