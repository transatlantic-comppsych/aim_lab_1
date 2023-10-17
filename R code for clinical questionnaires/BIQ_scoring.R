#load packages -----------------------------------------------------------
library(tidyverse)
library(conflicted)
library(dplyr)
library(stringr)
library(readxl)

# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/Elena/Downloads/copy_data_exp_140743-v17")
#load BIQ dataset and clean------------
BIQ_data <- read.csv("data_exp_140743-v17_questionnaire-uu2n.csv")
BIQ_data <- data.frame(BIQ_data$Random_ID, BIQ_data$Response, BIQ_data$Object.ID)
BIQ_data <- subset(BIQ_data, BIQ_data.Response != "" & BIQ_data.Response != "END" &
                     BIQ_data.Response != "BEGIN" & BIQ_data.Random_ID != "")

BIQ_data <- reshape(BIQ_data, idvar = "BIQ_data.Random_ID", timevar = "BIQ_data.Object.ID", direction = "wide")

#change numbers to match AUDIT scoring system

#BIQ_data <- BIQ_data %>%
 # rename(Object_5 = `BIQ_data.Response.object-5`, Object_18 = `BIQ_data.Response.object-18`, Object_259 = `BIQ_data.Response.object-259` )

BIQ_data$`BIQ_data.Response.object-5` <- as.numeric(BIQ_data$`BIQ_data.Response.object-5`)
BIQ_data$`BIQ_data.Response.object-18` <- as.numeric(BIQ_data$`BIQ_data.Response.object-18`)
BIQ_data$`BIQ_data.Response.object-80` <- as.numeric(BIQ_data$`BIQ_data.Response.object-80`)
BIQ_data$`BIQ_data.Response.object-259` <- as.numeric(BIQ_data$`BIQ_data.Response.object-259`)
BIQ_data$`BIQ_data.Response.object-36` <- as.numeric(BIQ_data$`BIQ_data.Response.object-36`)
BIQ_data$`BIQ_data.Response.object-86` <- as.numeric(BIQ_data$`BIQ_data.Response.object-86`)
BIQ_data$`BIQ_data.Response.object-53` <- as.numeric(BIQ_data$`BIQ_data.Response.object-53`)
BIQ_data$`BIQ_data.Response.object-60` <- as.numeric(BIQ_data$`BIQ_data.Response.object-60`)
BIQ_data$`BIQ_data.Response.object-67` <- as.numeric(BIQ_data$`BIQ_data.Response.object-67`)


reversed_columns <-  c("BIQ_data.Response.object-5", "BIQ_data.Response.object-18","BIQ_data.Response.object-259" )

BIQ_data<- BIQ_data %>%
  mutate(across(reversed_columns , ~ case_when(
    . == 0 ~ 8,
    . == 1 ~ 7,
    . == 2 ~ 6,
    . == 3 ~ 5,
    . == 4 ~ 4,
    . == 5 ~ 3,
    . == 6 ~ 2,
    . == 7 ~ 1,
    . == 8 ~ 0,
    TRUE ~ .
  )))

#scoring----
BIQ_scores <-  numeric(length(BIQ_data$BIQ_data.Random_ID))
BIQ_scores <- 0
Questions <- c(2:10)

for (i in 1:nrow(BIQ_data)) {
  BIQ_score <- sum(BIQ_data[i, Questions])
  BIQ_scores[i] <- BIQ_score
}

BIQ_data$BIQ_score <-  BIQ_scores

BIQ_data$BDD<- 0

#Assign "1" if they meet BDD threshold 
for (i in 1:nrow(BIQ_data)) {
  if (BIQ_data$BIQ_score[i] >= 40) {
    BIQ_data$BDD[i] <- 1
  }
}
