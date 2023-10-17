#load packages -----------------------------------------------------------
library(tidyverse)
library(conflicted)
library(dplyr)
library(stringr)
library(readxl)

# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/Elena/Downloads/copy_data_exp_142855-v8")
#load DUDIT dataset and clean------------
Clinical_survey <- read.csv("data_exp_142855-v8_questionnaire-nn1k.csv") #We will need this to get pt sex
Clinical_survey <- data.frame(Clinical_survey$Random_ID, Clinical_survey$Sex.object.25.Response)
Clinical_survey <- rename(Clinical_survey, "Random_ID" =Clinical_survey.Random_ID )
DUDIT_data <- read.csv("data_exp_142855-v8_questionnaire-3zyz.csv")
DUDIT_data_clean <- DUDIT_data[, c(32, 34, 36, 38, 40, 42, 44, 46,48,50, 52, 53)]
rows_to_remove <- c(1, nrow(DUDIT_data_clean))
DUDIT_data_clean <- DUDIT_data_clean[-rows_to_remove, ]
DUDIT_data_clean <- merge(DUDIT_data_clean, Clinical_survey, by= "Random_ID" )

# Identify non-ID variable columns
non_id_cols <- setdiff(names(DUDIT_data_clean), c("Random_ID", "Clinical_survey.Sex.object.25.Response"))

# Convert non-ID variables to numeric
DUDIT_data_clean[, non_id_cols] <- lapply(DUDIT_data_clean[, non_id_cols], as.numeric)


#change numbers to match DUDIT scoring system
excluded_columns <-  c("Random_ID", "Multiple.Choice.Grid.object.5..Have.you.or.anyone.else.been.hurt..mentally.or.physically..because.you.used.drugs..Quantised"
,"Multiple.Choice.Grid.object.5.Has.a.relative.or.a.friend..a.doctor.or.a.nurse..or.anyone.else..been.worried.about.your.drug.use.or.said.to.you.that.you.should.stop.using.drugs..Quantised", "Clinical_survey.Sex.object.25.Response" )

DUDIT_data_clean<- DUDIT_data_clean %>%
  mutate(across(-excluded_columns , ~ case_when(
    . == 1 ~ 0,
    . == 2 ~ 1,
    . == 3 ~ 2,
    . == 4 ~ 3,
    . == 5 ~ 4,
    TRUE ~ .
  )))

last_2_columns <- c("Multiple.Choice.Grid.object.5..Have.you.or.anyone.else.been.hurt..mentally.or.physically..because.you.used.drugs..Quantised","Multiple.Choice.Grid.object.5.Has.a.relative.or.a.friend..a.doctor.or.a.nurse..or.anyone.else..been.worried.about.your.drug.use.or.said.to.you.that.you.should.stop.using.drugs..Quantised")
DUDIT_data_clean<- DUDIT_data_clean %>%
  mutate(across(last_2_columns , ~ case_when(
    . == 1 ~ 0,
    . == 2 ~ 2,
    . == 3 ~ 4,
    TRUE ~ .
  )))

#scoring----
DUDIT_scores <-  numeric(length(DUDIT_data_clean$Random_ID))
DUDIT_scores <- 0
Questions <- c(2:11)

for (i in 1:nrow(DUDIT_data_clean)) {
  DUDIT_score <- sum(DUDIT_data_clean[i, Questions])
  DUDIT_scores[i] <- DUDIT_score
}

DUDIT_data_clean$DUDIT_score <-  as.numeric(DUDIT_scores)

DUDIT_data_clean$DUD<- 0

#Males: A positive score for drug problem  is 6 and above
#Females: A positive score for drug problem is 2 and above
#A score of 25 and above indicates drug dependence for both sexes. 
#Assign "1" if they meet DUD threshold 
for (i in 1:nrow(DUDIT_data_clean)) {
  
  if (DUDIT_data_clean$Clinical_survey.Sex.object.25.Response[i] == "Male" && DUDIT_data_clean$DUDIT_score[i] >= 6) {
    DUDIT_data_clean$DUD[i] <- 1
  }
  if (DUDIT_data_clean$Clinical_survey.Sex.object.25.Response[i] == "Female" && DUDIT_data_clean$DUDIT_score[i] >= 2) {
    DUDIT_data_clean$DUD[i] <- 1
  }
}


#A score above 25 indicates drug dependence 
DUDIT_data_clean$Drug_Dependence<- 0

#Assign "1" if they meet DUD threshold 
for (i in 1:nrow(DUDIT_data_clean)) {
  if (DUDIT_data_clean$DUDIT_score[i] >= 25) {
    DUDIT_data_clean$Drug_Dependence[i] <- 1
  }
}



