#load packages -----------------------------------------------------------
library(tidyverse)
library(conflicted)
library(dplyr)
library(stringr)
library(readxl)
# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/Elena/Downloads/copy_data_exp_142752-v5")
#load MDQ dataset and clean------------
MDQ_data <- read.csv("data_exp_142752-v5_questionnaire-jgx5.csv")

MDQ_data_clean <- MDQ_data[, c(32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 58, 60, 62, 64, 65)]
#rows_to_remove <- c(1, nrow(MDQ_data_clean), (nrow(MDQ_data_clean)-1))
#MDQ_data_clean <- MDQ_data_clean[-rows_to_remove, ]

# Identify non-ID variable columns
non_id_cols <- setdiff(names(MDQ_data_clean), "Random_ID")

# Convert non-ID variables to numeric
MDQ_data_clean[, non_id_cols] <- lapply(MDQ_data_clean[, non_id_cols], as.numeric)

#change numbers to match AUDIT scoring system
excluded_columns <-  c("Random_ID", "Multiple.Choice.Grid.object.6.Has.a.health.professional.ever.told.you.that.you.have.manic.depressive.illness.or.bipolar.disorder..Quantised",
                       "Multiple.Choice.Grid.object.6.Have.any.of.your.blood.relatives..i.e..children..siblings..parents..grandparents..aunts..uncles..had.manic.depressive.illness.or.bipolar.disorder..Quantised",
                       "Multiple.Choice.Grid.object.5.How.much.of.a.problem.did.any.of.these.cause.you...like.being.unable.to.work..having.family..money.or.legal.troubles..getting.into.arguments.or.fights..Quantised"
                       )

MDQ_data_clean<- MDQ_data_clean %>%
  mutate(across(-excluded_columns , ~ case_when(
    . == 1 ~ 1,
    . == 2 ~ 0,
    TRUE ~ .
  )))

# Scoring -----------------------------------------------------------------
MDQ_scores <-  numeric(length(MDQ_data_clean$Random_ID))

MDQ_scores <- 0
Q1_13 <- c(1:13)

for (i in 1:nrow(MDQ_data_clean)) {
  MDQ_score <- sum(MDQ_data_clean[i, Q1_13])
  MDQ_scores[i] <- MDQ_score
}

MDQ_data_clean$MDQ_score_Q13 <- MDQ_scores

#To meet a diagnosis, pt need to score above 7 in Q1-13 AND 
#Check “yes” Q14 AND
#Symptoms caused either “moderate” or “serious” problems (question 15).

# Create a new column 'meets_threshold' with default value 'no'
MDQ_data_clean$meets_threshold <- 0

# Loop through the rows
for (i in 1:nrow(MDQ_data_clean)) {
  if (MDQ_data_clean$MDQ_score_Q13[i] >= 7 &&
      MDQ_data_clean$Multiple.Choice.Grid.object.3.Have.several.of.these.ever.happened.during.the.same.period.of.time..Quantised[i] == 1 & 
      as.numeric(MDQ_data_clean$Multiple.Choice.Grid.object.5.How.much.of.a.problem.did.any.of.these.cause.you...like.being.unable.to.work..having.family..money.or.legal.troubles..getting.into.arguments.or.fights..Quantised[i]) > 2) {
    MDQ_data_clean$meets_threshold[i] <- 1
  }
}




