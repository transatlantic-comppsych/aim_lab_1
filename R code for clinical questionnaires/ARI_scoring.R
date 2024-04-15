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
ARI_data_clean <- ARI_data[, c(32, 34, 36, 38, 40, 42, 44, 45)]

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

ARI_data_clean <- ARI_data_clean %>%
  rename(
    Q1= Multiple.Choice.Grid.object.2.Easily.annoyed.by.others.Quantised,
    Q2= Multiple.Choice.Grid.object.2.Often.lose.temper.Quantised,
    Q3= Multiple.Choice.Grid.object.2.Stay.angry.for.a.long.time.Quantised,
    Q4= Multiple.Choice.Grid.object.2.Angry.most.of.the.time.Quantised,
    Q5= Multiple.Choice.Grid.object.2.Get.angry.frequently.Quantised,
    Q6= Multiple.Choice.Grid.object.2.Lose.temper.easily.Quantised,
    Q7= Multiple.Choice.Grid.object.2.Overall..irritability.causes.me.problems.Quantised)

#scoring----
#Questions 1:6 summed only for total raw score
#total_average_score = raw score/6 and rounded to nearest integer; none (0), mild-moderate (1), or moderate-severe (2)

ARI_data_clean <- ARI_data_clean %>%
  rowwise() %>%
  mutate(ARI_raw_score = sum(c_across(Q1:Q6)),
         ARI_total_average_score = round(sum(c_across(Q1:Q6))/6)) %>%
  ungroup()

