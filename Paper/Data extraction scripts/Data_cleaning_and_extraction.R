library(here)
library(tidyverse)
# Extract Pilot 21 data ---------------------------------------------------------------
# v3 and v4 have exactly the same task version, but BIQ was causing a problem due to a Gorilla update and it was changed in version 4
df21_raw_v3 <- read.csv(here("Data", "Raw_data", "Prolific participants", "Prolific participants (18-25; pilot 21)", "SUP_PRF_p21_vid_bigPE_nohist_newjud_v3", "SUP_PRF_p21_vid_bigPE_nohist_newjud_v3_task_main.csv"))
df21_spin_v3 <- read.csv(here("Data", "Raw_data", "Prolific participants", "Prolific participants (18-25; pilot 21)","SUP_PRF_p21_vid_bigPE_nohist_newjud_v3", "SUP_PRF_p21_vid_bigPE_nohist_newjud_v3_mini_spin.csv"))
df21_cesd_v3 <- read.csv(here("Data", "Raw_data", "Prolific participants", "Prolific participants (18-25; pilot 21)","SUP_PRF_p21_vid_bigPE_nohist_newjud_v3", "SUP_PRF_p21_vid_bigPE_nohist_newjud_v3_ces_d.csv"))

df21_raw_v4 <- read.csv(here("Data", "Raw_data", "Prolific participants", "Prolific participants (18-25; pilot 21)", "SUP_PRF_p21_vid_bigPE_nohist_newjud_v4", "SUP_PRF_p21_vid_bigPE_nohist_newjud_v4_task_main.csv"))
df21_raw_v4 <- df21_raw_v4[, !(names(df21_raw_v4) %in% "X.1")]
df21_spin_v4 <- read.csv(here("Data", "Raw_data", "Prolific participants", "Prolific participants (18-25; pilot 21)","SUP_PRF_p21_vid_bigPE_nohist_newjud_v4", "SUP_PRF_p21_vid_bigPE_nohist_newjud_v4_mini_spin.csv"))
df21_cesd_v4 <- read.csv(here("Data", "Raw_data", "Prolific participants", "Prolific participants (18-25; pilot 21)","SUP_PRF_p21_vid_bigPE_nohist_newjud_v4", "SUP_PRF_p21_vid_bigPE_nohist_newjud_v4_ces_d.csv"))

df21_raw <- rbind(df21_raw_v3, df21_raw_v4)
df21_spin <- rbind(df21_spin_v3, df21_spin_v4)
df21_cesd <- rbind(df21_cesd_v3, df21_cesd_v4)


CESD_data_clean <- df21_cesd[, c(31, 33, 35, 37, 39, 41, 43, 45,47,49,
                                 51, 53, 55, 57, 59, 61, 63, 65, 67, 69, 70 )]
# Identify non-ID variable columns
non_id_cols <- setdiff(names(CESD_data_clean), "Random_ID")

# Convert non-ID variables to numeric
CESD_data_clean[, non_id_cols] <- lapply(CESD_data_clean[, non_id_cols], as.numeric)

#change numbers to match CES-D scoring system
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

#scoring
CESD_scores <-  numeric(length(CESD_data_clean$Random_ID))
CESD_scores <- 0
Questions <- c(1:20)

for (i in 1:nrow(CESD_data_clean)) {
  CESD_score <- sum(CESD_data_clean[i, Questions])
  CESD_scores[i] <- CESD_score
}

CESD_data_clean$CESD_score <-  CESD_scores

CESD_data_clean$Depression_Threshold<- 0
CESD_data_clean$Depression_status <- "Not_Depressed"

#Assign "1" if they meet Depression threshold 
for (i in 1:nrow(CESD_data_clean)) {
  if (CESD_data_clean$CESD_score[i] >= 16) {
    CESD_data_clean$Depression_Threshold[i] <- 1
    CESD_data_clean$Depression_status[i] <- "Depressed"
  }
}

# We have now different names for different screens:
# Prediction 
# Feedback 
# First 4 trials are with the histograms and are called "Trials_Histogram" in Display column instead of "Trials"
# The trial numbers start from 1 again after the histograms so we need to concatenate them all.
# Let's rename them to "Trials" to extract them together with the rest of the trials

df21_raw$Trial.Number[df21_raw$Display == "Trials "] <- df21_raw$Trial.Number[df21_raw$Display == "Trials "] + 4

df21_raw <- df21_raw %>%
  mutate(Screen = if_else(Screen == "Prediction", "Prediction ", Screen))


df21_raw <- df21_raw %>%
  mutate(Display = if_else(Display == "Trials_Histogram", "Trials ", Display))

df21_spin <- df21_spin %>%
  rename(
   "Q1" =  "Rating.Scale.object.2.Fear.of.embarrassment.causes.me.to.avoid.doing.things.or.speaking.to.people.",
   "Q2" =  "Rating.Scale.object.2.I.avoid.activities.in.which.I.am.the.center.of.attention.",
   "Q3" = "Rating.Scale.object.2.Being.embarrassed.or.looking.stupid.are.among.my.worst.fears."
  )

df21_spin <- df21_spin %>%
  rowwise() %>%
  mutate(mini_SPIN_total = sum(c(Q1, Q2, Q3), na.rm = TRUE)) %>%
  ungroup()


# create a new df21 that only keeps the columns we need
df21 <- df21_raw[, c("Trial.Number", "Display", "Screen", "Response.Type", "Response", 
                     "Reaction.Time", "Spreadsheet..Histogram", "Spreadsheet..Feedback", "Random_ID")]

df21 <- df21 %>% dplyr::filter(Response.Type != "info")

df21 <- df21 %>%
  mutate(Response = as.numeric(as.character(Response))) %>%
  dplyr::filter(!is.na(Response))

df21$Screen <- ifelse(df21$Trial.Number == 1 & (df21$Display == "Mood " | df21$Display == "Anxiety "), "BL_1",
                      ifelse(df21$Trial.Number == 2 & (df21$Display == "Mood " | df21$Display == "Anxiety "), "BL_2", 
                             df21$Screen))

df21$m_hist <- ifelse(df21$Spreadsheet..Histogram == "h2.png", 29,
                             ifelse(df21$Spreadsheet..Histogram == "h3.png", 37,
                                                  ifelse(df21$Spreadsheet..Histogram == "h6.png", 63,
                                                         ifelse(df21$Spreadsheet..Histogram == "h7.png", 71, NA))))  


# get rid of the rows we don't need: practice runs, continue button presses etc. and only keep Trials, Anxiety and Mood that include all variables we are interested in:
df21 <- df21 %>%
  dplyr::filter(
    (Display == "Trials ") |
      (Display == "Exit") |
      (Display == "Anxiety " & !Trial.Number %in% c(1, 2)) |
      (Display == "Mood " & !Trial.Number %in% c(1, 2)) 
  )

# # setting Mood and Anxiety trial numbers to match the trials starting from 1 (since we removed the baseline ones above)
df21$Trial.Number <- ifelse(df21$Display == 'Mood ' | df21$Display == 'Anxiety ', df21$Trial.Number - 2, df21$Trial.Number)
df21$Display <- ifelse(df21$Display == 'Mood ' | df21$Display == 'Anxiety ', "Trials", df21$Display)

# renaming the feedback column to fdbk
names(df21)[names(df21) == 'Spreadsheet..Feedback'] <- 'fdbk'

# Now let's create columns for PE (fdbk-hist) and Subj_PE (fdbk_Prediction)

df21$SubjPE <- ifelse(df21$Screen == "Prediction ", df21$fdbk - df21$Response, NA)
df21$PE <- ifelse(df21$Screen == "Prediction ", df21$fdbk - df21$m_hist, NA)
# to keep just one repetition of feedback and mean histogram per trial per subject 
# and replace the rest with NA so that when we want to convert to long format we won't have duplicates
df21$fdbk <- ifelse(df21$Screen == "Prediction ", df21$fdbk, NA)
df21$m_hist <- ifelse(df21$Screen == "Prediction ", df21$m_hist, NA)
df21$stress_level <- ifelse(df21$Screen == "Screen 12", df21$Response, NA)

long_df21 <- df21 %>%
  dplyr::select(Random_ID, Trial.Number, fdbk, m_hist, PE, SubjPE, stress_level) %>%
  pivot_longer(cols = c(fdbk, m_hist, PE, SubjPE, stress_level),
               names_to = "Screen",
               values_to = "Response")

# This will keep only the rows where 'Response' is not NA
long_df21 <- long_df21[!is.na(long_df21$Response), ]

# Selecting relevant columns from the original df21
df21 <- df21 %>%
  dplyr::select(-c(fdbk, m_hist, PE, SubjPE, stress_level))

# Binding the rows together
df21 <- bind_rows(df21, long_df21)

df21$Response <- as.numeric(df21$Response)

# looking at the relationship between PE and SubjPE
df21_PE <- subset(df21, Screen == "PE")
names(df21_PE)[names(df21_PE) == "Screen"] <- "Screen_PE"
names(df21_PE)[names(df21_PE) == "Response"] <- "Response_PE"

df21_SubjPE <- subset(df21, Screen == "SubjPE")
names(df21_SubjPE)[names(df21_SubjPE) == "Screen"] <- "Screen_SubjPE"
names(df21_SubjPE)[names(df21_SubjPE) == "Response"] <- "Response_SubjPE"

df21_fdbk <- subset(df21, Screen == "fdbk")
names(df21_fdbk)[names(df21_fdbk) == "Screen"] <- "Screen_fdbk"
names(df21_fdbk)[names(df21_fdbk) == "Response"] <- "Response_fdbk"

df21_hist <- subset(df21, Screen == "m_hist")
names(df21_hist)[names(df21_hist) == "Screen"] <- "Screen_m_hist"
names(df21_hist)[names(df21_hist) == "Response"] <- "Response_m_hist"

df21_stress_level <- subset(df21, Screen == "Screen 12")
names(df21_stress_level)[names(df21_stress_level) == "Screen"] <- "Screen_stress_level"
names(df21_stress_level)[names(df21_stress_level) == "Response"] <- "Response_stress_level"

df21_predic <- subset(df21, Screen == "Prediction ")
names(df21_predic)[names(df21_predic) == "Screen"] <- "Screen_pred"
names(df21_predic)[names(df21_predic) == "Response"] <- "Response_pred"


list_of_dfs_21 <- list(df21_PE, df21_fdbk, df21_hist, df21_predic, df21_SubjPE)

# Using reduce with inner_join
merged_df21 <- reduce(list_of_dfs_21, inner_join, by = c("Random_ID", "Trial.Number"))

df21_Anxious <- subset(df21, Screen == "Anxious")
names(df21_Anxious)[names(df21_Anxious) == "Screen"] <- "Screen_Ax"
names(df21_Anxious)[names(df21_Anxious) == "Response"] <- "Response_Ax"

df21_Happy <- subset(df21, Screen == "Happy")
# df21_fdbk <- subset(df21, Screen == "Prediction")
names(df21_Happy)[names(df21_Happy) == "Screen"] <- "Screen_H"
names(df21_Happy)[names(df21_Happy) == "Response"] <- "Response_H"

final_df21 <- merged_df21 %>%
  inner_join(df21_Anxious, by = c("Random_ID", "Trial.Number")) %>%
  inner_join(df21_Happy, by = c("Random_ID", "Trial.Number"))


# subset only the columns we are interested in
final_df21 <- final_df21[c("Trial.Number", "Random_ID", "Response_H", "Response_Ax", "Response_fdbk", "Response_SubjPE", "Response_PE", "Response_m_hist", "Response_pred")]

final_df21 <- final_df21 %>%
  left_join(df21_spin %>% dplyr::select(Random_ID, mini_SPIN_total), by = "Random_ID")

final_df21 <- final_df21 %>%
  left_join(df21_stress_level %>% dplyr::select(Random_ID, Response_stress_level), by = "Random_ID")

final_df21 <- final_df21 %>%
  left_join(CESD_data_clean %>% dplyr::select(Random_ID, CESD_score, Depression_Threshold, Depression_status), by = "Random_ID")

final_df21$Social_Anxiety <- ifelse(final_df21$mini_SPIN_total >= 6, "high", "low")


final_df21 %>%
  group_by(Random_ID) %>%
  summarise(Trial.Number = n())

write.csv(final_df21, "pilot_21_variables.csv", row.names = FALSE)

# Extract Prolific 18-25 data ----------------------------------------------------------
dfpro_18_25 <- read.csv(here("Data", "Raw_data", "Prolific participants", "Prolific participants (18-25)", "SUP_PRF_18-25_task_main.csv"))
dfpro_18_25_spin <- read.csv(here("Data", "Raw_data", "Prolific participants", "Prolific participants (18-25)","SUP_PRF_18-25_mini_spin.csv"))
dfpro_18_25_cesd <- read.csv(here("Data", "Raw_data", "Prolific participants", "Prolific participants (18-25)","SUP_PRF_18-25_ces_d.csv"))

# extract CES-D scores
CESD_data_clean <- dfpro_18_25_cesd[, c(30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50,
                                        52, 54, 56, 58, 60, 62, 64, 66, 68, 69)]
# Identify non-ID variable columns
non_id_cols <- setdiff(names(CESD_data_clean), "Random_ID")

# Convert non-ID variables to numeric
CESD_data_clean[, non_id_cols] <- lapply(CESD_data_clean[, non_id_cols], as.numeric)

#change numbers to match CES-D scoring system
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

#scoring---
CESD_scores <-  numeric(length(CESD_data_clean$Random_ID))
CESD_scores <- 0
Questions <- c(1:20)

for (i in 1:nrow(CESD_data_clean)) {
  CESD_score <- sum(CESD_data_clean[i, Questions])
  CESD_scores[i] <- CESD_score
}

CESD_data_clean$CESD_score <-  CESD_scores

CESD_data_clean$Depression_Threshold<- 0
CESD_data_clean$Depression_status <- "Not_Depressed"

#Assign "1" if they meet Depression threshold
for (i in 1:nrow(CESD_data_clean)) {
  if (CESD_data_clean$CESD_score[i] >= 16) {
    CESD_data_clean$Depression_Threshold[i] <- 1
    CESD_data_clean$Depression_status[i] <- "Depressed"
  }
}

# We have now different names for different screens:
# Prediction 
# Feedback 
# First 4 trials are with the histograms and are called "Trials_Histogram" in Display column instead of "Trials"
# The trial numbers start from 1 again after the histograms so we need to concatenate them all.
# Let's rename them to "Trials" to extract them together with the rest of the trials

dfpro_18_25$Trial.Number[dfpro_18_25$Display == "Trials "] <- dfpro_18_25$Trial.Number[dfpro_18_25$Display == "Trials "] + 4

dfpro_18_25 <- dfpro_18_25 %>%
  mutate(Screen = if_else(Screen == "Prediction", "Prediction ", Screen))


dfpro_18_25 <- dfpro_18_25 %>%
  mutate(Display = if_else(Display == "Trials_Histogram", "Trials ", Display))

dfpro_18_25_spin <- dfpro_18_25_spin %>%
  rename(
    "Q1" =  "Rating.Scale.object.2.Fear.of.embarrassment.causes.me.to.avoid.doing.things.or.speaking.to.people.",
    "Q2" =  "Rating.Scale.object.2.I.avoid.activities.in.which.I.am.the.center.of.attention.",
    "Q3" = "Rating.Scale.object.2.Being.embarrassed.or.looking.stupid.are.among.my.worst.fears."
  )

dfpro_18_25_spin <- dfpro_18_25_spin %>%
  rowwise() %>%
  mutate(mini_SPIN_total = sum(c(Q1, Q2, Q3), na.rm = TRUE)) %>%
  ungroup()


# create a new dfpro_18_25_selected that only keeps the columns we need
dfpro_18_25_selected <- dfpro_18_25[, c("Trial.Number", "Display", "Screen", "Response.Type", "Response", 
                                        "Reaction.Time", "Spreadsheet..Histogram", "Spreadsheet..Feedback", "Random_ID")]

dfpro_18_25_selected <- dfpro_18_25_selected %>% dplyr::filter(Response.Type != "info")

dfpro_18_25_selected <- dfpro_18_25_selected %>%
  mutate(Response = as.numeric(as.character(Response))) %>%
  dplyr::filter(!is.na(Response))

dfpro_18_25_selected$Screen <- ifelse(dfpro_18_25_selected$Trial.Number == 1 & (dfpro_18_25_selected$Display == "Mood " | dfpro_18_25_selected$Display == "Anxiety "), "BL_1",
                                      ifelse(dfpro_18_25_selected$Trial.Number == 2 & (dfpro_18_25_selected$Display == "Mood " | dfpro_18_25_selected$Display == "Anxiety "), "BL_2", 
                                             dfpro_18_25_selected$Screen))

dfpro_18_25_selected$m_hist <- ifelse(dfpro_18_25_selected$Spreadsheet..Histogram == "h2.png", 29,
                                      ifelse(dfpro_18_25_selected$Spreadsheet..Histogram == "h3.png", 37,
                                             ifelse(dfpro_18_25_selected$Spreadsheet..Histogram == "h6.png", 63,
                                                    ifelse(dfpro_18_25_selected$Spreadsheet..Histogram == "h7.png", 71, NA))))  


# get rid of the rows we don't need: practice runs, continue button presses etc. and only keep Trials, Anxiety and Mood that include all variables we are interested in:
dfpro_18_25_selected <- dfpro_18_25_selected %>%
  dplyr::filter(
    (Display == "Trials ") |
      (Display == "Exit") |
      (Display == "Anxiety " & !Trial.Number %in% c(1, 2)) |
      (Display == "Mood " & !Trial.Number %in% c(1, 2)) 
  )

# # setting Mood and Anxiety trial numbers to match the trials starting from 1 (since we removed the baseline ones above)
dfpro_18_25_selected$Trial.Number <- ifelse(dfpro_18_25_selected$Display == 'Mood ' | dfpro_18_25_selected$Display == 'Anxiety ', dfpro_18_25_selected$Trial.Number - 2, dfpro_18_25_selected$Trial.Number)
dfpro_18_25_selected$Display <- ifelse(dfpro_18_25_selected$Display == 'Mood ' | dfpro_18_25_selected$Display == 'Anxiety ', "Trials", dfpro_18_25_selected$Display)

# renaming the feedback column to fdbk
names(dfpro_18_25_selected)[names(dfpro_18_25_selected) == 'Spreadsheet..Feedback'] <- 'fdbk'

# Now let's create columns for PE (fdbk-hist) and Subj_PE (fdbk_Prediction)

dfpro_18_25_selected$SubjPE <- ifelse(dfpro_18_25_selected$Screen == "Prediction ", dfpro_18_25_selected$fdbk - dfpro_18_25_selected$Response, NA)
dfpro_18_25_selected$PE <- ifelse(dfpro_18_25_selected$Screen == "Prediction ", dfpro_18_25_selected$fdbk - dfpro_18_25_selected$m_hist, NA)
# to keep just one repetition of feedback and mean histogram per trial per subject 
# and replace the rest with NA so that when we want to convert to long format we won't have duplicates
dfpro_18_25_selected$fdbk <- ifelse(dfpro_18_25_selected$Screen == "Prediction ", dfpro_18_25_selected$fdbk, NA)
dfpro_18_25_selected$m_hist <- ifelse(dfpro_18_25_selected$Screen == "Prediction ", dfpro_18_25_selected$m_hist, NA)
dfpro_18_25_selected$stress_level <- ifelse(dfpro_18_25_selected$Screen == "Screen 12", dfpro_18_25_selected$Response, NA)

long_dfpro_18_25_selected <- dfpro_18_25_selected %>%
  dplyr::select(Random_ID, Trial.Number, fdbk, m_hist, PE, SubjPE, stress_level) %>%
  pivot_longer(cols = c(fdbk, m_hist, PE, SubjPE, stress_level),
               names_to = "Screen",
               values_to = "Response")

# This will keep only the rows where 'Response' is not NA
long_dfpro_18_25_selected <- long_dfpro_18_25_selected[!is.na(long_dfpro_18_25_selected$Response), ]

# Selecting relevant columns from the original dfpro_18_25_selected
dfpro_18_25_selected <- dfpro_18_25_selected %>%
  dplyr::select(-c(fdbk, m_hist, PE, SubjPE, stress_level))

# Binding the rows together
dfpro_18_25_selected <- bind_rows(dfpro_18_25_selected, long_dfpro_18_25_selected)

dfpro_18_25_selected$Response <- as.numeric(dfpro_18_25_selected$Response)

# looking at the relationship between PE and SubjPE
dfpro_18_25_selected_PE <- subset(dfpro_18_25_selected, Screen == "PE")
names(dfpro_18_25_selected_PE)[names(dfpro_18_25_selected_PE) == "Screen"] <- "Screen_PE"
names(dfpro_18_25_selected_PE)[names(dfpro_18_25_selected_PE) == "Response"] <- "Response_PE"

dfpro_18_25_selected_SubjPE <- subset(dfpro_18_25_selected, Screen == "SubjPE")
names(dfpro_18_25_selected_SubjPE)[names(dfpro_18_25_selected_SubjPE) == "Screen"] <- "Screen_SubjPE"
names(dfpro_18_25_selected_SubjPE)[names(dfpro_18_25_selected_SubjPE) == "Response"] <- "Response_SubjPE"

dfpro_18_25_selected_fdbk <- subset(dfpro_18_25_selected, Screen == "fdbk")
names(dfpro_18_25_selected_fdbk)[names(dfpro_18_25_selected_fdbk) == "Screen"] <- "Screen_fdbk"
names(dfpro_18_25_selected_fdbk)[names(dfpro_18_25_selected_fdbk) == "Response"] <- "Response_fdbk"

dfpro_18_25_selected_hist <- subset(dfpro_18_25_selected, Screen == "m_hist")
names(dfpro_18_25_selected_hist)[names(dfpro_18_25_selected_hist) == "Screen"] <- "Screen_m_hist"
names(dfpro_18_25_selected_hist)[names(dfpro_18_25_selected_hist) == "Response"] <- "Response_m_hist"

dfpro_18_25_selected_stress_level <- subset(dfpro_18_25_selected, Screen == "Screen 12")
names(dfpro_18_25_selected_stress_level)[names(dfpro_18_25_selected_stress_level) == "Screen"] <- "Screen_stress_level"
names(dfpro_18_25_selected_stress_level)[names(dfpro_18_25_selected_stress_level) == "Response"] <- "Response_stress_level"

dfpro_18_25_selected_predic <- subset(dfpro_18_25_selected, Screen == "Prediction ")
names(dfpro_18_25_selected_predic)[names(dfpro_18_25_selected_predic) == "Screen"] <- "Screen_pred"
names(dfpro_18_25_selected_predic)[names(dfpro_18_25_selected_predic) == "Response"] <- "Response_pred"


list_of_dfs_21 <- list(dfpro_18_25_selected_PE, dfpro_18_25_selected_fdbk, dfpro_18_25_selected_hist, dfpro_18_25_selected_predic, dfpro_18_25_selected_SubjPE)

# Using reduce with inner_join
merged_dfpro_18_25_selected <- reduce(list_of_dfs_21, inner_join, by = c("Random_ID", "Trial.Number"))

dfpro_18_25_selected_Anxious <- subset(dfpro_18_25_selected, Screen == "Anxious")
names(dfpro_18_25_selected_Anxious)[names(dfpro_18_25_selected_Anxious) == "Screen"] <- "Screen_Ax"
names(dfpro_18_25_selected_Anxious)[names(dfpro_18_25_selected_Anxious) == "Response"] <- "Response_Ax"

dfpro_18_25_selected_Happy <- subset(dfpro_18_25_selected, Screen == "Happy")
# dfpro_18_25_selected_fdbk <- subset(dfpro_18_25_selected, Screen == "Prediction")
names(dfpro_18_25_selected_Happy)[names(dfpro_18_25_selected_Happy) == "Screen"] <- "Screen_H"
names(dfpro_18_25_selected_Happy)[names(dfpro_18_25_selected_Happy) == "Response"] <- "Response_H"

final_df_18_25_Pro <- merged_dfpro_18_25_selected %>%
  inner_join(dfpro_18_25_selected_Anxious, by = c("Random_ID", "Trial.Number")) %>%
  inner_join(dfpro_18_25_selected_Happy, by = c("Random_ID", "Trial.Number"))


# subset only the columns we are interested in
final_df_18_25_Pro <- final_df_18_25_Pro[c("Trial.Number", "Random_ID", "Response_H", "Response_Ax", "Response_fdbk", "Response_SubjPE", "Response_PE", "Response_m_hist", "Response_pred")]

final_df_18_25_Pro <- final_df_18_25_Pro %>%
  left_join(dfpro_18_25_spin %>% dplyr::select(Random_ID, mini_SPIN_total), by = "Random_ID")

final_df_18_25_Pro <- final_df_18_25_Pro %>%
  left_join(dfpro_18_25_selected_stress_level %>% dplyr::select(Random_ID, Response_stress_level), by = "Random_ID")

final_df_18_25_Pro <- final_df_18_25_Pro %>%
  left_join(CESD_data_clean %>% dplyr::select(Random_ID, CESD_score, Depression_Threshold, Depression_status), by = "Random_ID")

final_df_18_25_Pro$Social_Anxiety <- ifelse(final_df_18_25_Pro$mini_SPIN_total >= 6, "high", "low")

# excluding people with problematic performance according to Jess's video check
final_df_18_25_Pro <- final_df_18_25_Pro[!(final_df_18_25_Pro$Random_ID %in% c("SUPPRF73571")), ]



final_df_18_25_Pro %>%
  group_by(Random_ID) %>%
  summarise(Trial.Number = n())

write.csv(final_df_18_25_Pro, "surprise_study_18_25_Pro_variables.csv", row.names = FALSE)

# Extract Prolific 26-45 data ---------------------------------------------
dfpro_26_45 <- read.csv(here("Data", "Raw_data", "Prolific participants", "Prolific participants (26-45)", "SUP_PRF_26-45_task_main.csv"))
dfpro_26_45_spin <- read.csv(here("Data", "Raw_data", "Prolific participants", "Prolific participants (26-45)","SUP_PRF_26-45_mini_spin.csv"))
dfpro_26_45_cesd <- read.csv(here("Data", "Raw_data", "Prolific participants", "Prolific participants (26-45)","SUP_PRF_26-45_ces_d.csv"))

# extract CES-D scores
CESD_data_clean <- dfpro_26_45_cesd[, c(30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50,
                                        52, 54, 56, 58, 60, 62, 64, 66, 68, 69)]
# Identify non-ID variable columns
non_id_cols <- setdiff(names(CESD_data_clean), "Random_ID")

# Convert non-ID variables to numeric
CESD_data_clean[, non_id_cols] <- lapply(CESD_data_clean[, non_id_cols], as.numeric)

#change numbers to match CES-D scoring system
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

#scoring---
CESD_scores <-  numeric(length(CESD_data_clean$Random_ID))
CESD_scores <- 0
Questions <- c(1:20)

for (i in 1:nrow(CESD_data_clean)) {
  CESD_score <- sum(CESD_data_clean[i, Questions])
  CESD_scores[i] <- CESD_score
}

CESD_data_clean$CESD_score <-  CESD_scores

CESD_data_clean$Depression_Threshold<- 0
CESD_data_clean$Depression_status <- "Not_Depressed"

#Assign "1" if they meet Depression threshold
for (i in 1:nrow(CESD_data_clean)) {
  if (CESD_data_clean$CESD_score[i] >= 16) {
    CESD_data_clean$Depression_Threshold[i] <- 1
    CESD_data_clean$Depression_status[i] <- "Depressed"
  }
}

# We have now different names for different screens:
# Prediction 
# Feedback 
# First 4 trials are with the histograms and are called "Trials_Histogram" in Display column instead of "Trials"
# The trial numbers start from 1 again after the histograms so we need to concatenate them all.
# Let's rename them to "Trials" to extract them together with the rest of the trials

dfpro_26_45$Trial.Number[dfpro_26_45$Display == "Trials "] <- dfpro_26_45$Trial.Number[dfpro_26_45$Display == "Trials "] + 4

dfpro_26_45 <- dfpro_26_45 %>%
  mutate(Screen = if_else(Screen == "Prediction", "Prediction ", Screen))


dfpro_26_45 <- dfpro_26_45 %>%
  mutate(Display = if_else(Display == "Trials_Histogram", "Trials ", Display))

dfpro_26_45_spin <- dfpro_26_45_spin %>%
  rename(
    "Q1" =  "Rating.Scale.object.2.Fear.of.embarrassment.causes.me.to.avoid.doing.things.or.speaking.to.people.",
    "Q2" =  "Rating.Scale.object.2.I.avoid.activities.in.which.I.am.the.center.of.attention.",
    "Q3" = "Rating.Scale.object.2.Being.embarrassed.or.looking.stupid.are.among.my.worst.fears."
  )

dfpro_26_45_spin <- dfpro_26_45_spin %>%
  rowwise() %>%
  mutate(mini_SPIN_total = sum(c(Q1, Q2, Q3), na.rm = TRUE)) %>%
  ungroup()


# create a new dfpro_26_45_selected that only keeps the columns we need
dfpro_26_45_selected <- dfpro_26_45[, c("Trial.Number", "Display", "Screen", "Response.Type", "Response", 
                                        "Reaction.Time", "Spreadsheet..Histogram", "Spreadsheet..Feedback", "Random_ID")]

dfpro_26_45_selected <- dfpro_26_45_selected %>% dplyr::filter(Response.Type != "info")

dfpro_26_45_selected <- dfpro_26_45_selected %>%
  mutate(Response = as.numeric(as.character(Response))) %>%
  dplyr::filter(!is.na(Response))

dfpro_26_45_selected$Screen <- ifelse(dfpro_26_45_selected$Trial.Number == 1 & (dfpro_26_45_selected$Display == "Mood " | dfpro_26_45_selected$Display == "Anxiety "), "BL_1",
                                      ifelse(dfpro_26_45_selected$Trial.Number == 2 & (dfpro_26_45_selected$Display == "Mood " | dfpro_26_45_selected$Display == "Anxiety "), "BL_2", 
                                             dfpro_26_45_selected$Screen))

dfpro_26_45_selected$m_hist <- ifelse(dfpro_26_45_selected$Spreadsheet..Histogram == "h2.png", 29,
                                      ifelse(dfpro_26_45_selected$Spreadsheet..Histogram == "h3.png", 37,
                                             ifelse(dfpro_26_45_selected$Spreadsheet..Histogram == "h6.png", 63,
                                                    ifelse(dfpro_26_45_selected$Spreadsheet..Histogram == "h7.png", 71, NA))))  


# get rid of the rows we don't need: practice runs, continue button presses etc. and only keep Trials, Anxiety and Mood that include all variables we are interested in:
dfpro_26_45_selected <- dfpro_26_45_selected %>%
  dplyr::filter(
    (Display == "Trials ") |
      (Display == "Exit") |
      (Display == "Anxiety " & !Trial.Number %in% c(1, 2)) |
      (Display == "Mood " & !Trial.Number %in% c(1, 2)) 
  )

# # setting Mood and Anxiety trial numbers to match the trials starting from 1 (since we removed the baseline ones above)
dfpro_26_45_selected$Trial.Number <- ifelse(dfpro_26_45_selected$Display == 'Mood ' | dfpro_26_45_selected$Display == 'Anxiety ', dfpro_26_45_selected$Trial.Number - 2, dfpro_26_45_selected$Trial.Number)
dfpro_26_45_selected$Display <- ifelse(dfpro_26_45_selected$Display == 'Mood ' | dfpro_26_45_selected$Display == 'Anxiety ', "Trials", dfpro_26_45_selected$Display)

# renaming the feedback column to fdbk
names(dfpro_26_45_selected)[names(dfpro_26_45_selected) == 'Spreadsheet..Feedback'] <- 'fdbk'

# Now let's create columns for PE (fdbk-hist) and Subj_PE (fdbk_Prediction)

dfpro_26_45_selected$SubjPE <- ifelse(dfpro_26_45_selected$Screen == "Prediction ", dfpro_26_45_selected$fdbk - dfpro_26_45_selected$Response, NA)
dfpro_26_45_selected$PE <- ifelse(dfpro_26_45_selected$Screen == "Prediction ", dfpro_26_45_selected$fdbk - dfpro_26_45_selected$m_hist, NA)
# to keep just one repetition of feedback and mean histogram per trial per subject 
# and replace the rest with NA so that when we want to convert to long format we won't have duplicates
dfpro_26_45_selected$fdbk <- ifelse(dfpro_26_45_selected$Screen == "Prediction ", dfpro_26_45_selected$fdbk, NA)
dfpro_26_45_selected$m_hist <- ifelse(dfpro_26_45_selected$Screen == "Prediction ", dfpro_26_45_selected$m_hist, NA)
dfpro_26_45_selected$stress_level <- ifelse(dfpro_26_45_selected$Screen == "Screen 12", dfpro_26_45_selected$Response, NA)

long_dfpro_26_45_selected <- dfpro_26_45_selected %>%
  dplyr::select(Random_ID, Trial.Number, fdbk, m_hist, PE, SubjPE, stress_level) %>%
  pivot_longer(cols = c(fdbk, m_hist, PE, SubjPE, stress_level),
               names_to = "Screen",
               values_to = "Response")

# This will keep only the rows where 'Response' is not NA
long_dfpro_26_45_selected <- long_dfpro_26_45_selected[!is.na(long_dfpro_26_45_selected$Response), ]

# Selecting relevant columns from the original dfpro_26_45_selected
dfpro_26_45_selected <- dfpro_26_45_selected %>%
  dplyr::select(-c(fdbk, m_hist, PE, SubjPE, stress_level))

# Binding the rows together
dfpro_26_45_selected <- bind_rows(dfpro_26_45_selected, long_dfpro_26_45_selected)

dfpro_26_45_selected$Response <- as.numeric(dfpro_26_45_selected$Response)

# looking at the relationship between PE and SubjPE
dfpro_26_45_selected_PE <- subset(dfpro_26_45_selected, Screen == "PE")
names(dfpro_26_45_selected_PE)[names(dfpro_26_45_selected_PE) == "Screen"] <- "Screen_PE"
names(dfpro_26_45_selected_PE)[names(dfpro_26_45_selected_PE) == "Response"] <- "Response_PE"

dfpro_26_45_selected_SubjPE <- subset(dfpro_26_45_selected, Screen == "SubjPE")
names(dfpro_26_45_selected_SubjPE)[names(dfpro_26_45_selected_SubjPE) == "Screen"] <- "Screen_SubjPE"
names(dfpro_26_45_selected_SubjPE)[names(dfpro_26_45_selected_SubjPE) == "Response"] <- "Response_SubjPE"

dfpro_26_45_selected_fdbk <- subset(dfpro_26_45_selected, Screen == "fdbk")
names(dfpro_26_45_selected_fdbk)[names(dfpro_26_45_selected_fdbk) == "Screen"] <- "Screen_fdbk"
names(dfpro_26_45_selected_fdbk)[names(dfpro_26_45_selected_fdbk) == "Response"] <- "Response_fdbk"

dfpro_26_45_selected_hist <- subset(dfpro_26_45_selected, Screen == "m_hist")
names(dfpro_26_45_selected_hist)[names(dfpro_26_45_selected_hist) == "Screen"] <- "Screen_m_hist"
names(dfpro_26_45_selected_hist)[names(dfpro_26_45_selected_hist) == "Response"] <- "Response_m_hist"

dfpro_26_45_selected_stress_level <- subset(dfpro_26_45_selected, Screen == "Screen 12")
names(dfpro_26_45_selected_stress_level)[names(dfpro_26_45_selected_stress_level) == "Screen"] <- "Screen_stress_level"
names(dfpro_26_45_selected_stress_level)[names(dfpro_26_45_selected_stress_level) == "Response"] <- "Response_stress_level"

dfpro_26_45_selected_predic <- subset(dfpro_26_45_selected, Screen == "Prediction ")
names(dfpro_26_45_selected_predic)[names(dfpro_26_45_selected_predic) == "Screen"] <- "Screen_pred"
names(dfpro_26_45_selected_predic)[names(dfpro_26_45_selected_predic) == "Response"] <- "Response_pred"

list_of_dfs_21 <- list(dfpro_26_45_selected_PE, dfpro_26_45_selected_fdbk, dfpro_26_45_selected_hist, dfpro_26_45_selected_predic, dfpro_26_45_selected_SubjPE)

# Using reduce with inner_join
merged_dfpro_26_45_selected <- reduce(list_of_dfs_21, inner_join, by = c("Random_ID", "Trial.Number"))

dfpro_26_45_selected_Anxious <- subset(dfpro_26_45_selected, Screen == "Anxious")
names(dfpro_26_45_selected_Anxious)[names(dfpro_26_45_selected_Anxious) == "Screen"] <- "Screen_Ax"
names(dfpro_26_45_selected_Anxious)[names(dfpro_26_45_selected_Anxious) == "Response"] <- "Response_Ax"

dfpro_26_45_selected_Happy <- subset(dfpro_26_45_selected, Screen == "Happy")
# dfpro_26_45_selected_fdbk <- subset(dfpro_26_45_selected, Screen == "Prediction")
names(dfpro_26_45_selected_Happy)[names(dfpro_26_45_selected_Happy) == "Screen"] <- "Screen_H"
names(dfpro_26_45_selected_Happy)[names(dfpro_26_45_selected_Happy) == "Response"] <- "Response_H"

final_df_26_45_Pro <- merged_dfpro_26_45_selected %>%
  inner_join(dfpro_26_45_selected_Anxious, by = c("Random_ID", "Trial.Number")) %>%
  inner_join(dfpro_26_45_selected_Happy, by = c("Random_ID", "Trial.Number"))


# subset only the columns we are interested in
final_df_26_45_Pro <- final_df_26_45_Pro[c("Trial.Number", "Random_ID", "Response_H", "Response_Ax", "Response_fdbk", "Response_SubjPE", "Response_PE", "Response_m_hist", "Response_pred")]

final_df_26_45_Pro <- final_df_26_45_Pro %>%
  left_join(dfpro_26_45_spin %>% dplyr::select(Random_ID, mini_SPIN_total), by = "Random_ID")

final_df_26_45_Pro <- final_df_26_45_Pro %>%
  left_join(dfpro_26_45_selected_stress_level %>% dplyr::select(Random_ID, Response_stress_level), by = "Random_ID")

final_df_26_45_Pro <- final_df_26_45_Pro %>%
  left_join(CESD_data_clean %>% dplyr::select(Random_ID, CESD_score, Depression_Threshold, Depression_status), by = "Random_ID")

final_df_26_45_Pro$Social_Anxiety <- ifelse(final_df_26_45_Pro$mini_SPIN_total >= 6, "high", "low")

# excluding people with problematic performance according to Jess's video check
final_df_26_45_Pro <- final_df_26_45_Pro[!(final_df_26_45_Pro$Random_ID %in% c("SUPPRF42289", "SUPPRF64792", "SUPPRF95116")), ]


final_df_26_45_Pro %>%
  group_by(Random_ID) %>%
  summarise(Trial.Number = n())

write.csv(final_df_26_45_Pro, "surprise_study_26_45_Pro_variables.csv", row.names = FALSE)

# Extract Local Community 18-25 data --------------------------------------
dfcom_18_25_1 <- read.csv(here("Data", "Raw_data", "Local Community Participants", "Local Community Participants v1", "SUP_COM_18-25_v1_task_main.csv"))
dfcom_18_25_2 <- read.csv(here("Data", "Raw_data", "Local Community Participants", "Local Community Participants v2", "SUP_COM_18-25_v2_task_main.csv"))
dfcom_18_25_2$X.1 <- NULL


dfcom_18_25 <- rbind(dfcom_18_25_1, dfcom_18_25_2)

dfcom_18_25_spin_1 <- read.csv(here("Data", "Raw_data", "Local Community Participants", "Local Community Participants v1", "SUP_COM_18-25_v1_mini_spin.csv"))
dfcom_18_25_spin_2 <- read.csv(here("Data", "Raw_data", "Local Community Participants", "Local Community Participants v2", "SUP_COM_18-25_v2_mini_spin.csv"))
dfcom_18_25_spin_2$X <- NULL

dfcom_18_25_spin <- rbind(dfcom_18_25_spin_1, dfcom_18_25_spin_2)

dfcom_18_25_ces_d_1 <- read.csv(here("Data", "Raw_data", "Local Community Participants", "Local Community Participants v1", "SUP_COM_18-25_v1_ces_d.csv"))
dfcom_18_25_ces_d_2 <- read.csv(here("Data", "Raw_data", "Local Community Participants", "Local Community Participants v2", "SUP_COM_18-25_v2_ces_d.csv"))
dfcom_18_25_ces_d_2$X <- NULL


dfcom_18_25_ces_d <- rbind(dfcom_18_25_ces_d_1, dfcom_18_25_ces_d_2)

# extract CES-D scores
CESD_data_clean <- dfcom_18_25_ces_d[, c(31, 33, 35, 37, 39, 41, 43, 45,47,49,
                                         51, 53, 55, 57, 59, 61, 63, 65, 67, 69, 70 )]
# Identify non-ID variable columns
non_id_cols <- setdiff(names(CESD_data_clean), "Random_ID_new")

# Convert non-ID variables to numeric
CESD_data_clean[, non_id_cols] <- lapply(CESD_data_clean[, non_id_cols], as.numeric)

#change numbers to match CES-D scoring system
excluded_columns <-  c("Random_ID_new", "Multiple.Choice.Grid.object.3.4...I.felt.I.was.just.as.good.as.other.people..Quantised",
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

#scoring---
CESD_scores <-  numeric(length(CESD_data_clean$Random_ID_new))
CESD_scores <- 0
Questions <- c(1:20)

for (i in 1:nrow(CESD_data_clean)) {
  CESD_score <- sum(CESD_data_clean[i, Questions])
  CESD_scores[i] <- CESD_score
}

CESD_data_clean$CESD_score <-  CESD_scores

CESD_data_clean$Depression_Threshold<- 0
CESD_data_clean$Depression_status <- "Not_Depressed"

#Assign "1" if they meet Depression threshold
for (i in 1:nrow(CESD_data_clean)) {
  if (CESD_data_clean$CESD_score[i] >= 16) {
    CESD_data_clean$Depression_Threshold[i] <- 1
    CESD_data_clean$Depression_status[i] <- "Depressed"
  }
}

# We have now different names for different screens:
# Prediction 
# Feedback 
# First 4 trials are with the histograms and are called "Trials_Histogram" in Display column instead of "Trials"
# The trial numbers start from 1 again after the histograms so we need to concatenate them all.
# Let's rename them to "Trials" to extract them together with the rest of the trials

dfcom_18_25$Trial.Number[dfcom_18_25$Display == "Trials "] <- dfcom_18_25$Trial.Number[dfcom_18_25$Display == "Trials "] + 4

dfcom_18_25 <- dfcom_18_25 %>%
  mutate(Screen = if_else(Screen == "Prediction", "Prediction ", Screen))


dfcom_18_25 <- dfcom_18_25 %>%
  mutate(Display = if_else(Display == "Trials_Histogram", "Trials ", Display))

dfcom_18_25_spin <- dfcom_18_25_spin %>%
  rename(
    "Q1" =  "Rating.Scale.object.2.Fear.of.embarrassment.causes.me.to.avoid.doing.things.or.speaking.to.people.",
    "Q2" =  "Rating.Scale.object.2.I.avoid.activities.in.which.I.am.the.center.of.attention.",
    "Q3" = "Rating.Scale.object.2.Being.embarrassed.or.looking.stupid.are.among.my.worst.fears."
  )

dfcom_18_25_spin <- dfcom_18_25_spin %>%
  rowwise() %>%
  mutate(mini_SPIN_total = sum(c(Q1, Q2, Q3), na.rm = TRUE)) %>%
  ungroup()


# create a new dfc_18_25 that only keeps the columns we need
dfc_18_25 <- dfcom_18_25[, c("Trial.Number", "Display", "Screen", "Response.Type", "Response", 
                             "Reaction.Time", "Spreadsheet..Histogram", "Spreadsheet..Feedback", "Random_ID_new")]

dfc_18_25 <- dfc_18_25 %>% dplyr::filter(Response.Type != "info")

dfc_18_25 <- dfc_18_25 %>%
  mutate(Response = as.numeric(as.character(Response))) %>%
  dplyr::filter(!is.na(Response))

dfc_18_25$Screen <- ifelse(dfc_18_25$Trial.Number == 1 & (dfc_18_25$Display == "Mood " | dfc_18_25$Display == "Anxiety "), "BL_1",
                           ifelse(dfc_18_25$Trial.Number == 2 & (dfc_18_25$Display == "Mood " | dfc_18_25$Display == "Anxiety "), "BL_2", 
                                  dfc_18_25$Screen))

dfc_18_25$m_hist <- ifelse(dfc_18_25$Spreadsheet..Histogram == "h2.png", 29,
                           ifelse(dfc_18_25$Spreadsheet..Histogram == "h3.png", 37,
                                  ifelse(dfc_18_25$Spreadsheet..Histogram == "h6.png", 63,
                                         ifelse(dfc_18_25$Spreadsheet..Histogram == "h7.png", 71, NA))))  


# get rid of the rows we don't need: practice runs, continue button presses etc. and only keep Trials, Anxiety and Mood that include all variables we are interested in:
dfc_18_25 <- dfc_18_25 %>%
  dplyr::filter(
    (Display == "Trials ") |
      (Display == "Exit") |
      (Display == "Anxiety " & !Trial.Number %in% c(1, 2)) |
      (Display == "Mood " & !Trial.Number %in% c(1, 2)) 
  )

# # setting Mood and Anxiety trial numbers to match the trials starting from 1 (since we removed the baseline ones above)
dfc_18_25$Trial.Number <- ifelse(dfc_18_25$Display == 'Mood ' | dfc_18_25$Display == 'Anxiety ', dfc_18_25$Trial.Number - 2, dfc_18_25$Trial.Number)
dfc_18_25$Display <- ifelse(dfc_18_25$Display == 'Mood ' | dfc_18_25$Display == 'Anxiety ', "Trials", dfc_18_25$Display)

# renaming the feedback column to fdbk
names(dfc_18_25)[names(dfc_18_25) == 'Spreadsheet..Feedback'] <- 'fdbk'

# Now let's create columns for PE (fdbk-hist) and Subj_PE (fdbk_Prediction)

dfc_18_25$SubjPE <- ifelse(dfc_18_25$Screen == "Prediction ", dfc_18_25$fdbk - dfc_18_25$Response, NA)
dfc_18_25$PE <- ifelse(dfc_18_25$Screen == "Prediction ", dfc_18_25$fdbk - dfc_18_25$m_hist, NA)
# to keep just one repetition of feedback and mean histogram per trial per subject 
# and replace the rest with NA so that when we want to convert to long format we won't have duplicates
dfc_18_25$fdbk <- ifelse(dfc_18_25$Screen == "Prediction ", dfc_18_25$fdbk, NA)
dfc_18_25$m_hist <- ifelse(dfc_18_25$Screen == "Prediction ", dfc_18_25$m_hist, NA)
dfc_18_25$stress_level <- ifelse(dfc_18_25$Screen == "Screen 12", dfc_18_25$Response, NA)

long_dfc_18_25 <- dfc_18_25 %>%
  dplyr::select(Random_ID_new, Trial.Number, fdbk, m_hist, PE, SubjPE, stress_level) %>%
  pivot_longer(cols = c(fdbk, m_hist, PE, SubjPE, stress_level),
               names_to = "Screen",
               values_to = "Response")

# This will keep only the rows where 'Response' is not NA
long_dfc_18_25 <- long_dfc_18_25[!is.na(long_dfc_18_25$Response), ]

# Selecting relevant columns from the original dfc_18_25
dfc_18_25 <- dfc_18_25 %>%
  dplyr::select(-c(fdbk, m_hist, PE, SubjPE, stress_level))

# Binding the rows together
dfc_18_25 <- bind_rows(dfc_18_25, long_dfc_18_25)

dfc_18_25$Response <- as.numeric(dfc_18_25$Response)

# looking at the relationship between PE and SubjPE
dfc_18_25_PE <- subset(dfc_18_25, Screen == "PE")
names(dfc_18_25_PE)[names(dfc_18_25_PE) == "Screen"] <- "Screen_PE"
names(dfc_18_25_PE)[names(dfc_18_25_PE) == "Response"] <- "Response_PE"

dfc_18_25_SubjPE <- subset(dfc_18_25, Screen == "SubjPE")
names(dfc_18_25_SubjPE)[names(dfc_18_25_SubjPE) == "Screen"] <- "Screen_SubjPE"
names(dfc_18_25_SubjPE)[names(dfc_18_25_SubjPE) == "Response"] <- "Response_SubjPE"

dfc_18_25_fdbk <- subset(dfc_18_25, Screen == "fdbk")
names(dfc_18_25_fdbk)[names(dfc_18_25_fdbk) == "Screen"] <- "Screen_fdbk"
names(dfc_18_25_fdbk)[names(dfc_18_25_fdbk) == "Response"] <- "Response_fdbk"

dfc_18_25_hist <- subset(dfc_18_25, Screen == "m_hist")
names(dfc_18_25_hist)[names(dfc_18_25_hist) == "Screen"] <- "Screen_m_hist"
names(dfc_18_25_hist)[names(dfc_18_25_hist) == "Response"] <- "Response_m_hist"

dfc_18_25_stress_level <- subset(dfc_18_25, Screen == "Screen 12")
names(dfc_18_25_stress_level)[names(dfc_18_25_stress_level) == "Screen"] <- "Screen_stress_level"
names(dfc_18_25_stress_level)[names(dfc_18_25_stress_level) == "Response"] <- "Response_stress_level"

dfc_18_25_predic <- subset(dfc_18_25, Screen == "Prediction ")
names(dfc_18_25_predic)[names(dfc_18_25_predic) == "Screen"] <- "Screen_pred"
names(dfc_18_25_predic)[names(dfc_18_25_predic) == "Response"] <- "Response_pred"

list_of_dfs_21 <- list(dfc_18_25_PE, dfc_18_25_fdbk, dfc_18_25_hist, dfc_18_25_predic, dfc_18_25_SubjPE)

# Using reduce with inner_join
merged_dfc_18_25 <- reduce(list_of_dfs_21, inner_join, by = c("Random_ID_new", "Trial.Number"))

dfc_18_25_Anxious <- subset(dfc_18_25, Screen == "Anxious")
names(dfc_18_25_Anxious)[names(dfc_18_25_Anxious) == "Screen"] <- "Screen_Ax"
names(dfc_18_25_Anxious)[names(dfc_18_25_Anxious) == "Response"] <- "Response_Ax"

dfc_18_25_Happy <- subset(dfc_18_25, Screen == "Happy")
# dfc_18_25_fdbk <- subset(dfc_18_25, Screen == "Prediction")
names(dfc_18_25_Happy)[names(dfc_18_25_Happy) == "Screen"] <- "Screen_H"
names(dfc_18_25_Happy)[names(dfc_18_25_Happy) == "Response"] <- "Response_H"

final_df_18_25_com <- merged_dfc_18_25 %>%
  inner_join(dfc_18_25_Anxious, by = c("Random_ID_new", "Trial.Number")) %>%
  inner_join(dfc_18_25_Happy, by = c("Random_ID_new", "Trial.Number"))


# subset only the columns we are interested in
final_df_18_25_com <- final_df_18_25_com[c("Trial.Number", "Random_ID_new", "Response_H", "Response_Ax", "Response_fdbk", "Response_SubjPE", "Response_PE", "Response_m_hist", "Response_pred")]

final_df_18_25_com <- final_df_18_25_com %>%
  left_join(dfcom_18_25_spin %>% dplyr::select(Random_ID_new, mini_SPIN_total), by = "Random_ID_new")

final_df_18_25_com <- final_df_18_25_com %>%
  left_join(dfc_18_25_stress_level %>% dplyr::select(Random_ID_new, Response_stress_level), by = "Random_ID_new")

final_df_18_25_com <- final_df_18_25_com %>%
  left_join(CESD_data_clean %>% dplyr::select(Random_ID_new, CESD_score, Depression_Threshold, Depression_status), by = "Random_ID_new")

final_df_18_25_com$Social_Anxiety <- ifelse(final_df_18_25_com$mini_SPIN_total >= 6, "high", "low")

# excluding people with problematic performance according to Jess's video check
final_df_18_25_com <- final_df_18_25_com[!(final_df_18_25_com$Random_ID_new %in% c("SUPLCM31721", "SUPLCM52921", "SUPLCM91399", "SUPLCM18538")), ]


final_df_18_25_com %>%
  group_by(Random_ID_new) %>%
  summarise(Trial.Number = n())

write.csv(final_df_18_25_com, "surprise_study_18_25_com_variables.csv", row.names = FALSE)

# Extract School data -----------------------------------------------------
df_14_18_1 <- read.csv(here("Data", "Raw_data", "School participants", "surprise_study_schools_v7", "SUP_SCH_v7_task_main.csv"))
df_14_18_2 <- read.csv(here("Data", "Raw_data", "School participants", "surprise_study_schools_v9", "SUP_SCH_v9_task_main.csv"))

df_14_18 <- rbind(df_14_18_1, df_14_18_2)

df_14_18_spin_1 <- read.csv(here("Data", "Raw_data", "School participants", "surprise_study_schools_v7", "SUP_SCH_v7_mini_spin.csv"))
df_14_18_spin_2 <- read.csv(here("Data", "Raw_data", "School participants", "surprise_study_schools_v9","SUP_SCH_v9_mini_spin.csv"))

df_14_18_spin <- rbind(df_14_18_spin_1,df_14_18_spin_2)

df_students_rcads_1 <- read.csv(here("Data", "Raw_data", "School participants", "surprise_study_schools_v7","SUP_SCH_v7_rcads.csv"))
df_students_rcads_2 <- read.csv(here("Data", "Raw_data", "School participants", "surprise_study_schools_v9","SUP_SCH_v9_rcads.csv"))
df_students_rcads_2 <- df_students_rcads_2[, !(colnames(df_students_rcads_2) %in% "Participant.External.Session.ID")]

df_students_rcads<- rbind(df_students_rcads_1,df_students_rcads_2)
# extract RCAD scores
df_students_rcads_clean <- df_students_rcads[,122:128]

# We have now different names for different screens:
# Prediction 
# Feedback 
# First 4 trials are with the histograms and are called "Trials_Histogram" in Display column instead of "Trials"
# The trial numbers start from 1 again after the histograms so we need to concatenate them all.
# Let's rename them to "Trials" to extract them together with the rest of the trials


df_14_18$Trial.Number[df_14_18$Display == "Trials "] <- df_14_18$Trial.Number[df_14_18$Display == "Trials "] + 4

df_14_18 <- df_14_18 %>%
  mutate(Screen = if_else(Screen == "Prediction", "Prediction ", Screen))


df_14_18 <- df_14_18 %>%
  mutate(Display = if_else(Display == "Trials_Histogram", "Trials ", Display))

df_14_18_spin <- df_14_18_spin %>%
  rename(
    "Q1" =  "Rating.Scale.object.2.Fear.of.embarrassment.causes.me.to.avoid.doing.things.or.speaking.to.people.",
    "Q2" =  "Rating.Scale.object.2.I.avoid.activities.in.which.I.am.the.center.of.attention.",
    "Q3" = "Rating.Scale.object.2.Being.embarrassed.or.looking.stupid.are.among.my.worst.fears."
  )

df_14_18_spin <- df_14_18_spin %>%
  rowwise() %>%
  mutate(mini_SPIN_total = sum(c(Q1, Q2, Q3), na.rm = TRUE)) %>%
  ungroup()


# create a new df_students that only keeps the columns we need
df_students <- df_14_18[, c("Trial.Number", "Display", "Screen", "Response.Type", "Response", 
                            "Reaction.Time", "Spreadsheet..Histogram", "Spreadsheet..Feedback", "Random_ID_new")]

df_students <- df_students %>% dplyr::filter(Response.Type != "info")

df_students <- df_students %>%
  mutate(Response = as.numeric(as.character(Response))) %>%
  dplyr::filter(!is.na(Response))

df_students$Screen <- ifelse(df_students$Trial.Number == 1 & (df_students$Display == "Mood " | df_students$Display == "Anxiety "), "BL_1",
                             ifelse(df_students$Trial.Number == 2 & (df_students$Display == "Mood " | df_students$Display == "Anxiety "), "BL_2", 
                                    df_students$Screen))

df_students$m_hist <- ifelse(df_students$Spreadsheet..Histogram == "h2.png", 29,
                             ifelse(df_students$Spreadsheet..Histogram == "h3.png", 37,
                                    ifelse(df_students$Spreadsheet..Histogram == "h6.png", 63,
                                           ifelse(df_students$Spreadsheet..Histogram == "h7.png", 71, NA))))  


# get rid of the rows we don't need: practice runs, continue button presses etc. and only keep Trials, Anxiety and Mood that include all variables we are interested in:
df_students <- df_students %>%
  dplyr::filter(
    (Display == "Trials ") |
      (Display == "Exit") |
      (Display == "Anxiety " & !Trial.Number %in% c(1, 2)) |
      (Display == "Mood " & !Trial.Number %in% c(1, 2)) 
  )

# # setting Mood and Anxiety trial numbers to match the trials starting from 1 (since we removed the baseline ones above)
df_students$Trial.Number <- ifelse(df_students$Display == 'Mood ' | df_students$Display == 'Anxiety ', df_students$Trial.Number - 2, df_students$Trial.Number)
df_students$Display <- ifelse(df_students$Display == 'Mood ' | df_students$Display == 'Anxiety ', "Trials", df_students$Display)

# renaming the feedback column to fdbk
names(df_students)[names(df_students) == 'Spreadsheet..Feedback'] <- 'fdbk'

# Now let's create columns for PE (fdbk-hist) and Subj_PE (fdbk_Prediction)

df_students$SubjPE <- ifelse(df_students$Screen == "Prediction ", df_students$fdbk - df_students$Response, NA)
df_students$PE <- ifelse(df_students$Screen == "Prediction ", df_students$fdbk - df_students$m_hist, NA)
# to keep just one repetition of feedback and mean histogram per trial per subject 
# and replace the rest with NA so that when we want to convert to long format we won't have duplicates
df_students$fdbk <- ifelse(df_students$Screen == "Prediction ", df_students$fdbk, NA)
df_students$m_hist <- ifelse(df_students$Screen == "Prediction ", df_students$m_hist, NA)
df_students$stress_level <- ifelse(df_students$Screen == "Screen 12", df_students$Response, NA)

long_df_students <- df_students %>%
  dplyr::select(Random_ID_new, Trial.Number, fdbk, m_hist, PE, SubjPE, stress_level) %>%
  pivot_longer(cols = c(fdbk, m_hist, PE, SubjPE, stress_level),
               names_to = "Screen",
               values_to = "Response")

# This will keep only the rows where 'Response' is not NA
long_df_students <- long_df_students[!is.na(long_df_students$Response), ]

# Selecting relevant columns from the original df_students
df_students <- df_students %>%
  dplyr::select(-c(fdbk, m_hist, PE, SubjPE, stress_level))

# Binding the rows together
df_students <- bind_rows(df_students, long_df_students)

df_students$Response <- as.numeric(df_students$Response)

# looking at the relationship between PE and SubjPE
df_students_PE <- subset(df_students, Screen == "PE")
names(df_students_PE)[names(df_students_PE) == "Screen"] <- "Screen_PE"
names(df_students_PE)[names(df_students_PE) == "Response"] <- "Response_PE"

df_students_SubjPE <- subset(df_students, Screen == "SubjPE")
names(df_students_SubjPE)[names(df_students_SubjPE) == "Screen"] <- "Screen_SubjPE"
names(df_students_SubjPE)[names(df_students_SubjPE) == "Response"] <- "Response_SubjPE"

df_students_fdbk <- subset(df_students, Screen == "fdbk")
names(df_students_fdbk)[names(df_students_fdbk) == "Screen"] <- "Screen_fdbk"
names(df_students_fdbk)[names(df_students_fdbk) == "Response"] <- "Response_fdbk"

df_students_hist <- subset(df_students, Screen == "m_hist")
names(df_students_hist)[names(df_students_hist) == "Screen"] <- "Screen_m_hist"
names(df_students_hist)[names(df_students_hist) == "Response"] <- "Response_m_hist"

df_students_stress_level <- subset(df_students, Screen == "Screen 12")
names(df_students_stress_level)[names(df_students_stress_level) == "Screen"] <- "Screen_stress_level"
names(df_students_stress_level)[names(df_students_stress_level) == "Response"] <- "Response_stress_level"

df_students_predic <- subset(df_students, Screen == "Prediction ")
names(df_students_predic)[names(df_students_predic) == "Screen"] <- "Screen_pred"
names(df_students_predic)[names(df_students_predic) == "Response"] <- "Response_pred"

list_of_dfs_21 <- list(df_students_PE, df_students_fdbk, df_students_hist, df_students_predic, df_students_SubjPE)

# Using reduce with inner_join
merged_df_students <- reduce(list_of_dfs_21, inner_join, by = c("Random_ID_new", "Trial.Number"))

df_students_Anxious <- subset(df_students, Screen == "Anxious")
names(df_students_Anxious)[names(df_students_Anxious) == "Screen"] <- "Screen_Ax"
names(df_students_Anxious)[names(df_students_Anxious) == "Response"] <- "Response_Ax"

df_students_Happy <- subset(df_students, Screen == "Happy")
# df_students_fdbk <- subset(df_students, Screen == "Prediction")
names(df_students_Happy)[names(df_students_Happy) == "Screen"] <- "Screen_H"
names(df_students_Happy)[names(df_students_Happy) == "Response"] <- "Response_H"

final_df_students <- merged_df_students %>%
  inner_join(df_students_Anxious, by = c("Random_ID_new", "Trial.Number")) %>%
  inner_join(df_students_Happy, by = c("Random_ID_new", "Trial.Number"))


# subset only the columns we are interested in
final_df_students <- final_df_students[c("Trial.Number", "Random_ID_new", "Response_H", "Response_Ax", "Response_fdbk", "Response_SubjPE", "Response_PE", "Response_m_hist", "Response_pred")]

final_df_students <- final_df_students %>%
  left_join(df_14_18_spin %>% dplyr::select(Random_ID_new, mini_SPIN_total), by = "Random_ID_new")

final_df_students <- 
  left_join(final_df_students,df_students_rcads_clean , by = "Random_ID_new")

final_df_students <- final_df_students %>%
  left_join(df_students_stress_level %>% dplyr::select(Random_ID_new, Response_stress_level), by = "Random_ID_new")

# final_df_students <- final_df_students %>%
#   left_join(CESD_data_clean %>% dplyr::select(Random_ID_new, CESD_score, Depression_Threshold, Depression_status), by = "Random_ID_new")

final_df_students$Social_Anxiety <- ifelse(final_df_students$mini_SPIN_total >= 6, "high", "low")

# excluding people with poor data quality after the video inspection (not doing the task, or laughing with friends etc):


# excluding people with problematic performance according to Jess's video check
final_df_students <- final_df_students[!(final_df_students$Random_ID_new %in% c("SUPSCO12782", "SUPSCO24387", "SUPSCO46030", "SUPSCO12088","SUPSCO70018")), ]

final_df_students %>%
  group_by(Random_ID_new) %>%
  summarise(Trial.Number = n())

write.csv(final_df_students, "surprise_study_14_18_variables.csv", row.names = FALSE)
