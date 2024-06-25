library(meta)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)
library(parameters)
library(lmerTest)

# add CESD for pilots 
all_pilots_without_CESD_p6_18 <- read.csv("/Users/marjan/Desktop/aim_lab_1/all_pilots_with_mini_spin.csv")
# remove pilots 12 and above
all_pilots_without_CESD_p6_12 <- all_pilots_without_CESD_p6_18 %>% 
  dplyr::filter(!pilot_nr %in% c(13, 14, 15, 16, 17, 18))

all_pilots_without_CESD_p6_12 <- all_pilots_without_CESD_p6_12 %>% 
  dplyr::filter(Random_ID != "SUPPRF70813")


demo_p6_p11 <- read.csv("/Users/marjan/Desktop/aim_lab_1/Pilots_6_to_11_with_mini_spin_BDD_demographic_medication_data_Emma.csv")

# now concatenate this data with mean/se for age, % women, mini_SPIN_total and CESD_score
# let's merge demo_p6_p11 with age and sex for pilots 12-22 (excluding pilot 17)
demo_p12 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_anxQ/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_anxQ_v2/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_anxQ_v2_demographics.csv")
demo_p13 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_vid_big_pos_neg_PE/SUP_PRF_pilot_vid_big_pos_neg_PE_v5/SUP_PRF_pilot_vid_big_pos_neg_PE_v5_demographics.csv")
demo_p14 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_vid_big_PE_narr1_avatars/SUP_PRF_pilot_vid_big_PE_narr1_avatars_v9/SUP_PRF_pilot_vid_big_PE_narr1_avatars_v9_demographics.csv")
demo_p15 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_vid_big_PE_narr2_oldjudges/SUP_PRF_pilot_vid_big_PE_narr2_oldjudges_v4/SUP_PRF_pilot_vid_big_PE_narr2_oldjudges_v4_demographics.csv")
demo_p16 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_vid_big_PE_narr2_early_pred/SUP_PRF_pilot_vid_big_PE_narr2_early_pred_v2/SUP_PRF_pilot_vid_big_PE_narr2_early_pred_v2_demographics.csv")
demo_p18 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_p18_vid_bigPE_nar2_earlypred_chk_cbal/SUP_PRF_p18_vid_bigPE_nar2_earlypred_chk_cbal_v4/SUP_PRF_p18_vid_bigPE_nar2_earlypred_chk_cbal_v4_demographics.csv")
demo_p19 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_p19_vid_bigPE_nar2_new_qns_hist_early_4jud/SUP_PRF_p19_vid_bigPE_nar2_new_qns_hist_early_4jud_v5/SUP_PRF_p19_vid_bigPE_nar2_new_qns_hist_early_4jud_v5_demographics.csv")
demo_p20 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_p20_vid_bigPE_2pred_afterfdbk_nohist_4jud/SUP_PRF_p20_vid_bigPE_2pred_afterfdbk_nohist_4jud_v4/SUP_PRF_p20_vid_bigPE_2pred_afterfdbk_nohist_4jud_v4_demographics.csv")
demo_p21 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_p21_vid_bigPE_nohist_newjud/SUP_PRF_p21_vid_bigPE_nohist_newjud_v3/SUP_PRF_p21_vid_bigPE_nohist_newjud_v3_demographics.csv")
demo_p22 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_p22_vid_bigPE_nohist_newjud_2anxQ/SUP_PRF_p22_vid_bigPE_nohist_newjud_2anxQ_v1/SUP_PRF_p22_vid_bigPE_nohist_newjud_2anxQ_v1_demographics.csv")

demo_p12$pilot_nr <- 12
demo_p13$pilot_nr <- 13
demo_p14$pilot_nr <- 14
demo_p15$pilot_nr <- 15
demo_p16$pilot_nr <- 16
demo_p18$pilot_nr <- 18
demo_p19$pilot_nr <- 19
demo_p20$pilot_nr <- 20
demo_p21$pilot_nr <- 21
demo_p22$pilot_nr <- 22


demo_all <- rbind(demo_p13, demo_p14, demo_p16, demo_p18, demo_p19, demo_p20, demo_p21, demo_p22)

# merging this data to the rest
demo_all <- demo_all[, c("Random_ID", "Age", "Sex", "pilot_nr")]
# pilots 12 and 15 have different column names for some reason, so extracting these values separately for them and then merge them with the rest
demo_p12 <- subset(demo_p12, select = c("Random_ID", "Age", "Sex", "pilot_nr"))
demo_p15 <- subset(demo_p15, select = c("Random_ID", "Age", "Sex", "pilot_nr"))
demo_p6_p11 <- subset(demo_p6_p11, select = c("Random_ID", "Age", "Sex", "pilot_nr"))

# format of pilots 6 to pilot 11 are different from the rest, let's name them the same way before merging them
demo_p6_p11 <- demo_p6_p11 %>%
  mutate(pilot_nr = as.numeric(gsub("[^0-9]", "", pilot_nr)))


demo_all <- rbind(demo_p6_p11, demo_all, demo_p12, demo_p15)
# some people revoked their consent, so let's remove them
demo_all <- subset(demo_all, Age != "CONSENT_REVOKED")

demo_all <- demo_all %>%
  mutate(Age = as.numeric(Age))

summary_stats_demo <- demo_all %>%
  group_by(pilot_nr) %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    se_age = sd(Age, na.rm = TRUE) / sqrt(n()),
    percentage_women = round(sum(Sex == "Female") / n() * 100, 2),
    proportion_women = sum(Sex == "Female") / n(),
    se_percentage_women = round(sqrt(proportion_women * (1 - proportion_women) / n()) * 100, 2)
  )

# Concatenate all pilots (6-22) for age, sex, cesd, mini_SPIN
df6_cesd <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_nrnd_typ/SUP_PRF_pilot_fdbk_nrnd_typ_v3/SUP_PRF_pilot_fdbk_nrnd_typ_v3_ces_d.csv")  
df7_cesd <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_typ/SUP_PRF_pilot_fdbk_bignrnd_typ_v3/SUP_PRF_pilot_fdbk_bignrnd_typ_v3_ces_d.csv")  
df8_cesd <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_biggestnrnd_typ/SUP_PRF_pilot_fdbk_biggestnrnd_typ_v3/SUP_PRF_pilot_fdbk_biggestnrnd_typ_v3_ces_d.csv")
df9_cesd <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_typ_YPAG/SUP_PRF_pilot_fdbk_bignrnd_typ_YPAG_v6/SUP_PRF_pilot_fdbk_bignrnd_typ_YPAG_v6_ces_d.csv")
df10_cesd <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_v5/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_v5_ces_d.csv")
df11_cesd <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_v7/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_v7_ces_d.csv")
df12_cesd <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_anxQ/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_anxQ_v2/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_anxQ_v2_ces_d.csv")


# "Multiple.Choice.Grid.object.3.20...I.could.not.get..going...Quantised" 
# "Multiple.Choice.Grid.20...I.could.not.get..going...Quantised"

# need to rename column names for pilot 7, as it is missing "object.3" for some reason
names(df7_cesd) <- gsub("Grid", "Grid.object.3", names(df7_cesd))

cesdp6_12 <- rbind(df6_cesd, df7_cesd, df8_cesd, df9_cesd, df10_cesd, df11_cesd, df12_cesd)


# scoring CESD for the pilots that are missing it
# extract CES-D scores
CESD_data_clean <- cesdp6_12[, c(32, 34, 36, 38, 40, 42, 44, 46,48,50,
                                 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 71 )]
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
CESD_data_clean$Depression_status <- "Not_Depressed"

#Assign "1" if they meet Depression threshold 
for (i in 1:nrow(CESD_data_clean)) {
  if (CESD_data_clean$CESD_score[i] >= 16) {
    CESD_data_clean$Depression_Threshold[i] <- 1
    CESD_data_clean$Depression_status[i] <- "Depressed"
  }
}

all_pilots_without_CESD_p6_12 <- all_pilots_without_CESD_p6_12 %>%
  left_join(CESD_data_clean %>% select(Random_ID, CESD_score), by = "Random_ID")


# now concatenate this data with mean/se for age, % women, mini_SPIN_total and CESD_score
# let's merge demo_p6_p11 with age and sex for pilots 12-22 (excluding pilot 17)
demo_p12 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_anxQ/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_anxQ_v2/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_anxQ_v2_demographics.csv")
demo_p13 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_vid_big_pos_neg_PE/SUP_PRF_pilot_vid_big_pos_neg_PE_v5/SUP_PRF_pilot_vid_big_pos_neg_PE_v5_demographics.csv")
demo_p14 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_vid_big_PE_narr1_avatars/SUP_PRF_pilot_vid_big_PE_narr1_avatars_v9/SUP_PRF_pilot_vid_big_PE_narr1_avatars_v9_demographics.csv")
demo_p15 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_vid_big_PE_narr2_oldjudges/SUP_PRF_pilot_vid_big_PE_narr2_oldjudges_v4/SUP_PRF_pilot_vid_big_PE_narr2_oldjudges_v4_demographics.csv")
demo_p16 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_vid_big_PE_narr2_early_pred/SUP_PRF_pilot_vid_big_PE_narr2_early_pred_v2/SUP_PRF_pilot_vid_big_PE_narr2_early_pred_v2_demographics.csv")
demo_p18 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_p18_vid_bigPE_nar2_earlypred_chk_cbal/SUP_PRF_p18_vid_bigPE_nar2_earlypred_chk_cbal_v4/SUP_PRF_p18_vid_bigPE_nar2_earlypred_chk_cbal_v4_demographics.csv")
demo_p19 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_p19_vid_bigPE_nar2_new_qns_hist_early_4jud/SUP_PRF_p19_vid_bigPE_nar2_new_qns_hist_early_4jud_v5/SUP_PRF_p19_vid_bigPE_nar2_new_qns_hist_early_4jud_v5_demographics.csv")
demo_p20 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_p20_vid_bigPE_2pred_afterfdbk_nohist_4jud/SUP_PRF_p20_vid_bigPE_2pred_afterfdbk_nohist_4jud_v4/SUP_PRF_p20_vid_bigPE_2pred_afterfdbk_nohist_4jud_v4_demographics.csv")
demo_p21 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_p21_vid_bigPE_nohist_newjud/SUP_PRF_p21_vid_bigPE_nohist_newjud_v3/SUP_PRF_p21_vid_bigPE_nohist_newjud_v3_demographics.csv")
demo_p22 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_p22_vid_bigPE_nohist_newjud_2anxQ/SUP_PRF_p22_vid_bigPE_nohist_newjud_2anxQ_v1/SUP_PRF_p22_vid_bigPE_nohist_newjud_2anxQ_v1_demographics.csv")

demo_p12$pilot_nr <- 12
demo_p13$pilot_nr <- 13
demo_p14$pilot_nr <- 14
demo_p15$pilot_nr <- 15
demo_p16$pilot_nr <- 16
demo_p18$pilot_nr <- 18
demo_p19$pilot_nr <- 19
demo_p20$pilot_nr <- 20
demo_p21$pilot_nr <- 21
demo_p22$pilot_nr <- 22


demo_all <- rbind(demo_p13, demo_p14, demo_p16, demo_p18, demo_p19, demo_p20, demo_p21, demo_p22)

# merging this data to the rest
demo_all <- demo_all[, c("Random_ID", "Age", "Sex", "pilot_nr")]
# pilots 12 and 15 have different column names for some reason, so extracting these values separately for them and then merge them with the rest
demo_p12 <- subset(demo_p12, select = c("Random_ID", "Age", "Sex", "pilot_nr"))
demo_p15 <- subset(demo_p15, select = c("Random_ID", "Age", "Sex", "pilot_nr"))
demo_p6_p11 <- subset(demo_p6_p11, select = c("Random_ID", "Age", "Sex", "pilot_nr"))

# format of pilots 6 to pilot 11 are different from the rest, let's name them the same way before merging them
demo_p6_p11 <- demo_p6_p11 %>%
  mutate(pilot_nr = as.numeric(gsub("[^0-9]", "", pilot_nr)))


demo_all <- rbind(demo_p6_p11, demo_all, demo_p12, demo_p15)
# some people revoked their consent, so let's remove them
demo_all <- subset(demo_all, Age != "CONSENT_REVOKED")

demo_all <- demo_all %>%
  mutate(Age = as.numeric(Age))

summary_stats_demo <- demo_all %>%
  group_by(pilot_nr) %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    se_age = sd(Age, na.rm = TRUE) / sqrt(n()),
    percentage_women = round(sum(Sex == "Female") / n() * 100, 2),
    proportion_women = sum(Sex == "Female") / n(),
    se_percentage_women = round(sqrt(proportion_women * (1 - proportion_women) / n()) * 100, 2)
  )


# now let's concatenate pilots 13-22 (although thet will have different sizes since different design)
df13 <- read.csv("/Users/marjan/Desktop/aim_lab_1/pilot_13_variables.csv") 
df14 <- read.csv("/Users/marjan/Desktop/aim_lab_1/pilot_14_variables.csv")  
df15 <- read.csv("/Users/marjan/Desktop/aim_lab_1/pilot_15_variables.csv")  
df16 <- read.csv("/Users/marjan/Desktop/aim_lab_1/pilot_16_variables.csv")  
df17 <- read.csv("/Users/marjan/Desktop/aim_lab_1/pilot_17_variables.csv")  
df18 <- read.csv("/Users/marjan/Desktop/aim_lab_1/pilot_18_variables.csv")  
df19 <- read.csv("/Users/marjan/Desktop/aim_lab_1/pilot_19_variables.csv")  
df20 <- read.csv("/Users/marjan/Desktop/aim_lab_1/pilot_20_variables.csv")  
df21 <- read.csv("/Users/marjan/Desktop/aim_lab_1/pilot_21_variables.csv")  
df22 <- read.csv("/Users/marjan/Desktop/aim_lab_1/pilot_22_variables.csv")  

df13$pilot_nr <- 13
df14$pilot_nr <- 14
df15$pilot_nr <- 15
df16$pilot_nr <- 16
df17$pilot_nr <- 17
df18$pilot_nr <- 18
df19$pilot_nr <- 19
df20$pilot_nr <- 20
df21$pilot_nr <- 21
df22$pilot_nr <- 22


df_list <- list(df13, df14, df15, df16, df17, df18, df19, df20, df21, df22)     

columns_to_keep <- c("Trial.Number", "Random_ID", "Response_H", "Response_Ax", 
                     "Response_fdbk", "Response_SubjPE", "Response_PE", 
                     "Response_pred", "mini_SPIN_total", "CESD_score", "pilot_nr")

# a for loop to subset each data frame
for (i in 1:length(df_list)) {
  df_list[[i]] <- df_list[[i]][, columns_to_keep, drop = FALSE]
}

combined_df13_22 <- bind_rows(df_list)

# let's change column names to match the earlier pilots
combined_df13_22 <- combined_df13_22 %>%
  rename(
    Mood = Response_H,
    Anxiety = Response_Ax,
    fdbk = Response_fdbk,
    SubjPE = Response_SubjPE,
    PE = Response_PE,
    Prediction = Response_pred
  )

all_pilots_without_CESD_p6_12 <- all_pilots_without_CESD_p6_12[, c("Trial.Number", "Random_ID", "Mood", "Anxiety", 
                     "fdbk", "SubjPE", "PE", "Prediction", "mini_SPIN_total", "CESD_score", "pilot_nr")]

desired_order <- c("Trial.Number", "Random_ID", "Mood", "Anxiety", "fdbk", "SubjPE", 
                   "PE", "Prediction", "mini_SPIN_total", "CESD_score", "pilot_nr")

# Rearrange the columns
all_pilots_without_CESD_p6_12 <- all_pilots_without_CESD_p6_12 %>%
  select(all_of(desired_order))

# now that all dataframes have the same column names and order, let's merge them
df_surprises <- rbind(all_pilots_without_CESD_p6_12, combined_df13_22)


# now let's write a function that runs the LMEs per pilot, and saves the beta's and SE per pilot
# we also would like to save the mean and SE for: mini_SPIN_total, CESD_score, age, sex (% females)
# also keeping the column pilot_nr

unique_pilots <- unique(df_surprises$pilot_nr)

# LME for mood
# Initialize lists to store models and standardized parameters
mix_models_per_pilot <- list()
std_param_mix_models_per_pilot <- list()
results <- data.frame()

for(i in seq_along(unique_pilots)) {
  pilot <- unique_pilots[i]
  
  # Fit the mixed model for each pilot's data
  model <- lmerTest::lmer(
    Mood ~ SubjPE + mini_SPIN_total + (SubjPE | Random_ID), 
    data = df_surprises %>% dplyr::filter(pilot_nr == pilot), 
    REML = FALSE, 
    control = lmerControl(optimizer = "bobyqa")
  )
  
  # Store the model in the list
  mix_models_per_pilot[[pilot]] <- model
  
  # Standardize parameters of the fitted model
  std_params <- parameters::standardise_parameters(model)
  std_param_mix_models_per_pilot[[pilot]] <- std_params
  
  # Extract beta coefficients and standard errors
  beta_coeffs <- summary(model)$coefficients
  beta_se <- summary(model)$coefficients[, "Std. Error"]
  
  # Calculate the standardized beta for SubjPE
  standard_beta_SubjPE <- round(std_params$Std_Coefficient[std_params$Parameter == "SubjPE"], 2)
  
  # Create a data frame with results for the current pilot
  pilot_results <- data.frame(
    pilot_nr = pilot,
    term = rownames(beta_coeffs),
    estimate = beta_coeffs[, "Estimate"],
    std_error = beta_se,
    beta = NA
  )
  
  # Assign the calculated beta to the row corresponding to SubjPE
  pilot_results$beta[pilot_results$term == "SubjPE"] <- standard_beta_SubjPE
  
  # Append the results to the overall results data frame
  results <- bind_rows(results, pilot_results)
}



betas_surprises_mood <- results %>% 
  dplyr::filter(term == "SubjPE")

betas_surprises_mood <- betas_surprises_mood %>% 
  dplyr::filter(pilot_nr != 17)

betas_surprises_mood <- betas_surprises_mood %>%
  select(pilot_nr, std_error, beta)

df_surprises <- df_surprises %>% 
  dplyr::filter(pilot_nr != 17)


# let's do the same thing to extract CESD and mini_spin scores 
summary_stats_SPIN_CESD <- df_surprises %>%
  group_by(pilot_nr) %>%
  summarise(
    mean_mini_SPIN = mean(mini_SPIN_total, na.rm = TRUE),
    se_mini_SPIN = sd(mini_SPIN_total, na.rm = TRUE) / sqrt(n()),
    mean_CESD = mean(CESD_score, na.rm = TRUE),
    se_CESD = sd(CESD_score, na.rm = TRUE) / sqrt(n()),
  )


# Now let's merge the betas, demographic data, and clinical data before doing the meta-regression
meta_reg_df_mood <- summary_stats_SPIN_CESD %>%
  left_join(summary_stats_demo, by = "pilot_nr") %>%
  left_join(betas_surprises_mood, by = "pilot_nr")


# add a column for video vs text
# add a column for size of PEs
# add a column for high vs low SA
df_surprises_Trial1 <- subset(df_surprises, Trial.Number == 1)

meta_reg_df_mood <- meta_reg_df_mood %>%
  mutate(modality = case_when(
    pilot_nr %in% c(6, 7, 8, 9) ~ "text",
    pilot_nr %in% 10:22 ~ "video + audio",
    TRUE ~ NA_character_  
  ))

meta_reg_df_mood <- meta_reg_df_mood %>%
  mutate(PE_size = case_when(
    pilot_nr == 6 ~ "normal PE",
    pilot_nr == 8 ~ "biggest pos PE",
    pilot_nr == 13 ~ "biggest pos & neg PE",
    pilot_nr %in% c(7, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22) ~ "bigger pos PE",
    TRUE ~ NA_character_  
  ))

# Calculate percentages of high SA
high_SA_percentage <- df_surprises_Trial1 %>%
  group_by(pilot_nr) %>%
  summarise(
    total = n(),  # Total count of trials per pilot_nr
    high_SA_count = sum(mini_SPIN_total > 6),  # Count of mini_SPIN_total > 6
    high_SA_perc = round((high_SA_count / total) * 100,2),  # Calculate percentage
    .groups = 'drop'  # Remove grouping
  )

meta_reg_df_mood <- meta_reg_df_mood %>%
  left_join(high_SA_percentage %>% select(pilot_nr, high_SA_perc), by = "pilot_nr")


# saving the data in case we did not want to run all the code above again 
write.csv(meta_reg_df_mood, file = "meta_reg_df_mood.csv")

# Meta-analysis and Meta-regression
library(meta)
library(metafor)
library(tidyverse)
library(dmetar)
# if (!require("remotes")) {
#   install.packages("remotes")
# }
# remotes::install_github("MathiasHarrer/dmetar")

glimpse(meta_reg_df_mood)

# names(meta_reg_df_mood) <- gsub("beta", "TE", names(meta_reg_df))
# names(meta_reg_df_mood) <- gsub("std_error", "seTE", names(meta_reg_df))

# Meta-analysis with just the betas
result_mood <- metagen(
  TE = beta,
  seTE = std_error,
  data = meta_reg_df_mood,
  studlab = as.character(meta_reg_df_mood$pilot_nr),
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "REML",
  hakn = TRUE,
)


mreg_1 <- metareg(result_mood, ~ pilot_nr)
print(mreg_1)

mreg_2 <- metareg(result_mood, ~ pilot_nr + mean_age )
print(mreg_2)

mreg_3 <- metareg(result_mood, ~ pilot_nr + mean_age + mean_CESD + mean_mini_SPIN + percentage_women)
print(mreg_3)

mreg_4 <- metareg(result_mood, ~ pilot_nr * mean_age + mean_CESD + mean_mini_SPIN + percentage_women)
print(mreg_4)

mreg_5 <- metareg(result_mood, ~  mean_age + mean_CESD + mean_mini_SPIN + percentage_women)
print(mreg_5)

mreg_6 <- metareg(result_mood, ~  mean_age + mean_CESD + mean_mini_SPIN * percentage_women)
print(mreg_6)


# Making forest plots for mood

forest(result_mood, 
       sortvar = TE,               # Sort studies by the treatment effect
       comb.fixed = FALSE,         # Assuming a random-effects model is more appropriate
       comb.random = TRUE,         # Display the combined random effects estimate
       xlim = c(-0.5, 0.5),            # Adjust these limits based on your effect size range
       xlab = "Beta: mood ~ PE", # Label for the x-axis
       alim = c(-0.5, 0.5),            # Axis limits, adjust as necessary
       leftcols = c("studlab", "TE", "ci"), # Columns to show on the left side of the plot
       rightcols = c("w.fixed", "w.random")) # Show the weights under fixed and random effects models


# let's re-order them to be from earliest to latest pilot
result_mood$data <- result_mood$data %>%
  mutate(pilot_nr = as.numeric(as.character(pilot_nr))) %>%  # Ensure numeric IDs
  filter(pilot_nr >= 6 & pilot_nr <= 22) %>%  # Filter studies between 6 to 22
  arrange(pilot_nr)

forest(result_mood,
       sortvar = NULL,  # No additional sorting, data is pre-sorted
       comb.fixed = TRUE,
       comb.random = TRUE,
       xlim = c(-0.5, 0.5),            # Adjust these limits based on your effect size range
       xlab = "Beta: mood ~ PE", # Label for the x-axis
       alim = c(-0.5, 0.5),       
       leftcols = c("studlab", "TE", "ci"),
       rightcols = c("w.fixed", "w.random"),
       order = result_mood$data$pilot_nr)  

# adding percentage women to see how we can add categories
meta_reg_df_mood$percentage_women_s <- ifelse(meta_reg_df_mood$percentage_women > 50, "Above 50%", "50% and below")
meta_reg_df_mood$high_SA_perc_s <- ifelse(meta_reg_df_mood$high_SA_perc > 40, "Above 40%", "40% and below")

# make the plot for SA proportion
result_mood_SA <- metagen(
  TE = beta,
  seTE = std_error,
  data = meta_reg_df_mood,
  byvar = high_SA_perc_s, 
  studlab = as.character(meta_reg_df_mood$pilot_nr),
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "REML",
  hakn = TRUE,
  subgroup.name = "Proportion of social anxiety"  # Descriptive name for the subgroups
)

forest(result_mood_SA,
       sortvar = NULL,  # No additional sorting, data is pre-sorted
       comb.fixed = TRUE,
       comb.random = TRUE,
       xlim = c(-0.5, 0.5),            # Adjust these limits based on your effect size range
       xlab = "Beta: mood ~ PE", # Label for the x-axis
       alim = c(-0.5, 0.5),       
       leftcols = c("studlab", "TE", "ci"),
       rightcols = c("w.fixed", "w.random"),
       order = result_mood$data$pilot_nr)  


# make the plot for size of PE
result_mood_PE_size <- metagen(
  TE = beta,
  seTE = std_error,
  data = meta_reg_df_mood,
  byvar = PE_size, 
  studlab = as.character(meta_reg_df_mood$pilot_nr),
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "REML",
  hakn = TRUE,
  subgroup.name = "Size of PE"  # Descriptive name for the subgroups
)

forest(result_mood_PE_size,
       sortvar = NULL,  # No additional sorting, data is pre-sorted
       comb.fixed = TRUE,
       comb.random = TRUE,
       xlim = c(-0.5, 0.5),            # Adjust these limits based on your effect size range
       xlab = "Beta: mood ~ PE", # Label for the x-axis
       alim = c(-0.5, 0.5),       
       leftcols = c("studlab", "TE", "ci"),
       rightcols = c("w.fixed", "w.random"),
       order = result_mood$data$pilot_nr)  

# make the plot for modality
result_mood_modality <- metagen(
  TE = beta,
  seTE = std_error,
  data = meta_reg_df_mood,
  byvar = modality, 
  studlab = as.character(meta_reg_df_mood$pilot_nr),
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "REML",
  hakn = TRUE,
  subgroup.name = "modality: text vs video"  # Descriptive name for the subgroups
)

forest(result_mood_modality,
       sortvar = NULL,  # No additional sorting, data is pre-sorted
       comb.fixed = TRUE,
       comb.random = TRUE,
       xlim = c(-0.5, 0.5),            # Adjust these limits based on your effect size range
       xlab = "Beta: mood ~ PE", # Label for the x-axis
       alim = c(-0.5, 0.5),       
       leftcols = c("studlab", "TE", "ci"),
       rightcols = c("w.fixed", "w.random"),
       order = result_mood$data$pilot_nr)  



# LME for anxiety
# Initialize lists to store models and standardized parameters
unique_pilots <- unique(df_surprises$pilot_nr)
# excluding pilot 12 because it asks about how relaxed someone is instead of anxiety, and pilot 17 which had 
# a different design (chocking and event related, finding too different)
unique_pilots <- unique_pilots[unique_pilots != 12]
unique_pilots <- unique_pilots[unique_pilots != 17]

mix_models_per_pilot <- list()
std_param_mix_models_per_pilot <- list()
results <- data.frame()

for(i in seq_along(unique_pilots)) {
  pilot <- unique_pilots[i]
  
  # Fit the mixed model for each pilot's data
  model <- lmerTest::lmer(
    Anxiety ~ SubjPE + mini_SPIN_total + (1 | Random_ID), 
    data = df_surprises %>% dplyr::filter(pilot_nr == pilot), 
    REML = FALSE, 
    control = lmerControl(optimizer = "bobyqa")
  )
  
  # Store the model in the list
  mix_models_per_pilot[[pilot]] <- model
  
  # Standardize parameters of the fitted model
  std_params <- parameters::standardise_parameters(model)
  std_param_mix_models_per_pilot[[pilot]] <- std_params
  
  # Extract beta coefficients and standard errors
  beta_coeffs <- summary(model)$coefficients
  beta_se <- summary(model)$coefficients[, "Std. Error"]
  
  # Calculate the standardized beta for SubjPE
  standard_beta_SubjPE <- round(std_params$Std_Coefficient[std_params$Parameter == "SubjPE"], 2)
  
  # Create a data frame with results for the current pilot
  pilot_results <- data.frame(
    pilot_nr = pilot,
    term = rownames(beta_coeffs),
    estimate = beta_coeffs[, "Estimate"],
    std_error = beta_se,
    beta = NA
  )
  
  # Assign the calculated beta to the row corresponding to SubjPE
  pilot_results$beta[pilot_results$term == "SubjPE"] <- standard_beta_SubjPE
  
  # Append the results to the overall results data frame
  results <- bind_rows(results, pilot_results)
}


betas_surprises_anxiety <- results %>% 
  dplyr::filter(term == "SubjPE")

betas_surprises_anxiety <- betas_surprises_anxiety %>%
  select(pilot_nr, std_error, beta)


# Now let's merge the betas, demographic data, and clinical data before doing the meta-regression
meta_reg_df_anxiety <- summary_stats_SPIN_CESD %>%
  left_join(summary_stats_demo, by = "pilot_nr") %>%
  left_join(betas_surprises_anxiety, by = "pilot_nr")


# add a column for video vs text
# add a column for size of PEs
# add a column for high vs low SA
df_surprises_Trial1 <- subset(df_surprises, Trial.Number == 1)

meta_reg_df_anxiety <- meta_reg_df_anxiety %>%
  mutate(modality = case_when(
    pilot_nr %in% c(6, 7, 8, 9) ~ "text",
    pilot_nr %in% 10:22 ~ "video + audio",
    TRUE ~ NA_character_  
  ))

meta_reg_df_anxiety <- meta_reg_df_anxiety %>%
  mutate(PE_size = case_when(
    pilot_nr == 6 ~ "normal PE",
    pilot_nr == 8 ~ "biggest pos PE",
    pilot_nr == 13 ~ "biggest pos & neg PE",
    pilot_nr %in% c(7, 9, 10, 11, 14, 15, 16,18, 19, 20, 21, 22) ~ "bigger pos PE",
    TRUE ~ NA_character_  
  ))

# Calculate percentages of high SA
high_SA_percentage <- df_surprises_Trial1 %>%
  group_by(pilot_nr) %>%
  summarise(
    total = n(),  # Total count of trials per pilot_nr
    high_SA_count = sum(mini_SPIN_total > 6),  # Count of mini_SPIN_total > 6
    high_SA_perc = round((high_SA_count / total) * 100,2),  # Calculate percentage
    .groups = 'drop'  # Remove grouping
  )

meta_reg_df_anxiety <- meta_reg_df_anxiety %>%
  left_join(high_SA_percentage %>% select(pilot_nr, high_SA_perc), by = "pilot_nr")


# saving the data in case we did not want to run all the code above again 
write.csv(meta_reg_df_anxiety, file = "meta_reg_df_anxiety.csv")

# meta_reg_df_anxiety <- read.csv("/Users/marjan/Desktop/aim_lab_1/meta_reg_df_anxiety.csv")

meta_reg_df_anxiety <- meta_reg_df_anxiety %>%
  distinct(pilot_nr, .keep_all = TRUE)

meta_reg_df_anxiety <- subset(meta_reg_df_anxiety, pilot_nr != 12)

# meta_reg_df_anxiety <- meta_reg_df_anxiety %>%
#   mutate(beta = if_else(pilot_nr == 12, -beta, beta))

# Meta-analysis with just the betas
result_anxiety <- metagen(
  TE = beta,
  seTE = std_error,
  data = meta_reg_df_anxiety,
  studlab = as.character(meta_reg_df_anxiety$pilot_nr),
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "REML",
  hakn = TRUE,
)



mreg_1 <- metareg(result_anxiety, ~ pilot_nr)
print(mreg_1)

mreg_2 <- metareg(result_anxiety, ~ pilot_nr + mean_age )
print(mreg_2)

mreg_3 <- metareg(result_anxiety, ~ pilot_nr + mean_age + mean_CESD + mean_mini_SPIN + percentage_women)
print(mreg_3)

mreg_4 <- metareg(result_anxiety, ~ pilot_nr * mean_age + mean_CESD + mean_mini_SPIN + percentage_women)
print(mreg_4)

mreg_5 <- metareg(result_anxiety, ~  mean_age + mean_CESD + mean_mini_SPIN + percentage_women)
print(mreg_5)

mreg_6 <- metareg(result_anxiety, ~  mean_age + mean_CESD + mean_mini_SPIN * percentage_women)
print(mreg_6)



# Making forest plots for mood

forest(result_anxiety, 
       sortvar = TE,               # Sort studies by the treatment effect
       comb.fixed = FALSE,         # Assuming a random-effects model is more appropriate
       comb.random = TRUE,         # Display the combined random effects estimate
       xlim = c(-0.5, 0.5),            # Adjust these limits based on your effect size range
       xlab = "Beta: anxiety ~ PE", # Label for the x-axis
       alim = c(-0.5, 0.5),            # Axis limits, adjust as necessary
       leftcols = c("studlab", "TE", "ci"), # Columns to show on the left side of the plot
       rightcols = c("w.fixed", "w.random")) # Show the weights under fixed and random effects models


# let's re-order them to be from earliest to latest pilot
result_anxiety$data <- result_anxiety$data %>%
  mutate(pilot_nr = as.numeric(as.character(pilot_nr))) %>%  # Ensure numeric IDs
  filter(pilot_nr >= 6 & pilot_nr <= 22) %>%  # Filter studies between 6 to 22
  arrange(pilot_nr)

forest(result_anxiety,
       sortvar = NULL,  # No additional sorting, data is pre-sorted
       comb.fixed = TRUE,
       comb.random = TRUE,
       xlim = c(-0.5, 0.5),            # Adjust these limits based on your effect size range
       xlab = "Beta: anxiety ~ PE", # Label for the x-axis
       alim = c(-0.5, 0.5),       
       leftcols = c("studlab", "TE", "ci"),
       rightcols = c("w.fixed", "w.random"),
       order = result_anxiety$data$pilot_nr)  

# adding percentage women to see how we can add categories
meta_reg_df_anxiety$percentage_women_s <- ifelse(meta_reg_df_anxiety$percentage_women > 50, "Above 50%", "50% and below")
meta_reg_df_anxiety$high_SA_perc_s <- ifelse(meta_reg_df_anxiety$high_SA_perc > 40, "Above 40%", "40% and below")

# make the plot for SA proportion
result_anxiety_SA <- metagen(
  TE = beta,
  seTE = std_error,
  data = meta_reg_df_anxiety,
  byvar = high_SA_perc_s, 
  studlab = as.character(meta_reg_df_anxiety$pilot_nr),
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "REML",
  hakn = TRUE,
  subgroup.name = "Proportion of social anxiety"  # Descriptive name for the subgroups
)

forest(result_anxiety_SA,
       sortvar = NULL,  # No additional sorting, data is pre-sorted
       comb.fixed = TRUE,
       comb.random = TRUE,
       xlim = c(-0.5, 0.5),            # Adjust these limits based on your effect size range
       xlab = "Beta: anxiety ~ PE", # Label for the x-axis
       alim = c(-0.5, 0.5),       
       leftcols = c("studlab", "TE", "ci"),
       rightcols = c("w.fixed", "w.random"),
       order = result_anxiety$data$pilot_nr)  


# make the plot for size of PE
result_anxiety_PE_size <- metagen(
  TE = beta,
  seTE = std_error,
  data = meta_reg_df_anxiety,
  byvar = PE_size, 
  studlab = as.character(meta_reg_df_anxiety$pilot_nr),
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "REML",
  hakn = TRUE,
  subgroup.name = "Size of PE"  # Descriptive name for the subgroups
)

forest(result_anxiety_PE_size,
       sortvar = NULL,  # No additional sorting, data is pre-sorted
       comb.fixed = TRUE,
       comb.random = TRUE,
       xlim = c(-0.5, 0.5),            # Adjust these limits based on your effect size range
       xlab = "Beta: anxiety ~ PE", # Label for the x-axis
       alim = c(-0.5, 0.5),       
       leftcols = c("studlab", "TE", "ci"),
       rightcols = c("w.fixed", "w.random"),
       order = result_anxiety$data$pilot_nr)  

# make the plot for modality
result_anxiety_modality <- metagen(
  TE = beta,
  seTE = std_error,
  data = meta_reg_df_anxiety,
  byvar = modality, 
  studlab = as.character(meta_reg_df_anxiety$pilot_nr),
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "REML",
  hakn = TRUE,
  subgroup.name = "modality:"  # Descriptive name for the subgroups
)

forest(result_anxiety_modality,
       sortvar = NULL,  # No additional sorting, data is pre-sorted
       comb.fixed = TRUE,
       comb.random = TRUE,
       xlim = c(-0.5, 0.5),            # Adjust these limits based on your effect size range
       xlab = "Beta: anxiety ~ PE", # Label for the x-axis
       alim = c(-0.5, 0.5),       
       leftcols = c("studlab", "TE", "ci"),
       rightcols = c("w.fixed", "w.random"),
       order = result_anxiety$data$pilot_nr)  
