# Merge task data ---------------------------------------------------------

# Load libraries
library(dplyr)
library(here)


here()



# Read and clean datasets
data_pilot21 <- read.csv(here("Data", "Extracted_data", "Prolific participants", "pilot_21_variables.csv")) %>%
  rename(Depression_score = CESD_score) %>%
  mutate(Dataset = "Pilot_21")
data_pilot21$Depression_score_scaled <- scale(data_pilot21[, c("Depression_score")])

data_pilot_21_lsas_v3 <- read.csv(here( "Data", "Raw_data", "Prolific participants", "Prolific participants (18-25; pilot 21)","SUP_PRF_p21_vid_bigPE_nohist_newjud_v3","SUP_PRF_p21_vid_bigPE_nohist_newjud_v3_lsas_sr.csv"))
data_pilot_21_lsas_v4 <- read.csv(here( "Data", "Raw_data", "Prolific participants", "Prolific participants (18-25; pilot 21)", "SUP_PRF_p21_vid_bigPE_nohist_newjud_v4", "SUP_PRF_p21_vid_bigPE_nohist_newjud_v4_lsas_sr.csv"))
data_pilot_21_lsas <- rbind(data_pilot_21_lsas_v3, data_pilot_21_lsas_v4)

cols_to_sum <- seq(30, 124, by = 2)
data_pilot_21_lsas$LSAS_Total <- rowSums(data_pilot_21_lsas[, cols_to_sum], na.rm = TRUE)
data_pilot_21_lsas$SA_status_LSAS <- ifelse(data_pilot_21_lsas$LSAS_Total > 29, "SA", "no_SA")
cols_to_sum_anxiety <- seq(30, 122, by = 4)
cols_to_sum_avoidance <- seq(32, 124, by = 4)
data_pilot_21_lsas$LSAS_Anxiety_score <- rowSums(data_pilot_21_lsas[, cols_to_sum_anxiety], na.rm = TRUE)
data_pilot_21_lsas$LSAS_Avoidance_score <- rowSums(data_pilot_21_lsas[, cols_to_sum_avoidance], na.rm = TRUE)

data_pilot21 <- merge(
  data_pilot21, 
  data_pilot_21_lsas[, c("Random_ID", "LSAS_Total", "SA_status_LSAS", "LSAS_Anxiety_score", "LSAS_Avoidance_score")], 
  by = "Random_ID", 
  all.x = TRUE
)

data_18_25_pro <- read.csv(here("Data", "Extracted_data", "Prolific participants", "surprise_study_18_25_Pro_variables.csv")) %>%
  rename(Depression_score = CESD_score) %>%
  mutate(Dataset = "18_25_Pro")
data_18_25_pro$Depression_score_scaled <- scale(data_18_25_pro[, c("Depression_score")])

data_18_25_pro_lsas <- read.csv(here("Data", "Raw_data", "Prolific participants", "Prolific participants (18-25)", "SUP_PRF_18-25_lsas_sr.csv"))
cols_to_sum <- seq(29, 123, by = 2)
data_18_25_pro_lsas$LSAS_Total <- rowSums(data_18_25_pro_lsas[, cols_to_sum], na.rm = TRUE)
data_18_25_pro_lsas$SA_status_LSAS <- ifelse(data_18_25_pro_lsas$LSAS_Total > 29, "SA", "no_SA")
cols_to_sum_anxiety <- seq(29, 121, by = 4)
cols_to_sum_avoidance <- seq(31, 123, by = 4)
data_18_25_pro_lsas$LSAS_Anxiety_score <- rowSums(data_18_25_pro_lsas[, cols_to_sum_anxiety], na.rm = TRUE)
data_18_25_pro_lsas$LSAS_Avoidance_score <- rowSums(data_18_25_pro_lsas[, cols_to_sum_avoidance], na.rm = TRUE)

data_18_25_pro <- merge(
  data_18_25_pro, 
  data_18_25_pro_lsas[, c("Random_ID", "LSAS_Total", "SA_status_LSAS", "LSAS_Anxiety_score", "LSAS_Avoidance_score")], 
  by = "Random_ID", 
  all.x = TRUE
)

data_26_45_pro <- read.csv(here("Data", "Extracted_data", "Prolific participants", "surprise_study_26_45_Pro_variables.csv")) %>%
  rename(Depression_score = CESD_score) %>%
  mutate(Dataset = "26_45_Pro")
data_26_45_pro$Depression_score_scaled <- scale(data_26_45_pro[, c("Depression_score")])

data_26_45_pro_lsas <- read.csv(here("Data", "Raw_data", "Prolific participants", "Prolific participants (26-45)", "SUP_PRF_26-45_lsas_sr.csv"))
cols_to_sum <- seq(29, 123, by = 2)
data_26_45_pro_lsas$LSAS_Total <- rowSums(data_26_45_pro_lsas[, cols_to_sum], na.rm = TRUE)
data_26_45_pro_lsas$SA_status_LSAS <- ifelse(data_26_45_pro_lsas$LSAS_Total > 29, "SA", "no_SA")
cols_to_sum_anxiety <- seq(29, 121, by = 4)
cols_to_sum_avoidance <- seq(31, 123, by = 4)
data_26_45_pro_lsas$LSAS_Anxiety_score <- rowSums(data_26_45_pro_lsas[, cols_to_sum_anxiety], na.rm = TRUE)
data_26_45_pro_lsas$LSAS_Avoidance_score <- rowSums(data_26_45_pro_lsas[, cols_to_sum_avoidance], na.rm = TRUE)

data_26_45_pro <- merge(
  data_26_45_pro, 
  data_26_45_pro_lsas[, c("Random_ID", "LSAS_Total", "SA_status_LSAS", "LSAS_Anxiety_score", "LSAS_Avoidance_score")], 
  by = "Random_ID", 
  all.x = TRUE
)

data_14_18 <- read.csv(here( "Data", "Extracted_data", "School participants", "surprise_study_14_18_variables.csv")) %>%
  mutate(Depression_Threshold = ifelse(Score..Major.Depression >= 15, 1, 0)) %>%
  rename(Random_ID = Random_ID_new) %>%
  rename(Depression_score = Score..Major.Depression) %>%
  mutate(Dataset = "14_18")
data_14_18$Depression_score_scaled <- scale(data_14_18[, c("Depression_score")])

data_14_18_lsas_v7 <- read.csv(here("Data", "Raw_data", "School participants", "surprise_study_schools_v7", "SUP_SCH_v7_lsas_ca.csv"))
data_14_18_lsas_v9 <- read.csv(here("Data", "Raw_data", "School participants", "surprise_study_schools_v9", "SUP_SCH_v9_lsas_ca.csv"))
data_14_18_lsas_v9 <- data_14_18_lsas_v9[, !(names(data_14_18_lsas_v9) %in% "Participant.External.Session.ID")]
data_14_18_lsas <- rbind(data_14_18_lsas_v7, data_14_18_lsas_v9) %>%
  rename(Random_ID = Random_ID_new)
cols_to_sum <- seq(31, 125, by = 2)
data_14_18_lsas$LSAS_Total <- rowSums(data_14_18_lsas[, cols_to_sum] - 1, na.rm = TRUE)
data_14_18_lsas$SA_status_LSAS <- ifelse(data_14_18_lsas$LSAS_Total > 29.5, "SA", "no_SA")
cols_to_sum_anxiety <- seq(31, 123, by = 4)
cols_to_sum_avoidance <- seq(33, 125, by = 4)
data_14_18_lsas$LSAS_Anxiety_score <- rowSums(data_14_18_lsas[, cols_to_sum_anxiety], na.rm = TRUE)
data_14_18_lsas$LSAS_Avoidance_score <- rowSums(data_14_18_lsas[, cols_to_sum_avoidance], na.rm = TRUE)

data_14_18 <- merge(
  data_14_18, 
  data_14_18_lsas[, c("Random_ID", "LSAS_Total", "SA_status_LSAS", "LSAS_Anxiety_score","LSAS_Avoidance_score")], 
  by = "Random_ID", 
  all.x = TRUE
)

data_18_25_com <- read.csv(here( "Data", "Extracted_data", "Local Community Participants", "surprise_study_18_25_com_variables.csv")) %>%
  rename(Random_ID = Random_ID_new) %>%
  rename(Depression_score = CESD_score) %>%
  #clean_data() %>%
  mutate(Dataset = "18_25_Com")
data_18_25_com$Depression_score_scaled <- scale(data_18_25_com[, c("Depression_score")])

data_18_25_com_lsas_v1 <- read.csv(here( "Data", "Raw_data", "Local Community Participants", "Local Community Participants v1", "SUP_COM_18-25_v1_lsas_sr.csv"))
data_18_25_com_lsas_v2 <- read.csv(here("Data", "Raw_data", "Local Community Participants", "Local Community Participants v2", "SUP_COM_18-25_v2_lsas_sr.csv"))
data_18_25_com_lsas_v2 <- data_18_25_com_lsas_v2[, !(names(data_18_25_com_lsas_v2) %in% "X")]
data_18_25_com_lsas <- rbind(data_18_25_com_lsas_v1, data_18_25_com_lsas_v2) %>%
  rename(Random_ID = Random_ID_new)
cols_to_sum <- seq(30, 124, by = 2)
data_18_25_com_lsas$LSAS_Total <- rowSums(data_18_25_com_lsas[, cols_to_sum], na.rm = TRUE)
data_18_25_com_lsas$SA_status_LSAS <- ifelse(data_18_25_com_lsas$LSAS_Total > 29, "SA", "no_SA")
cols_to_sum_anxiety <- seq(30, 122, by = 4)
cols_to_sum_avoidance <- seq(32, 124, by = 4)
data_18_25_com_lsas$LSAS_Anxiety_score <- rowSums(data_18_25_com_lsas[, cols_to_sum_anxiety], na.rm = TRUE)
data_18_25_com_lsas$LSAS_Avoidance_score <- rowSums(data_18_25_com_lsas[, cols_to_sum_avoidance], na.rm = TRUE)

data_18_25_com <- merge(
  data_18_25_com, 
  data_18_25_com_lsas[, c("Random_ID", "LSAS_Total", "SA_status_LSAS", "LSAS_Anxiety_score","LSAS_Avoidance_score")], 
  by = "Random_ID", 
  all.x = TRUE)

# Check if all datasets have the same column names
column_names_list <- list(
  colnames(data_18_25_pro),
  colnames(data_26_45_pro),
  colnames(data_14_18),
  colnames(data_18_25_com), 
  colnames(data_pilot21)
  
)

print(column_names_list)


merged_data <- bind_rows(data_18_25_pro, data_26_45_pro, data_14_18, data_18_25_com, data_pilot21)
merged_data <- merged_data[, !(names(merged_data) %in% "X")]

# Merge demographics ------------------------------------------------------


pilot21_demo_v3 <- read.csv(here("Data", "Raw_data", "Prolific participants","Prolific participants (18-25; pilot 21)", "SUP_PRF_p21_vid_bigPE_nohist_newjud_v3", "SUP_PRF_p21_vid_bigPE_nohist_newjud_v3_clinical_srv.csv"))
pilot21_demo_v4 <- read.csv(here("Data", "Raw_data","Prolific participants", "Prolific participants (18-25; pilot 21)", "SUP_PRF_p21_vid_bigPE_nohist_newjud_v4", "SUP_PRF_p21_vid_bigPE_nohist_newjud_v4_clinical_srv.csv"))
pilot21_demo <- rbind(pilot21_demo_v3,pilot21_demo_v4)
pilot21_demo$other_mental_health_diagnoses.object.7.Autism.Spectrum.Disorder <- NA
pilot21_demo <- pilot21_demo %>%
  rename(Ethnicity = Ethnicity..object.28.Response,
         Age = Dropdown.object.19.Response,
         Sex = Sex.object.25.Response,
         Gender = Gender.object.24.Response,
         Gender_other = Gender.object.24.Other,
         Mental_health_diagnosis = mental_health_diagnosis.object.3.Response,
         Depression = other_mental_health_diagnoses.object.7.Depression,
         Anxiety = other_mental_health_diagnoses.object.7.Anxiety.disorder,
         ADHD = other_mental_health_diagnoses.object.7.ADHD,
         Other_diagnoses = other_mental_health_diagnoses.object.7.__other,
         medication_status = medication_status.object.5.Response,
         fluoxetine = fluoxetine_prozac.object.10.Response,
         fluoxetine_dose = dose_fluoxetine_prozac.object.11.Value,
         sertraline = sertraline_zoloft.object.12.Response,
         sertraline_dose = dose_sertraline_zoloft.object.13.Value,
         ritalin = Ritalin.object.14.Response,
         ritalin_dose = dose_Ritalin.object.15.Value,
         other_medication = other_medication.object.16.Value,
         ethnicity_other = Ethnicity..object.28.Other,
         ASD = other_mental_health_diagnoses.object.7.Autism.Spectrum.Disorder)
pilot21_demo$dataset <- "Pilot_21"

pro_18_25_demo <- read.csv(here("Data", "Raw_data", "Prolific participants","Prolific participants (18-25)", "SUP_PRF_18-25_clinical_srv.csv"))
pro_18_25_demo <- pro_18_25_demo %>%
  rename(Ethnicity = Ethnicity..object.28.Response,
         Age = Dropdown.object.19.Response,
         Sex = Sex.object.25.Response,
         Gender = Gender.object.24.Response,
         Gender_other = Gender.object.24.Other,
         Mental_health_diagnosis = mental_health_diagnosis.object.3.Response,
         Depression = other_mental_health_diagnoses.object.7.Depression,
         Anxiety = other_mental_health_diagnoses.object.7.Anxiety.disorder,
         ADHD = other_mental_health_diagnoses.object.7.ADHD,
         Other_diagnoses = other_mental_health_diagnoses.object.7.__other,
         medication_status = medication_status.object.5.Response,
         fluoxetine = fluoxetine_prozac.object.10.Response,
         fluoxetine_dose = dose_fluoxetine_prozac.object.11.Value,
         sertraline = sertraline_zoloft.object.12.Response,
         sertraline_dose = dose_sertraline_zoloft.object.13.Value,
         ritalin = Ritalin.object.14.Response,
         ritalin_dose = dose_Ritalin.object.15.Value,
         other_medication = other_medication.object.16.Value,
         ethnicity_other = Ethnicity..object.28.Other,
         ASD = other_mental_health_diagnoses.object.7.Autism.Spectrum.Disorder)
pro_18_25_demo$dataset <- "pro_18_25"


pro_26_45_demo<- read.csv(here("Data", "Raw_data", "Prolific participants","Prolific participants (26-45)", "SUP_PRF_26-45_clinical_srv.csv"))
pro_26_45_demo <- pro_26_45_demo %>%
  rename(Ethnicity = Ethnicity..object.28.Response,
         Age = Dropdown.object.19.Response,
         Sex = Sex.object.25.Response,
         Gender = Gender.object.24.Response,
         Gender_other = Gender.object.24.Other,
         Mental_health_diagnosis = mental_health_diagnosis.object.3.Response,
         Depression = other_mental_health_diagnoses.object.7.Depression,
         Anxiety = other_mental_health_diagnoses.object.7.Anxiety.disorder,
         ADHD = other_mental_health_diagnoses.object.7.ADHD,
         Other_diagnoses = other_mental_health_diagnoses.object.7.__other,
         medication_status = medication_status.object.5.Response,
         fluoxetine = fluoxetine_prozac.object.10.Response,
         fluoxetine_dose = dose_fluoxetine_prozac.object.11.Value,
         sertraline = sertraline_zoloft.object.12.Response,
         sertraline_dose = dose_sertraline_zoloft.object.13.Value,
         ritalin = Ritalin.object.14.Response,
         ritalin_dose = dose_Ritalin.object.15.Value,
         other_medication = other_medication.object.16.Value,
         ethnicity_other = Ethnicity..object.28.Other,
         ASD = other_mental_health_diagnoses.object.7.Autism.Spectrum.Disorder)
pro_26_45_demo$dataset <- "pro_26_45"

school_demo_v7 <- read.csv(here("Data", "Raw_data", "School participants","surprise_study_schools_v7", "SUP_SCH_v7_clinical_srv.csv"))
school_demo_v7$other_mental_health_diagnoses.object.7.Autism.Spectrum.Disorder <- NA
school_demo_v9 <- read.csv(here("Data", "Raw_data", "School participants","surprise_study_schools_v9", "SUP_SCH_v9_clinical_srv.csv"))
school_demo_v9 <- school_demo_v9[, !names(school_demo_v9) %in% "Participant.External.Session.ID"]
school_demo <- rbind(school_demo_v7,school_demo_v9)
school_demo <- school_demo %>%
  rename(Random_ID = Random_ID_new,
         Ethnicity = Ethnicity..object.26.Response,
         Age = Dropdown.object.19.Response,
         Sex = Sex.object.25.Response,
         Gender = Gender.object.24.Response,
         Gender_other = Gender.object.24.Other,
         Mental_health_diagnosis = mental_health_diagnosis.object.3.Response,
         Depression = other_mental_health_diagnoses.object.7.Depression,
         Anxiety = other_mental_health_diagnoses.object.7.Anxiety.disorder,
         ADHD = other_mental_health_diagnoses.object.7.ADHD,
         Other_diagnoses = other_mental_health_diagnoses.object.7.__other,
         medication_status = medication_status.object.5.Response,
         fluoxetine = fluoxetine_prozac.object.10.Response,
         fluoxetine_dose = dose_fluoxetine_prozac.object.11.Value,
         sertraline = sertraline_zoloft.object.12.Response,
         sertraline_dose = dose_sertraline_zoloft.object.13.Value,
         ritalin = Ritalin.object.14.Response,
         ritalin_dose = dose_Ritalin.object.15.Value,
         other_medication = other_medication.object.16.Value,
         ethnicity_other = Ethnicity..object.26.Other,
         ASD = other_mental_health_diagnoses.object.7.Autism.Spectrum.Disorder)
school_demo$dataset <- "school"


local_com_demo_v1 <- read.csv(here("Data", "Raw_data", "Local Community Participants","Local Community Participants v1", "SUP_COM_18-25_v1_clinical_srv.csv"))
local_com_demo_v2 <- read.csv(here("Data", "Raw_data", "Local Community Participants","Local Community Participants v2", "SUP_COM_18-25_v2_clinical_srv.csv"))
local_com_demo_v2 <- local_com_demo_v2[, !names(local_com_demo_v2) %in% "X"]
local_com_demo <- rbind(local_com_demo_v1,local_com_demo_v2)
local_com_demo <- local_com_demo %>%
  rename( Random_ID = Random_ID_new,
         Ethnicity = Ethnicity..object.28.Response,
         Age = Dropdown.object.19.Response,
         Sex = Sex.object.25.Response,
         Gender = Gender.object.24.Response,
         Gender_other = Gender.object.24.Other,
         Mental_health_diagnosis = mental_health_diagnosis.object.3.Response,
         Depression = other_mental_health_diagnoses.object.7.Depression,
         Anxiety = other_mental_health_diagnoses.object.7.Anxiety.disorder,
         ADHD = other_mental_health_diagnoses.object.7.ADHD,
         Other_diagnoses = other_mental_health_diagnoses.object.7.__other,
         medication_status = medication_status.object.5.Response,
         fluoxetine = fluoxetine_prozac.object.10.Response,
         fluoxetine_dose = dose_fluoxetine_prozac.object.11.Value,
         sertraline = sertraline_zoloft.object.12.Response,
         sertraline_dose = dose_sertraline_zoloft.object.13.Value,
         ritalin = Ritalin.object.14.Response,
         ritalin_dose = dose_Ritalin.object.15.Value,
         other_medication = other_medication.object.16.Value,
         ethnicity_other = Ethnicity..object.28.Other,
         ASD = other_mental_health_diagnoses.object.7.Autism.Spectrum.Disorder)
local_com_demo <- local_com_demo[, !names(local_com_demo) %in% "Participant.External.Session.ID"]
local_com_demo$dataset <- "local_com"

columns_to_keep <- c(
  "Random_ID",
  "Age",
  "Sex",
  "Gender",
  "Gender_other",
  "Ethnicity",
  "ethnicity_other",
  "Mental_health_diagnosis",
  "Depression",
  "Anxiety",
  "ADHD",
  "ASD", 
  "Other_diagnoses",
  "medication_status",
  "fluoxetine",
  "fluoxetine_dose",
  "sertraline",
  "sertraline_dose",
  "ritalin",
  "ritalin_dose",
  "other_medication",
  "dataset"
)

# Combine datasets, keeping only the specified columns
merged_demo<- rbind(
  subset(pilot21_demo, select = columns_to_keep),
  subset(pro_18_25_demo, select = columns_to_keep), 
  subset(pro_26_45_demo, select = columns_to_keep), 
  subset(school_demo, select = columns_to_keep), 
  subset(local_com_demo, select = columns_to_keep) 
  )


merged_data <- read.csv(here("Data", "Extracted_data", "Merged Data - all groups", "surprise_task_merged_data.csv"))
# Keep only rows in merged_demo that have a Random_ID also in merged_data (i.e. get rid of people who did not complete task)
filtered_demo <- merged_demo %>%
  filter(Random_ID %in% merged_data$Random_ID)


# Merge task and demo data -------------------------------------------------------
merged_demo <- filtered_demo
# Identify duplicate columns (shared columns besides Random_ID)
shared_cols <- intersect(names(merged_demo), names(merged_data))
shared_cols <- setdiff(shared_cols, "Random_ID")

# Drop those columns from one dataset 
merged_task_data_clean <- merged_data %>%
  select(-all_of(shared_cols))

# Merge task and demographics datasets
merged_data_w_demographics <- inner_join(merged_demo, merged_task_data_clean, by = "Random_ID")

#Assign RCADS depression thresholds and categorise participants according to age and gender
get_depression_category <- function(score, sex, age) {
  thresholds <- list(
    "14_15" = list("Male" = c(normal = 13, raised = 15), "Female" = c(normal = 12, raised = 14)),
    "16_18" = list("Male" = c(normal = 12, raised = 14), "Female" = c(normal = 15, raised = 18))
  )
  
  age_band <- case_when(
    age %in% 14:15 ~ "14_15",
    age %in% 16:18 ~ "16_18",
    TRUE ~ NA_character_
  )
  
  if (is.na(age_band) || is.na(score) || is.na(sex)) return(NA)
  
  g <- ifelse(tolower(sex) %in% c("male", "m"), "Male", "Female")
  cutoff <- thresholds[[age_band]][[g]]
  
  if (score <= cutoff["normal"]) {
    return("Normal")
  } else if (score <= cutoff["raised"]) {
    return("Raised")
  } else {
    return("High")
  }
}

merged_data_w_demographics <- merged_data_w_demographics %>%
  mutate(
    RCADS_depression = case_when(
      dataset == "school" ~ mapply(
        get_depression_category,
        score = Depression_score,
        sex = Sex,
        age = Age
      ),
      TRUE ~ NA_character_
    )
  )


merged_data_w_demographics <- merged_data_w_demographics %>%
  mutate(
    Depression_Threshold = ifelse(
      dataset == "school" & (RCADS_depression %in% c("Raised", "High")),
      1,
      ifelse(Dataset == "14-18", 0, Depression_Threshold)
    )
  )

merged_data_w_demographics <- merged_data_w_demographics[, !(names(merged_data_w_demographics) %in% "Dataset")]
merged_data <- merged_data[, !(names(merged_data) %in% "Depression_Threshold")]


# Extract participant-level data: one row per Random_ID with Depression_Threshold
participant_level <- merged_data_w_demographics %>%
  select(Random_ID, Depression_Threshold) %>%
  distinct(Random_ID, .keep_all = TRUE)

# Merge participant-level data onto the long format dataset
merged_data <- merged_data %>%
  left_join(participant_level, by = "Random_ID")

write.csv(filtered_demo, "merged_data_demographics.csv")
write.csv(merged_data_w_demographics, "merged_data_w_demographics.csv")
write.csv(merged_data, "surprise_task_merged_data.csv")

