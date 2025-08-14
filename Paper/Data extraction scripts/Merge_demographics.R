pilot21_demo_v3 <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_pilot_21/SUP_PRF_p21_vid_bigPE_nohist_newjud_v3/SUP_PRF_p21_vid_bigPE_nohist_newjud_v3_clinical_srv.csv")
pilot21_demo_v4 <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_pilot_21/SUP_PRF_p21_vid_bigPE_nohist_newjud_v4/SUP_PRF_p21_vid_bigPE_nohist_newjud_v4_clinical_srv.csv")
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

pro_18_25_demo <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_study_prolific_18-25/SUP_PRF_18-25_clinical_srv.csv")
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


pro_26_45_demo<- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_study_prolific_26-45/SUP_PRF_26-45_clinical_srv.csv")
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

school_demo_v7 <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_study_schools/surprise_study_schools_v7/SUP_SCH_v7_clinical_srv.csv")
school_demo_v7$other_mental_health_diagnoses.object.7.Autism.Spectrum.Disorder <- NA
school_demo_v9 <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_study_schools/surprise_study_schools_v9/SUP_SCH_v9_clinical_srv.csv")
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


local_com_demo_v1 <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_study_community_1/SUP_COM_18-25_v1_clinical_srv.csv")
local_com_demo_v2 <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_study_community_2/SUP_COM_18-25_v2_clinical_srv.csv")
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


merged_data <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/Surprise_task_data_analysis/surprise_task_merged_data.csv")
# Keep only rows in merged_demo that have a Random_ID also in merged_data
filtered_demo <- merged_demo %>%
  filter(Random_ID %in% merged_data$Random_ID)
write.csv(filtered_demo, "merged_data_demographics.csv")


# create demographics table -----------------------------------------------
# Load required libraries
library(dplyr)
library(forcats)
library(scales)
library(gt)
library(tidyr)

df <- filtered_demo
# --- STEP 1: Prepare and clean dataset ---
df <- df %>%
  mutate(
    dataset_group = case_when(
      dataset %in% c("Pilot_21", "pro_18_25") ~ "Prolific participants aged 18–25",
      dataset == "pro_26_45" ~ "Prolific participants aged 26–45",
      dataset == "school" ~ "School participants aged 14-18",
      dataset == "local_com" ~ "Local Community aged 18–25",
      TRUE ~ "Other"
    ),
    
    # Flag gender identity differing from sex
    Gender_differs = ifelse(!is.na(Gender_other) & Gender_other != "", 1, 0),
    
    # Clean sex and ethnicity
    Sex = factor(Sex),
    Ethnicity_combined = case_when(
      !is.na(ethnicity_other) & ethnicity_other != ""  ~ paste0("Other: ", ethnicity_other),
      TRUE ~ as.character(Ethnicity)
    )
    
  )

# Set the order of dataset groups
df$dataset_group <- factor(df$dataset_group,
                           levels = c(
                             "Prolific participants aged 18–25",
                             "Prolific participants aged 26–45",
                             "Local Community aged 18–25",
                             "School participants aged 14-18"
                           ))


# Group low-frequency ethnicities into "Other"
df$Ethnicity_combined <- fct_lump(factor(df$Ethnicity_combined), n = 5)

# Ensure diagnosis variables only counted if prior question was "Yes"
df <- df %>%
  mutate(
    Depression = ifelse(Mental_health_diagnosis == "Yes", Depression, NA),
    Anxiety = ifelse(Mental_health_diagnosis == "Yes", Anxiety, NA),
    ADHD = ifelse(Mental_health_diagnosis == "Yes", ADHD, NA),
    ASD = ifelse(Mental_health_diagnosis == "Yes", ASD, NA)
  )

# --- STEP 2: Main demographics table ---

demographics_summary <- df %>%
  group_by(dataset_group) %>%
  summarise(
    N = n(),
    Age = paste0(round(mean(Age, na.rm = TRUE), 1), " (", round(sd(Age, na.rm = TRUE), 1), ")"),
    
    Female = paste0(sum(Sex == "Female", na.rm = TRUE), " (", percent(mean(Sex == "Female", na.rm = TRUE), accuracy = 0.1), ")"),
    Male = paste0(sum(Sex == "Male", na.rm = TRUE), " (", percent(mean(Sex == "Male", na.rm = TRUE), accuracy = 0.1), ")"),
    
    Gender_diff = paste0(sum(Gender_differs == 1, na.rm = TRUE), " (", percent(mean(Gender_differs == 1, na.rm = TRUE), accuracy = 0.1), ")"),
    
    Depression = paste0(sum(Depression == 1, na.rm = TRUE), " (", percent(sum(Depression == 1, na.rm = TRUE) / n(), accuracy = 0.1), ")"),
    Anxiety = paste0(sum(Anxiety == 1, na.rm = TRUE), " (", percent(sum(Anxiety == 1, na.rm = TRUE) / n(), accuracy = 0.1), ")"),
    ADHD = paste0(sum(ADHD == 1, na.rm = TRUE), " (", percent(sum(ADHD == 1, na.rm = TRUE) / n(), accuracy =0.1), ")"),
    ASD = paste0(sum(ASD == 1, na.rm = TRUE), " (", percent(sum(ASD == 1, na.rm = TRUE) / n(), accuracy = 0.1), ")")
  
  ) %>%
  ungroup()

# Display demographics table
demographics_summary %>%
  gt() %>%
  tab_header(title = "Demographic Characteristics by Dataset Group") %>%
  cols_label(
    dataset_group = "Dataset Group",
    N = "N",
    Age = "Age (Mean ± SD)",
    Female = "Female (Sex)",
    Male = "Male (Sex)",
    Gender_diff = "Identifies differently than assigned at birth",
    Depression = "Depression",
    Anxiety = "Anxiety",
    ADHD = "ADHD",
    ASD = "ASD"
  ) %>%
  cols_label(
    Gender_diff = "Identifies differently than assigned at birth")



# --- STEP 3: Ethnicity summary table ---

ethnicity_summary <- df %>%
  group_by(dataset_group, Ethnicity_combined) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(dataset_group) %>%
  mutate(
    pct = percent(n / sum(n), accuracy = 0.1),
    formatted = paste0(n, " (", pct, ")")
  ) %>%
  select(dataset_group, Ethnicity_combined, formatted) %>%
  pivot_wider(names_from = dataset_group, values_from = formatted)

# Display ethnicity table
ethnicity_summary %>%
  gt() %>%
  tab_header(title = "Ethnicity by Dataset Group") %>%
  cols_label(Ethnicity_combined = "Ethnicity")

