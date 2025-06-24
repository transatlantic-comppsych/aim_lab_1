# Load necessary library
library(dplyr)

setwd("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/Surprise_task_data_analysis")


# Define columns to remove
cols_to_remove <- c( "Score..Separation.Anxiety",   
                    "Score..Generalised.Anxiety", "Score..Obsessive.Compulsive", "Score..Social.Phobia", "Score..Panic.Disorder",
                   "Depression_status" )


# Function to remove unwanted columns if they exist
clean_data <- function(df) {
  df %>% select(-any_of(cols_to_remove))
}

# Read and clean datasets
data_pilot21 <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/Extracted_data/pilot_21_variables.csv") %>%
  clean_data() %>%
  rename(Depression_score = CESD_score) %>%
  mutate(Dataset = "Pilot_21")
#data_pilot21[, c("Depression_score")] <- scale(data_pilot21[, c("Depression_score")])

data_pilot_21_lsas_v3 <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_pilot_21/SUP_PRF_p21_vid_bigPE_nohist_newjud_v3/SUP_PRF_p21_vid_bigPE_nohist_newjud_v3_lsas_sr.csv")
data_pilot_21_lsas_v4 <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_pilot_21/SUP_PRF_p21_vid_bigPE_nohist_newjud_v4/SUP_PRF_p21_vid_bigPE_nohist_newjud_v4_lsas_sr.csv")
data_pilot_21_lsas <- rbind(data_pilot_21_lsas_v3,data_pilot_21_lsas_v4)
# Generate a sequence of every other column from 31 to 125
cols_to_sum <- seq(31, 125, by = 2)
# Compute the row-wise sum across those columns
data_pilot_21_lsas$LSAS_Total <- rowSums(data_pilot_21_lsas[, cols_to_sum], na.rm = TRUE)
data_pilot_21_lsas$SA_status_LSAS <- ifelse(data_pilot_21_lsas$LSAS_Total > 29, "SA", "no_SA")
# Merge only the needed columns from df into df2 by Random_ID
data_pilot21 <- merge(data_pilot21, data_pilot_21_lsas[, c("Random_ID", "LSAS_Total", "SA_status_LSAS")], 
             by = "Random_ID", 
             all.x = TRUE)


data_18_25_pro <- read.csv("final_df_18_25_Pro_with_stresslevels.csv") %>%
  clean_data() %>%
  rename(Depression_score = CESD_score) %>%
  mutate(Dataset = "18_25_Pro")
#data_18_25_pro[, c("Depression_score")] <- scale(data_18_25_pro[, c("Depression_score")])

data_18_25_pro_lsas <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_study_prolific_18-25/SUP_PRF_18-25_lsas_sr.csv")
# Generate a sequence of every other column from 30 to 124
cols_to_sum <- seq(30, 124, by = 2)
# Compute the row-wise sum across those columns
data_18_25_pro_lsas$LSAS_Total <- rowSums(data_18_25_pro_lsas[, cols_to_sum], na.rm = TRUE)
data_18_25_pro_lsas$SA_status_LSAS <- ifelse(data_18_25_pro_lsas$LSAS_Total > 29, "SA", "no_SA")
# Merge only the needed columns from df into df2 by Random_ID
data_18_25_pro <- merge(data_18_25_pro, data_18_25_pro_lsas[, c("Random_ID", "LSAS_Total", "SA_status_LSAS")], 
                      by = "Random_ID", 
                      all.x = TRUE)


data_26_45_pro <- read.csv("final_df_26_45_Pro_with_stresslevels.csv") %>%
  clean_data() %>%
  rename(Depression_score = CESD_score) %>%
  mutate(Dataset = "26_45_Pro")
#data_26_45_pro[, c("Depression_score")] <- scale(data_26_45_pro[, c("Depression_score")])

data_26_45_pro_lsas <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_study_prolific_26-45/SUP_PRF_26-45_lsas_sr.csv")
# Generate a sequence of every other column from 30 to 124
cols_to_sum <- seq(30, 124, by = 2)
# Compute the row-wise sum across those columns
data_26_45_pro_lsas$LSAS_Total <- rowSums(data_26_45_pro_lsas[, cols_to_sum], na.rm = TRUE)
data_26_45_pro_lsas$SA_status_LSAS <- ifelse(data_26_45_pro_lsas$LSAS_Total > 29, "SA", "no_SA")
# Merge only the needed columns from df into df2 by Random_ID
data_26_45_pro <- merge(data_26_45_pro, data_26_45_pro_lsas[, c("Random_ID", "LSAS_Total", "SA_status_LSAS")], 
                        by = "Random_ID", 
                        all.x = TRUE)

data_14_18 <- read.csv("final_df_students_with_stresslevels.csv") %>%
  mutate(Depression_Threshold = ifelse(Score..Major.Depression >= 15, 1, 0)) 

data_14_18 <- data_14_18 %>%
rename(Random_ID = Random_ID_new) %>%
  rename(Depression_score = Score..Major.Depression) %>%
  clean_data() %>%
  mutate(Dataset = "14_18")
#data_14_18[, c("Depression_score")] <- scale(data_14_18[, c("Depression_score")])
data_14_18_lsas_v7 <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_study_schools/surprise_study_schools_v7/SUP_SCH_v7_lsas_ca.csv")
data_14_18_lsas_v9 <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_study_schools/surprise_study_schools_v9/SUP_SCH_v9_lsas_ca.csv")
data_14_18_lsas_v9 <- data_14_18_lsas_v9[, !(names(data_14_18_lsas_v9) %in% "Participant.External.Session.ID")]
data_14_18_lsas <- rbind(data_14_18_lsas_v7, data_14_18_lsas_v9)
data_14_18_lsas <- data_14_18_lsas %>%
  rename(Random_ID = Random_ID_new) 
# Generate a sequence of every other column from 32 to 126
cols_to_sum <- seq(32, 126, by = 2)
# Compute the row-wise sum across those columns
data_14_18_lsas$LSAS_Total <- rowSums(data_14_18_lsas[, cols_to_sum] - 1, na.rm = TRUE) #subtracted 1 from all columns bc they were coded differently
data_14_18_lsas$SA_status_LSAS <- ifelse(data_14_18_lsas$LSAS_Total > 29.5, "SA", "no_SA")
# Merge only the needed columns from df into df2 by Random_ID
data_14_18 <- merge(data_14_18, data_14_18_lsas[, c("Random_ID", "LSAS_Total", "SA_status_LSAS")], 
                        by = "Random_ID", 
                        all.x = TRUE)


data_18_25_com <- read.csv("final_df_18_25_com_with_stresslevels.csv") %>%
  rename(Random_ID = Random_ID_new) %>%
  rename(Depression_score = CESD_score) %>%
  clean_data() %>%
  mutate(Dataset = "18_25_Com")
#data_18_25_com[, c("Depression_score")] <- scale(data_18_25_com[, c("Depression_score")])
data_18_25_com_lsas_v1 <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_study_community_1/SUP_COM_18-25_v1_lsas_sr.csv")
data_18_25_com_lsas_v2 <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/De_identified_data/surprise_study_community_2/SUP_COM_18-25_v2_lsas_sr.csv")
data_18_25_com_lsas_v2 <- data_18_25_com_lsas_v2[, !(names(data_18_25_com_lsas_v2) %in% "X")]
data_18_25_com_lsas <- rbind(data_18_25_com_lsas_v1, data_18_25_com_lsas_v2)
data_18_25_com_lsas <- data_18_25_com_lsas %>%
  rename(Random_ID = Random_ID_new) 
# Generate a sequence of every other column from 31 to 125
cols_to_sum <- seq(31, 125, by = 2)
# Compute the row-wise sum across those columns
data_18_25_com_lsas$LSAS_Total <- rowSums(data_18_25_com_lsas[, cols_to_sum], na.rm = TRUE)
data_18_25_com_lsas$SA_status_LSAS <- ifelse(data_18_25_com_lsas$LSAS_Total > 29, "SA", "no_SA")
# Merge only the needed columns from df into df2 by Random_ID
data_18_25_com <- merge(data_18_25_com, data_18_25_com_lsas[, c("Random_ID", "LSAS_Total", "SA_status_LSAS")], 
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

# Print column names for verification
print(column_names_list)


merged_data <- bind_rows(data_18_25_pro, data_26_45_pro, data_14_18, data_18_25_com, data_pilot21)

# View the first few rows
head(merged_data)

duplicates <- duplicated(merged_data)

# View duplicated rows
merged_data[duplicates, ]

library(dplyr)

# Assuming your dataframe is named 'df'
merged_data %>%
  group_by(Dataset) %>%
  summarise(Total_Trials = n()) 

merged_data %>%
  group_by(Random_ID) %>%
  summarise(Total_Trials = n())

merged_data <- merged_data[, !(names(merged_data) %in% "X")]


#write.csv(merged_data,"surprise_task_merged_data.csv")




# Add demographics  -------------------------------------------------------
merged_demo <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollxegeLondon/Desktop/GitHub/aim_lab_1/Surprise_task_data_analysis/merged_data_demographics.csv")
#merged_task_data <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/Surprise_task_data_analysis/surprise_task_merged_data.csv")
merged_task_data <- merged_data

# Identify duplicate columns (shared columns besides Random_ID)
shared_cols <- intersect(names(merged_demo), names(merged_task_data))
shared_cols <- setdiff(shared_cols, "Random_ID")

# Drop those columns from one dataset (usually the one you consider less "authoritative")
merged_task_data_clean <- merged_task_data %>%
  select(-all_of(shared_cols))

# Now merge without duplicating columns
merged_data_w_demographics <- inner_join(merged_demo, merged_task_data_clean, by = "Random_ID")



library(dplyr)

# Updated function using AGE instead of SchoolYear
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

# Apply to your dataset
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


