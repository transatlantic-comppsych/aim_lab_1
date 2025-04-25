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
data_18_25_pro <- read.csv("final_df_18_25_Pro_with_stresslevels.csv") %>%
  clean_data() %>%
  rename(Depression_score = CESD_score) %>%
  mutate(Dataset = "18_25_Pro")
data_18_25_pro[, c("Depression_score")] <- scale(data_18_25_pro[, c("Depression_score")])

data_26_45_pro <- read.csv("final_df_26_45_Pro_with_stresslevels.csv") %>%
  clean_data() %>%
  rename(Depression_score = CESD_score) %>%
  mutate(Dataset = "26_45_Pro")
data_26_45_pro[, c("Depression_score")] <- scale(data_26_45_pro[, c("Depression_score")])

data_14_18 <- read.csv("final_df_students_with_stresslevels.csv") %>%
  mutate(Depression_Threshold = ifelse(Score..Major.Depression >= 15, 1, 0)) 

data_14_18 <- data_14_18 %>%
rename(Random_ID = Random_ID_new) %>%
  rename(Depression_score = Score..Major.Depression) %>%
  clean_data() %>%
  mutate(Dataset = "14_18")
data_14_18[, c("Depression_score")] <- scale(data_14_18[, c("Depression_score")])

data_18_25_com <- read.csv("final_df_18_25_com_with_stresslevels.csv") %>%
  rename(Random_ID = Random_ID_new) %>%
  rename(Depression_score = CESD_score) %>%
  clean_data() %>%
  mutate(Dataset = "18_25_Com")
data_18_25_com[, c("Depression_score")] <- scale(data_18_25_com[, c("Depression_score")])

# Check if all datasets have the same column names
column_names_list <- list(
  colnames(data_18_25_pro),
  colnames(data_26_45_pro),
  colnames(data_14_18),
  colnames(data_18_25_com)
)

# Print column names for verification
print(column_names_list)


merged_data <- bind_rows(data_18_25_pro, data_26_45_pro, data_14_18, data_18_25_com)

# View the first few rows
head(merged_data)

write.csv(merged_data,"surprise_task_merged_data.csv")
