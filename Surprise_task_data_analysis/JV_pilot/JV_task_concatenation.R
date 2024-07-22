library(tidyverse)
library(conflicted)
library(dplyr)
library(ggplot2)
library(stringr)


# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/marjan/Desktop/pilot_data_prep_analysis/data_exp_137495-v33-2")

################# Combining CSVs- Questionnaires/Same tasks #################
# This is the script to use if you want to combine questionnaires or identical tasks (differing only e.g. on counterbalancing)
# You list the files you want to combine, each with a "" around them

C1_123 <- c("data_exp_137495-v33_task-vbxe.csv",
           "data_exp_137495-v33_task-wlid.csv",
           "data_exp_137495-v33_task-sqbj.csv")


C2_231 <- c("data_exp_137495-v33_task-e1h9.csv",
            "data_exp_137495-v33_task-mr3z.csv",
            "data_exp_137495-v33_task-dxsp.csv")

C3_321 <- c("data_exp_137495-v33_task-284e.csv",
            "data_exp_137495-v33_task-k4y6.csv",
            "data_exp_137495-v33_task-d9wy.csv")

C4_213 <- c("data_exp_137495-v33_task-ymmw.csv",
            "data_exp_137495-v33_task-7n67.csv",
            "data_exp_137495-v33_task-u3fm.csv")

C5_132 <- c("data_exp_137495-v33_task-s49y.csv",
            "data_exp_137495-v33_task-9nhz.csv",
            "data_exp_137495-v33_task-dwq4.csv")

C6_312 <- c("data_exp_137495-v33_task-ucye.csv",
            "data_exp_137495-v33_task-5ymq.csv",
            "data_exp_137495-v33_task-6rge.csv")


# You can combine the CSVs using either base R or tidyverse (subject to preference)
# using tidyverse
# C1
combined_dataC1_123 <- lapply(C1_123, read_csv) %>% 
  bind_rows()
combined_dataC1_123 <- data.frame(combined_dataC1_123)
# add a new column with column name to know which randomization column was used
combined_dataC1_123 <-  combined_dataC1_123 %>% 
  add_column(randomisation_column = "C1_123")
# remove "END OF FILE" row
combined_dataC1_123 <- combined_dataC1_123[combined_dataC1_123$Event.Index != "END OF FILE",]


# C2
combined_dataC2_231 <- lapply(C2_231, read_csv) %>% 
  bind_rows()
combined_dataC2_231 <- data.frame(combined_dataC2_231)
combined_dataC2_231 <-  combined_dataC2_231 %>% 
  add_column(randomisation_column = "C2_231")
combined_dataC2_231 <- combined_dataC2_231[combined_dataC2_231$Event.Index != "END OF FILE",]


# C3
combined_dataC3_321 <- lapply(C3_321, read_csv) %>% 
  bind_rows()
combined_dataC3_321 <- data.frame(combined_dataC3_321)
combined_dataC3_321 <-  combined_dataC3_321 %>% 
  add_column(randomisation_column = "C3_321")
combined_dataC3_321 <- combined_dataC3_321[combined_dataC3_321$Event.Index != "END OF FILE",]


# C4
combined_dataC4_213 <- lapply(C4_213, read_csv) %>% 
  bind_rows()
combined_dataC4_213 <- data.frame(combined_dataC4_213)
combined_dataC4_213 <-  combined_dataC4_213 %>% 
  add_column(randomisation_column = "C4_213")
combined_dataC4_213 <- combined_dataC4_213[combined_dataC4_213$Event.Index != "END OF FILE",]


# C5
combined_dataC5_132 <- lapply(C5_132, read_csv) %>% 
  bind_rows()
combined_dataC5_132 <- data.frame(combined_dataC5_132)
combined_dataC5_132 <-  combined_dataC5_132 %>% 
  add_column(randomisation_column = "C5_132")
combined_dataC5_132 <- combined_dataC5_132[combined_dataC5_132$Event.Index != "END OF FILE",]


# C6
combined_dataC6_312 <- lapply(C6_312, read_csv) %>% 
  bind_rows()
combined_dataC6_312 <- data.frame(combined_dataC6_312)
combined_dataC6_312 <-  combined_dataC6_312 %>% 
  add_column(randomisation_column = "C6_312")
combined_dataC2_231 <- combined_dataC2_231[combined_dataC2_231$Event.Index != "END OF FILE",]



# second part (MDOT first):

C1_123_second <- c("data_exp_137495-v33_task-empj.csv",
            "data_exp_137495-v33_task-ycba.csv",
            "data_exp_137495-v33_task-wuat.csv")


C2_231_second <- c("data_exp_137495-v33_task-hr3w.csv",
            "data_exp_137495-v33_task-yv6i.csv",
            "data_exp_137495-v33_task-x17x.csv")

C3_321_second <- c("data_exp_137495-v33_task-4kxm.csv",
            "data_exp_137495-v33_task-3d7z.csv",
            "data_exp_137495-v33_task-r4rd.csv")

C4_213_second <- c("data_exp_137495-v33_task-atm6.csv",
            "data_exp_137495-v33_task-o9u1.csv",
            "data_exp_137495-v33_task-mb68.csv")

C5_132_second <- c("data_exp_137495-v33_task-r1zi.csv",
            "data_exp_137495-v33_task-71ex.csv",
            "data_exp_137495-v33_task-v6vm.csv")

C6_312_second <- c("data_exp_137495-v33_task-xnd5.csv",
            "data_exp_137495-v33_task-vuju.csv",
            "data_exp_137495-v33_task-735q.csv")


# You can combine the CSVs using either base R or tidyverse (subject to preference)
# using tidyverse
# C1
combined_dataC1_123_second <- lapply(C1_123_second, read_csv) %>% 
  bind_rows()
combined_dataC1_123_second <- data.frame(combined_dataC1_123_second)
# add a new column with column name to know which randomization column was used
combined_dataC1_123_second <-  combined_dataC1_123_second %>% 
  add_column(randomisation_column = "C1_123_second")
# remove "END OF FILE" row
combined_dataC1_123_second <- combined_dataC1_123_second[combined_dataC1_123_second$Event.Index != "END OF FILE",]


# C2
combined_dataC2_231_second <- lapply(C2_231_second, read_csv) %>% 
  bind_rows()
combined_dataC2_231_second <- data.frame(combined_dataC2_231_second)
combined_dataC2_231_second <-  combined_dataC2_231_second %>% 
  add_column(randomisation_column = "C2_231_second")
combined_dataC2_231_second <- combined_dataC2_231_second[combined_dataC2_231_second$Event.Index != "END OF FILE",]


# C3
combined_dataC3_321_second <- lapply(C3_321_second, read_csv) %>% 
  bind_rows()
combined_dataC3_321_second <- data.frame(combined_dataC3_321_second)
combined_dataC3_321_second <-  combined_dataC3_321_second %>% 
  add_column(randomisation_column = "C3_321_second")
combined_dataC3_321_second <- combined_dataC3_321_second[combined_dataC3_321_second$Event.Index != "END OF FILE",]


# C4
combined_dataC4_213_second <- lapply(C4_213_second, read_csv) %>% 
  bind_rows()
combined_dataC4_213_second <- data.frame(combined_dataC4_213_second)
combined_dataC4_213_second <-  combined_dataC4_213_second %>% 
  add_column(randomisation_column = "C4_213")
combined_dataC4_213_second <- combined_dataC4_213_second[combined_dataC4_213_second$Event.Index != "END OF FILE",]


# C5
combined_dataC5_132_second <- lapply(C5_132_second, read_csv) %>% 
  bind_rows()
combined_dataC5_132_second <- data.frame(combined_dataC5_132_second)
combined_dataC5_132_second <-  combined_dataC5_132_second %>% 
  add_column(randomisation_column = "C5_132_second")
combined_dataC5_132_second <- combined_dataC5_132_second[combined_dataC5_132_second$Event.Index != "END OF FILE",]


# C6
combined_dataC6_312_second <- lapply(C6_312_second, read_csv) %>% 
  bind_rows()
combined_dataC6_312_second <- data.frame(combined_dataC6_312_second)
combined_dataC6_312_second <-  combined_dataC6_312_second %>% 
  add_column(randomisation_column = "C6_312_second")
combined_dataC2_231_second <- combined_dataC2_231_second[combined_dataC2_231_second$Event.Index != "END OF FILE",]


all_columns_combined <- rbind.data.frame(combined_dataC1_123_second, combined_dataC2_231_second, combined_dataC4_213_second, combined_dataC5_132_second, combined_dataC3_321, combined_dataC5_132)
# using tidyverse
# combined_dataC1_231 <- combined_dataC1_123 %>%
#   dplyr:: filter(`Event Index` != "END OF FILE")

# This line exports your combined data as a CSV. This new CSV will be called "combined_data.csv" and will appear in your working directory
write_csv(all_columns_combined,"all_columns_combined.csv")


df_subset_response <- subset(all_columns_combined, Response.Type == "response")

df_subset_response$Emotions <- grepl("-N\\.jpg$", df$Spreadsheet..stimulus)

df_subset_response <- df_subset_response %>%
  mutate(Emotions = case_when(
    Spreadsheet..stimulus == "-N\\.jpg$ |-N\\.jpeg$" ~ "Neutral",
    Spreadsheet..stimulus == "-H\\.jpg$ |-H\\.jpeg$" ~ "Happy",
    Spreadsheet..stimulus == "-S\\.jpg$ |-S\\.jpeg$" ~ "Sad",
  ))



# Extract the letter before the ".jpg" and create a new column
df_subset_response <- df_subset_response %>%
  mutate(Emotions = str_extract(Spreadsheet..stimulus, "(?<=-)[A-Z](?=\\.jpg$|\\.jpeg$)"))


df_test <- df_subset_response %>% 
  group_by(Spreadsheet..stimulus, Spreadsheet..display, Participant.Public.ID) %>%
  summarise(n = n(), .groups = "drop") 


write_csv(df_test,"checkreps.csv")

# trying to see if all images are in both J and V conditions because they did not have equal numbers so far:

df_SS1 <- read.csv("/Users/marjan/Desktop/pilot_data_prep_analysis/data_exp_137495-v33-2/SS1.csv")

# which images are repeated for V condition 
df_SS1 %>% 
  group_by(stimulus, display) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::spread(display, n, fill = 0) %>%
  dplyr::filter(J == 0, V == 2)

# which images are repeated for Jcondition 
df_SS1 %>% 
  group_by(stimulus, display) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::spread(display, n, fill = 0) %>%
  dplyr::filter(J == 2, V == 1)

df_SS1_count <- df_SS1 %>% 
  group_by(display, stimulus) %>%
  summarise(n = n(), .groups = "drop") 


write_csv(df_SS1_count,"df_SS1_count.csv")

df_subset_valence <- subset(df_subset_response, Screen == "Valence")
df_subset_judg <- subset(df_subset_response, Screen == "Judginess")

df_subset_valence <- data.frame(df_subset_valence)
# # mean and SD per subject to see whether there is a big difference in their answers



df_subset_valence$Response <- as.numeric(df_subset_valence$Response)
df_subset_judg$Response <- as.numeric(df_subset_judg$Response)
# unique(df_subset_valence$Response)
# sum(is.na(df_subset_valence$Response))

df_subset_valence %>%
  group_by(Participant.Public.ID) %>%
  summarise(
    mean = mean(Response, na.rm = TRUE),
    sd = sd(Response, na.rm = TRUE)
  )

df_subset_judg %>%
  group_by(Participant.Public.ID) %>%
  summarise(
    mean = mean(Response, na.rm = TRUE),
    sd = sd(Response, na.rm = TRUE)
  )

# Valence
ggplot(df_subset_valence, aes(x=Participant.Public.ID, y=Response)) +
  geom_jitter(width=0.1, size=1, alpha=0.5) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  labs(title = "valence ratings", x = "Participants with high social anxiety", y = "Ratings (0-100)") 
 
# Judginess
ggplot(df_subset_judg, aes(x=Participant.Public.ID, y=Response)) +
  geom_jitter(width=0.1, size=1, alpha=0.5) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  labs(title = "Judginess ratings", x = "Participants with high social anxiety", y = "Ratings (0-100)") 
