df_15 <- read.csv("C:/Users/Elena/Downloads/copy_data_exp_165975-v4_2/data_exp_165975-v4_task-6wec.csv")
df_15 <- df_15 %>% dplyr::filter (Screen.ID == "bxnzks")
df_15 <- df_15 %>% dplyr::filter (Response.Type == "tooEarly")
df_15$Response<-  as.numeric(df_15$Response)
df_15_stress_mean <- mean(df_15$Response)
df_15$pilot <- 15
df_15_mini_spin <- read.csv("C:/Users/Elena/Downloads/copy_data_exp_165975-v4_2/data_exp_165975-v4_questionnaire-4h2a.csv")
df_15_mini_spin <- df_15_mini_spin %>%
  rename(
    "Q1" =  "Rating.Scale.object.2.Fear.of.embarrassment.causes.me.to.avoid.doing.things.or.speaking.to.people.",
    "Q2" =  "Rating.Scale.object.2.I.avoid.activities.in.which.I.am.the.center.of.attention.",
    "Q3" = "Rating.Scale.object.2.Being.embarrassed.or.looking.stupid.are.among.my.worst.fears."
  )
df_15_mini_spin <- df_15_mini_spin[-1,]
df_15_mini_spin <- df_15_mini_spin[-nrow(df_15_mini_spin),]
df_15_mini_spin <- df_15_mini_spin %>%
  rowwise() %>%
  mutate_at(vars(Q1, Q2, Q3), as.numeric) %>%
  mutate(mini_SPIN_total = sum(Q1, Q2, Q3, na.rm = TRUE)) %>%
  ungroup()

df_15_mini_spin <- df_15_mini_spin %>%
  rowwise() %>%
  mutate(mini_SPIN_total = sum(c(Q1, Q2, Q3), na.rm = TRUE)) %>%
  ungroup()
df_15 <- left_join(df_15, df_15_mini_spin, by= "Participant.Public.ID")
df_15_LSAS<- read.csv("C:/Users/Elena/Downloads/copy_data_exp_165975-v4_2/data_exp_165975-v4_questionnaire-xzz2.csv")
df_15_LSAS <- df_15_LSAS[-1,]
df_15_LSAS <- df_15_LSAS[-nrow(df_15_LSAS),]
df_15_LSAS <- df_15_LSAS[, c(12, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52,
                             54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74,
                             76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98,
                             100, 102, 104, 106, 108, 110, 112, 114, 116, 118,120,
                             122, 124, 126)]
non_id_cols <- setdiff(names(df_15_LSAS), "Participant.Public.ID")

# Convert non-ID variables to numeric
df_15_LSAS[, non_id_cols] <- lapply(df_15_LSAS[, non_id_cols], as.numeric)

LSAS_scores <-  numeric(length(df_15_LSAS$Participant.Public.ID))

LSAS_scores <- 0
Questions <- c(2:49)

for (i in 1:nrow(df_15_LSAS)) {
  LSAS_score <- sum(df_15_LSAS[i, Questions])
  LSAS_scores[i] <- LSAS_score
}


df_15_LSAS$LSAS_score <- LSAS_scores

df_15_LSAS$SAD<- 0

#Assign "1" if they meet SAD threshold 
for (i in 1:nrow(df_15_LSAS)) {
  if (df_15_LSAS$LSAS_score[i] >= 30) {
    df_15_LSAS$SAD[i] <- 1
  }
}

df_15 <- left_join(df_15,df_15_LSAS[, c("Participant.Public.ID", "LSAS_score", "SAD")], by = "Participant.Public.ID")
df_15 <- subset(df_15, select =  c("Response", "pilot", "mini_SPIN_total", "LSAS_score", "SAD"))

df_16 <- read.csv("C:/Users/Elena/Downloads/copy_data_exp_166635-v2/SUP_PRF_pilot_vid_big_PE_narr2_early_pred_v2_task_main.csv")
df_16 <- df_16 %>% dplyr::filter (Screen.ID == "bxnzks")
df_16 <- df_16 %>% dplyr::filter (Response.Type == "tooEarly")
df_16$Response<-  as.numeric(df_16$Response)
df_16_stress_meanmean <- (df_16$Response)
df_16$pilot <- 16
df_16_mini_spin <- read.csv("C:/Users/Elena/Downloads/copy_data_exp_166635-v2/SUP_PRF_pilot_vid_big_PE_narr2_early_pred_v2_mini_spin.csv")
df_16_mini_spin <- df_16_mini_spin %>%
  rename(
    "Q1" =  "Rating.Scale.object.2.Fear.of.embarrassment.causes.me.to.avoid.doing.things.or.speaking.to.people.",
    "Q2" =  "Rating.Scale.object.2.I.avoid.activities.in.which.I.am.the.center.of.attention.",
    "Q3" = "Rating.Scale.object.2.Being.embarrassed.or.looking.stupid.are.among.my.worst.fears."
  )
df_16_mini_spin <- df_16_mini_spin %>%
  rowwise() %>%
  mutate_at(vars(Q1, Q2, Q3), as.numeric) %>%
  mutate(mini_SPIN_total = sum(Q1, Q2, Q3, na.rm = TRUE)) %>%
  ungroup()

df_16_mini_spin <- df_16_mini_spin %>%
  rowwise() %>%
  mutate(mini_SPIN_total = sum(c(Q1, Q2, Q3), na.rm = TRUE)) %>%
  ungroup()
df_16 <- left_join(df_16, df_16_mini_spin, by= "Random_ID")
df_16_LSAS <- read.csv("C:/Users/Elena/Downloads/copy_data_exp_166635-v2/SUP_PRF_pilot_vid_big_PE_narr2_early_pred_v2_lsas_sr.csv")
df_16_LSAS <- df_16_LSAS[, c(31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51,
                             53, 55, 57, 59, 61, 63, 65, 67, 69, 71, 73, 75, 
                             77, 79, 81, 83, 85, 87, 89, 91, 93, 94, 95, 97, 99,
                             101, 103, 105, 107, 109, 111, 113, 115, 117, 119,
                             121, 123, 125, 127)]

non_id_cols <- setdiff(names(df_16_LSAS), "Random_ID")

# Convert non-ID variables to numeric
df_16_LSAS[, non_id_cols] <- lapply(df_16_LSAS[, non_id_cols], as.numeric)

LSAS_scores <-  numeric(length(df_16_LSAS$Random_ID))

LSAS_scores <- 0
Questions <- c(1:48)

for (i in 1:nrow(df_16_LSAS)) {
  LSAS_score <- sum(df_16_LSAS[i, Questions])
  LSAS_scores[i] <- LSAS_score
}


df_16_LSAS$LSAS_score <- LSAS_scores

df_16_LSAS$SAD<- 0
  
  #Assign "1" if they meet SAD threshold 
  for (i in 1:nrow(df_16_LSAS)) {
    if (df_16_LSAS$LSAS_score[i] >= 30) {
      df_16_LSAS$SAD[i] <- 1
    }
  }

df_16 <- left_join(df_16,df_16_LSAS[, c("Random_ID", "LSAS_score", "SAD")], by = "Random_ID")
df_16 <- subset(df_16, select =  c("Response", "pilot", "mini_SPIN_total", "LSAS_score", "SAD"))


df_17 <- read.csv("C:/Users/Elena/Downloads/copy_data_exp_167771-v4/SUP_PRF_p17_vid_bigPE_nar2_earlypred_chk_cbal_v4_task_main.csv")
df_17 <- df_17 %>% dplyr::filter (Screen.ID == "bxnzks")
df_17 <- df_17 %>% dplyr::filter (Response.Type == "tooEarly")
df_17$Response<-  as.numeric(df_17$Response)
df_17_stress_mean <- mean(df_17$Response)
df_17$pilot <- 17
df_17_mini_spin <- df17_spin
df_17 <- left_join(df_17, df_17_mini_spin, by= "Random_ID")
df_17_LSAS <- read.csv("C:/Users/Elena/Downloads/copy_data_exp_167771-v4/SUP_PRF_p17_vid_bigPE_nar2_earlypred_chk_cbal_v4_lsas_sr.csv")
df_17_LSAS <- df_17_LSAS[, c(32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52,
                             54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74,
                             76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98,
                             100, 102, 104, 106, 108, 110, 112, 114, 116, 118,120,
                             122, 124, 126, 128)]

non_id_cols <- setdiff(names(df_17_LSAS), "Random_ID")

# Convert non-ID variables to numeric
df_17_LSAS[, non_id_cols] <- lapply(df_17_LSAS[, non_id_cols], as.numeric)

LSAS_scores <-  numeric(length(df_17_LSAS$Random_ID))

LSAS_scores <- 0
Questions <- c(1:48)

for (i in 1:nrow(df_17_LSAS)) {
  LSAS_score <- sum(df_17_LSAS[i, Questions])
  LSAS_scores[i] <- LSAS_score
}


df_17_LSAS$LSAS_score <- LSAS_scores


# Create a new columns SAD and GAD and set default as 'no' 
df_17_LSAS$SAD<- 0
  
  #Assign "1" if they meet SAD threshold 
  for (i in 1:nrow(df_17_LSAS)) {
    if (df_17_LSAS$LSAS_score[i] >= 30) {
      df_17_LSAS$SAD[i] <- 1
    }
  }


df_17 <- left_join(df_17,df_17_LSAS[, c("Random_ID", "LSAS_score", "SAD")], by = "Random_ID")
df_17 <- subset(df_17, select =  c("Response", "pilot", "mini_SPIN_total", "LSAS_score", "SAD"))

merged_df <- rbind(df_15, df_16, df_17)
merged_df$SA <- ifelse(merged_df$mini_SPIN_total >= 6, "SA", "No_SA")

ggplot(merged_df, aes(x = pilot, y = Response)) +
  geom_bar()

mean_value <- mean(merged_df$Response)

ggplot(merged_df, aes(x = pilot, y = Response)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue", width = 0.5) +
  geom_point(color = "blue", size = 3, position = position_jitter(width = 0.1)) +
  geom_hline(yintercept = mean_value, linetype = "dashed", color = "blue") +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", linewidth = 2, size = 1)+
  labs(title = "Reported stress across pilots",
       x = "Pilot",
       y = "Stress level")+  facet_wrap(~ SA, scales = "fixed")


ggplot(merged_df, aes(x = pilot, y = Response)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue", width = 0.5) +
  geom_point(color = "blue", size = 3, position = position_jitter(width = 0.1)) +
  geom_hline(yintercept = mean_value, linetype = "dashed", color = "blue") +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", linewidth = 2, size = 1)+
  labs(title = "Reported stress across pilots",
       x = "Pilot",
       y = "Stress level")+  facet_wrap(~ SAD, scales = "fixed")

correlation_by_pilot <- merged_df %>%
  group_by(pilot) %>%
  summarize(correlation = cor(LSAS_score, Response, use = "complete.obs"))

ggplot(merged_df, aes(x = LSAS_score, y = Response, color = as.factor(SAD))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + # Add a linear regression line
  labs(x = "LSAS Score", y = "Response") +
  ggtitle("Correlation between LSAS Score and Response Grouped by Pilot") +
  facet_wrap(~ pilot, ncol = 1)
