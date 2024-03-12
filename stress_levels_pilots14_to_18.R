
library(ggplot2)
library(dplyr)
library(tidyr)


final_df14 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df14_with_stresslevels.csv")
final_df15 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df15_with_stresslevels.csv")
final_df16 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df16_with_stresslevels.csv")
final_df17 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df17_with_stresslevels.csv")
final_df18 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df18_with_stresslevels.csv")
final_df19 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df19_with_stresslevels.csv")

final_df14$pilot_nr <- 14
final_df15$pilot_nr <- 15
final_df16$pilot_nr <- 16
final_df17$pilot_nr <- 17
final_df18$pilot_nr <- 18
final_df19$pilot_nr <- 19

combined_df <- bind_rows(final_df14, final_df15, final_df16, final_df17, final_df18, final_df19)

filter_combined_df <- subset(combined_df, combined_df$Trial.Number == 1)

write.csv(filter_combined_df, "stresslevels_p14_p19.csv", row.names = FALSE)

stress_level_plots <- ggplot(filter_combined_df, aes(x = Social_Anxiety, y = Response_stress_level , fill = Social_Anxiety)) +
  geom_boxplot(outlier.colour = "black") +
  stat_boxplot(geom = "errorbar",
               width = 0.15) + 
  geom_point(color = "black", position = position_jitter(width = 0.2), alpha = 0.7) +
  theme_minimal() +
  # labs(x = "Display", y = "Response") +
  # scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  # ggtitle("Average anxiety ratings in pilots 1 and 2 in socially anxious people") +
  xlab("") + 
  ylab("stress levels (0-10)") +
  facet_wrap(~pilot_nr, scales = "free") +
  # scale_fill_manual(values = c("black", "purple", "darkgreen", "blue"))+
  # ylim(c(50, 100)) + 
  # scale_y_continuous(breaks = seq(50, 100, by = 10), limits = c(50, 100))+
  theme(
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank(),  # Removes minor grid lines
    panel.border = element_blank(),      # Removes the border line
    axis.line = element_blank(),          # Removes axis lines
    plot.background = element_rect(fill = "white", colour = NA)   # Ensure plot background is white
  )

# Save plot to file with 300 dpi resolution
ggsave(filename = "Social_stress_levels.png", plot = stress_level_plots, dpi = 300, width = 9, height = 9)



filter_combined_df_SA <- subset(filter_combined_df, Social_Anxiety == "high")

df_chocking <- subset(filter_combined_df_SA, pilot_nr == 17 | pilot_nr == 18)
df_nonchocking <- subset(filter_combined_df_SA, pilot_nr == 15 | pilot_nr == 16)

hist(df_chocking$Response_stress_level)
hist(df_nonchocking$Response_stress_level)

shapiro.test(df_chocking$Response_stress_level)
shapiro.test(df_nonchocking$Response_stress_level)

wilcox.test(df_chocking$Response_stress_level, df_nonchocking$Response_stress_leve, alternative = "two.sided")

# t.test(df_chocking$Response_stress_level, df_nonchocking$Response_stress_level, alternative = "two.sided", var.equal = FALSE)

  
  
# I will do a meditation analysis to see how confidence/certainty rating influences the relationship between anxiety and subjPE:
library(mediation)
mediator_model <- lm(Response_certainty ~ Response_SubjPE, data = filter_combined_df)
outcome_model <- lm(Response_Ax ~ Response_SubjPE + Response_certainty, data = filter_combined_df)

med_analysis <- mediate(mediator_model, outcome_model, treat = "Response_SubjPE", mediator = "Response_certainty")

summary(med_analysis)
# If the ACME is significant, it indicates that confidence is a significant mediator in the relationship between SubjPE and Anxiety, which is not the case here.


mediator_model <- lm(Response_certainty ~ Response_SubjPE, data = filter_combined_df)
outcome_model <- lm(Response_H ~ Response_SubjPE + Response_certainty, data = filter_combined_df)

med_analysis <- mediate(mediator_model, outcome_model, treat = "Response_SubjPE", mediator = "Response_certainty")

summary(med_analysis)


# I will now look at the histogram and prediction correlation over time per pilot
# ________________________________________________________________________________
library(ggpubr)
library(broom)

correlations_df <- combined_df %>%
  group_by(pilot_nr, Trial.Number) %>%
  summarise(correlation = cor(Response_pred, Response_m_hist, use = "complete.obs")) %>%
  ungroup() 

correlations_df$pilot_nr <- as.factor(correlations_df$pilot_nr)

ggplot(correlations_df, aes(x = Trial.Number, y = correlation, group = pilot_nr, color = as.factor(pilot_nr))) +
  geom_line() +
  geom_smooth(method = "lm", se = TRUE, aes(fill = as.factor(pilot_nr))) +
  geom_point() +
  theme_minimal() +
  labs(x = "Trial Number", y = "Correlation histogram and prediction", color = "Pilot Number") +
  ggtitle("Correlation between Prediction and Histogram over time and per pilot")+
  stat_cor(aes(color = pilot_nr), label.x = 3)
  # stat_cor(aes(color = pilot_nr), label.x = 3, label.y = max(correlations_df$correlation, na.rm = TRUE) * 1)



# Does certainty rating changes across time

combined_df$pilot_nr <- as.factor(combined_df$pilot_nr)

conf_overtime <- ggplot(combined_df, aes(x = Trial.Number, y = Response_certainty, group = pilot_nr, color = as.factor(pilot_nr))) +
  # geom_line() +
  geom_smooth(method = "lm", se = TRUE, aes(fill = as.factor(pilot_nr))) +
  # geom_point() +
  theme_minimal() +
  labs(x = "Trial Number", y = "Certainty rating 0-100", color = "Pilot Number") +
  ggtitle("Certainty rating over time and per pilot")+
stat_cor(aes(color = pilot_nr), label.x = 3)+
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100))

ggsave(filename = "conf_overtime_p14-18.png", plot = conf_overtime, dpi = 300, width = 9, height = 9)





new_df <- combined_df %>%
  group_by(Random_ID, pilot_nr) %>%
  summarise(mean_confidence = mean(Response_certainty, na.rm = TRUE)) %>%
  ungroup()

mean_conf <- ggplot(new_df, aes(x = pilot_nr, y = mean_confidence)) +
  geom_boxplot(outlier.colour = "black") +
  stat_boxplot(geom = "errorbar",
               width = 0.15) + 
  geom_point(color = "black", position = position_jitter(width = 0.2), alpha = 0.7) +
  theme_minimal() +
  # labs(x = "Display", y = "Response") +
  # scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  # ggtitle("Average anxiety ratings in pilots 1 and 2 in socially anxious people") +
  # xlab("") + 
  ylab("Certainty rating 0-100") +
  # facet_wrap(~pilot_nr, scales = "free") +
  # scale_fill_manual(values = c("black", "purple", "darkgreen", "blue"))+
  # ylim(c(50, 100)) + 
  # scale_y_continuous(breaks = seq(50, 100, by = 10), limits = c(50, 100))+
  theme(
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank(),  # Removes minor grid lines
    panel.border = element_blank(),      # Removes the border line
    axis.line = element_blank(),          # Removes axis lines
    plot.background = element_rect(fill = "white", colour = NA)   # Ensure plot background is white
  )

# Save plot to file with 300 dpi resolution
ggsave(filename = "confidence_ratings_p14-18.png", plot = mean_conf, dpi = 300, width = 9, height = 9)



# does it correlate with PE?





# comparing stress level across pilots
# ____________________________________#

library(lme4)
library(parameters)
library(broom.mixed)

model1 <- lm(Response_stress_level ~ pilot_nr*Social_Anxiety, data = filter_combined_df)
AIC(model1)
summary(model1)

model2 <- lm(Response_stress_level ~ pilot_nr, data = filter_combined_df)
AIC(model2)
summary(model2)

model3 <- lm(Response_stress_level ~ pilot_nr + Social_Anxiety, data = filter_combined_df)
AIC(model3)
summary(model3)

# cant really use LME's since we have only one measurement?
# model1 <- lme4::lmer(Response_stress_level ~ pilot_nr*Social_Anxiety + (1 | Random_ID), data = filter_combined_df, control=lmerControl(optimizer="bobyqa"))




