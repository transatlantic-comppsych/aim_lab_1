
library(ggplot2)
library(dplyr)
library(tidyr)


final_df14 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df14_with_stresslevels.csv")
final_df15 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df15_with_stresslevels.csv")
final_df16 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df16_with_stresslevels.csv")
final_df17 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df17_with_stresslevels.csv")
final_df18 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df18_with_stresslevels.csv")

final_df14$pilot_nr <- 14
final_df15$pilot_nr <- 15
final_df16$pilot_nr <- 16
final_df17$pilot_nr <- 17
final_df18$pilot_nr <- 18

combined_df <- bind_rows(final_df14, final_df15, final_df16, final_df17, final_df18)

filter_combined_df <- subset(combined_df, combined_df$Trial.Number == 1)

write.csv(filter_combined_df, "stresslevels_p14_p18.csv", row.names = FALSE)

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



ggplot(correlations_df, aes(x = Trial.Number, y = correlation, group = pilot_nr, color = as.factor(pilot_nr))) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Trial Number", y = "Correlation histogram and prediction", color = "Pilot Number") +
  ggtitle("Correlation between Prediction and Histogram over time and per pilot")+
  stat_cor(aes(color = pilot_nr), label.x = 3)


overall_correlation_df <- combined_df %>%
  group_by(pilot_nr) %>%
  summarise(overall_correlation = cor(Response_pred, Response_m_hist, use = "complete.obs")) %>%
  ungroup()

annotated_df <- annotated_df %>%
  left_join(overall_correlation_df, by = "pilot_nr")

correlation_with_trial_df <- annotated_df %>%
  group_by(pilot_nr) %>%
  summarise(correlation_with_trial = cor(Trial.Number, correlation, use = "complete.obs")) %>%
  ungroup()

annotated_df <- annotated_df %>%
  left_join(correlation_with_trial_df, by = "pilot_nr")


ggplot(annotated_df, aes(x = Trial.Number, y = correlation, group = pilot_nr, color = as.factor(pilot_nr))) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, aes(fill = as.factor(pilot_nr))) +
  geom_text(data = annotated_df %>% group_by(pilot_nr) %>% summarize(max_trial = max(Trial.Number), last_correlation = last(correlation), correlation_with_trial = first(correlation_with_trial)), 
            aes(x = max_trial, y = last_correlation, label = sprintf("Corr: %.2f", correlation_with_trial)), 
            hjust = 3, vjust = 0, check_overlap = TRUE, color = "black") +
  theme_minimal() +
  labs(x = "Trial Number", y = "Correlation histogram and prediction", color = "Pilot Number") +
  ggtitle("Correlation between Prediction and Histogram over time and per pilot")
  theme(legend.position = "bottom")




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


