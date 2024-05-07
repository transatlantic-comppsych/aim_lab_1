
library(ggplot2)
library(dplyr)
library(tidyr)


final_df14 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df14_with_stresslevels.csv")
final_df15 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df15_with_stresslevels.csv")
final_df16 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df16_with_stresslevels.csv")
final_df17 <- read.csv("/Users/marjan/Desktop/aim_lab_1/final_df17_with_stresslevels.csv")

final_df14$pilot_nr <- 14
final_df15$pilot_nr <- 15
final_df16$pilot_nr <- 16
final_df17$pilot_nr <- 17

combined_df <- bind_rows(final_df14, final_df15, final_df16, final_df17)

filter_combined_df <- subset(combined_df, combined_df$Trial.Number == 1)

write.csv(filter_combined_df, "stresslevels_p14_p17.csv", row.names = FALSE)

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

df16_raw <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_vid_big_PE_narr2_early_pred/SUP_PRF_pilot_vid_big_PE_narr2_early_pred_v2/SUP_PRF_pilot_vid_big_PE_narr2_early_pred_v2_task_main.csv")


