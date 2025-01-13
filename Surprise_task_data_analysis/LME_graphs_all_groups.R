#load data and combine 4 datasets
final_df_students <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/Surprise_task_data_analysis/final_df_students_with_stresslevels.csv")
final_df_18_25_Pro <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/Surprise_task_data_analysis/final_df_18_25_Pro_with_stresslevels.csv")
final_df_18_25_com <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/Surprise_task_data_analysis/final_df_18_25_com_with_stresslevels.csv")
final_df_26_45_Pro <- read.csv("/Users/elenabagdades/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/GitHub/aim_lab_1/Surprise_task_data_analysis/final_df_26_45_Pro_with_stresslevels.csv")



final_df_18_25_com<- rename(final_df_18_25_com, "Random_ID" =  "Random_ID_new")
final_df_students<- rename(final_df_students, "Random_ID" =  "Random_ID_new")

#anxiety models 
#students
model_students <- lme4::lmer(Response_Ax ~ Response_SubjPE + Social_Anxiety + (Response_SubjPE | Random_ID), data = final_df_students, control=lmerControl(optimizer="bobyqa"))
coef_fixed_students <- fixef(model_students)
intercept_Response_Ax_students <- coef_fixed_students[1]
slope_Response_Ax_students <- coef_fixed_students[2]
standard_beta_Response_SubjPE_students <- parameters:: standardise_parameters (model_students)

#standard errors for intercept and slope
CI_multiplier <- 1.96  # 95% CI multiplier for a normal distribution
# Extract the standard errors for intercept and slope from the model summary
model_summary_students <- summary(model_students)
SE_intercept_students <- model_summary_students$coefficients[1, "Std. Error"]  # Standard error for the intercept
SE_slope_students <- model_summary_students$coefficients[2, "Std. Error"]      # Standard error for the slope



# Creating a sequence of `SubjPE` values for prediction
Response_SubjPE_range_students <- seq(min(final_df_students$Response_SubjPE), max(final_df_students$Response_SubjPE), length.out = 100)

# Calculating predicted Response_Ax and its CI bounds across the SubjPE range
predicted_Response_Ax_students <- intercept_Response_Ax_students + slope_Response_Ax_students * Response_SubjPE_range_students
CI_lower_students <- predicted_Response_Ax_students - CI_multiplier * sqrt(SE_intercept_students^2 + (Response_SubjPE_range_students^2 * SE_slope_students^2))
CI_upper_students <- predicted_Response_Ax_students + CI_multiplier * sqrt(SE_intercept_students^2 + (Response_SubjPE_range_students^2 * SE_slope_students^2))

# Preparing a data frame for ggplot
CI_data_students <- data.frame(Response_SubjPE=Response_SubjPE_range_students, CI_lower=CI_lower_students, CI_upper=CI_upper_students)

#18-25 prolific
model_pro_18_25 <- lme4::lmer(Response_Ax ~ Response_SubjPE + Social_Anxiety + (Response_SubjPE | Random_ID), data = final_df_18_25_Pro, control=lmerControl(optimizer="bobyqa"))
coef_fixed_pro_18_25 <- fixef(model_pro_18_25)
intercept_Response_Ax_pro_18_25 <- coef_fixed_pro_18_25[1]
slope_Response_Ax_pro_18_25 <- coef_fixed_pro_18_25[2]
standard_beta_Response_SubjPE_pro_18_25<- parameters:: standardise_parameters (model_pro_18_25)

#standard errors for intercept and slope
CI_multiplier <- 1.96  # 95% CI multiplier for a normal distribution
# Extract the standard errors for intercept and slope from the model summary
model_summary_pro_18_25 <- summary(model_pro_18_25)
SE_intercept_pro_18_25 <- model_summary_pro_18_25$coefficients[1, "Std. Error"]  # Standard error for the intercept
SE_slope_pro_18_25 <- model_summary_pro_18_25$coefficients[2, "Std. Error"]      # Standard error for the slope



# Creating a sequence of `SubjPE` values for prediction
Response_SubjPE_range_pro_18_25 <- seq(min(final_df_18_25_Pro$Response_SubjPE), max(final_df_18_25_Pro$Response_SubjPE), length.out = 100)

# Calculating predicted Response_Ax and its CI bounds across the SubjPE range
predicted_Response_Ax_pro_18_25 <- intercept_Response_Ax_pro_18_25 + slope_Response_Ax_pro_18_25 * Response_SubjPE_range_pro_18_25
CI_lower_pro_18_25 <- predicted_Response_Ax_pro_18_25 - CI_multiplier * sqrt(SE_intercept_pro_18_25^2 + (Response_SubjPE_range_pro_18_25^2 * SE_slope_pro_18_25^2))
CI_upper_pro_18_25 <- predicted_Response_Ax_pro_18_25 + CI_multiplier * sqrt(SE_intercept_pro_18_25^2 + (Response_SubjPE_range_pro_18_25^2 * SE_slope_pro_18_25^2))

# Preparing a data frame for ggplot
CI_data_pro_18_25 <- data.frame(Response_SubjPE=Response_SubjPE_range_pro_18_25, CI_lower=CI_lower_pro_18_25, CI_upper=CI_upper_pro_18_25)

#18-25 community

model_com_18_25 <- lme4::lmer(Response_Ax ~ Response_SubjPE + Social_Anxiety + (Response_SubjPE | Random_ID), data = final_df_18_25_com, control=lmerControl(optimizer="bobyqa"))
coef_fixed_com_18_25 <- fixef(model_com_18_25)
intercept_Response_Ax_com_18_25 <- coef_fixed_com_18_25[1]
slope_Response_Ax_com_18_25 <- coef_fixed_com_18_25[2]
standard_beta_Response_SubjPE_com_18_25 <- parameters::standardise_parameters(model_com_18_25)

#standard errors for intercept and slope
CI_multiplier <- 1.96  # 95% CI multiplier for a normal distribution
# Extract the standard errors for intercept and slope from the model summary
model_summary_com_18_25 <- summary(model_com_18_25)
SE_intercept_com_18_25 <- model_summary_com_18_25$coefficients[1, "Std. Error"]  # Standard error for the intercept
SE_slope_com_18_25 <- model_summary_com_18_25$coefficients[2, "Std. Error"]      # Standard error for the slope

# Creating a sequence of `SubjPE` values for prediction
Response_SubjPE_range_com_18_25 <- seq(min(final_df_18_25_com$Response_SubjPE), max(final_df_18_25_com$Response_SubjPE), length.out = 100)

# Calculating predicted Response_Ax and its CI bounds across the SubjPE range
predicted_Response_Ax_com_18_25 <- intercept_Response_Ax_com_18_25 + slope_Response_Ax_com_18_25 * Response_SubjPE_range_com_18_25
CI_lower_com_18_25 <- predicted_Response_Ax_com_18_25 - CI_multiplier * sqrt(SE_intercept_com_18_25^2 + (Response_SubjPE_range_com_18_25^2 * SE_slope_com_18_25^2))
CI_upper_com_18_25 <- predicted_Response_Ax_com_18_25 + CI_multiplier * sqrt(SE_intercept_com_18_25^2 + (Response_SubjPE_range_com_18_25^2 * SE_slope_com_18_25^2))

# Preparing a data frame for ggplot
CI_data_com_18_25 <- data.frame(Response_SubjPE=Response_SubjPE_range_com_18_25, CI_lower=CI_lower_com_18_25, CI_upper=CI_upper_com_18_25)

#26-45 pro
model_pro_26_45 <- lme4::lmer(Response_Ax ~ Response_SubjPE + Social_Anxiety + (Response_SubjPE | Random_ID), data = final_df_26_45_Pro, control=lmerControl(optimizer="bobyqa"))
coef_fixed_pro_26_45 <- fixef(model_pro_26_45)
intercept_Response_Ax_pro_26_45 <- coef_fixed_pro_26_45[1]
slope_Response_Ax_pro_26_45 <- coef_fixed_pro_26_45[2]
standard_beta_Response_SubjPE_pro_26_45 <- parameters::standardise_parameters(model_pro_26_45)

#standard errors for intercept and slope
CI_multiplier <- 1.96  # 95% CI multiplier for a normal distribution
# Extract the standard errors for intercept and slope from the model summary
model_summary_pro_26_45 <- summary(model_pro_26_45)
SE_intercept_pro_26_45 <- model_summary_pro_26_45$coefficients[1, "Std. Error"]  # Standard error for the intercept
SE_slope_pro_26_45 <- model_summary_pro_26_45$coefficients[2, "Std. Error"]      # Standard error for the slope

# Creating a sequence of `SubjPE` values for prediction
Response_SubjPE_range_pro_26_45 <- seq(min(final_df_26_45_Pro$Response_SubjPE), max(final_df_26_45_Pro$Response_SubjPE), length.out = 100)

# Calculating predicted Response_Ax and its CI bounds across the SubjPE range
predicted_Response_Ax_pro_26_45 <- intercept_Response_Ax_pro_26_45 + slope_Response_Ax_pro_26_45 * Response_SubjPE_range_pro_26_45
CI_lower_pro_26_45 <- predicted_Response_Ax_pro_26_45 - CI_multiplier * sqrt(SE_intercept_pro_26_45^2 + (Response_SubjPE_range_pro_26_45^2 * SE_slope_pro_26_45^2))
CI_upper_pro_26_45 <- predicted_Response_Ax_pro_26_45 + CI_multiplier * sqrt(SE_intercept_pro_26_45^2 + (Response_SubjPE_range_pro_26_45^2 * SE_slope_pro_26_45^2))

# Preparing a data frame for ggplot
CI_data_pro_26_45 <- data.frame(Response_SubjPE=Response_SubjPE_range_pro_26_45, CI_lower=CI_lower_pro_26_45, CI_upper=CI_upper_pro_26_45)



library(ggplot2)
library(dplyr)

# Combine data into a single data frame
CI_data_students$Group <- "Students"
CI_data_pro_18_25$Group <- "Prolific 18-25"
CI_data_com_18_25$Group <- "Community 18-25"
CI_data_pro_26_45$Group <- "Prolific 26-45"

# Bind all datasets together
CI_data_all <- bind_rows(CI_data_students, CI_data_pro_18_25, CI_data_com_18_25, CI_data_pro_26_45)

# Create the ribbon plot
ggplot(CI_data_all, aes(x = Response_SubjPE, ymin = CI_lower, ymax = CI_upper, fill = Group)) +
  geom_ribbon(alpha = 0.3) +
  geom_line(aes(y = (CI_lower + CI_upper) / 2, color = Group), size = 1) +  # Mean line
  theme_minimal() +
  labs(title = "Confidence Intervals of Anxiety by Group",
       x = "PE",
       y = "Anxiety",
       fill = "Group",
       color = "Group") +
  theme(legend.position = "top")




#MOOD MODELS
#students
model_students <- lme4::lmer(Response_H ~ Response_SubjPE + Social_Anxiety + (Response_SubjPE | Random_ID), data = final_df_students, control=lmerControl(optimizer="bobyqa"))
coef_fixed_students <- fixef(model_students)
intercept_Response_H_students <- coef_fixed_students[1]
slope_Response_H_students <- coef_fixed_students[2]
standard_beta_Response_SubjPE_students <- parameters:: standardise_parameters (model_students)

#standard errors for intercept and slope
CI_multiplier <- 1.96  # 95% CI multiplier for a normal distribution
# Extract the standard errors for intercept and slope from the model summary
model_summary_students <- summary(model_students)
SE_intercept_students <- model_summary_students$coefficients[1, "Std. Error"]  # Standard error for the intercept
SE_slope_students <- model_summary_students$coefficients[2, "Std. Error"]      # Standard error for the slope

# Creating a sequence of `SubjPE` values for prediction
Response_SubjPE_range_students <- seq(min(final_df_students$Response_SubjPE), max(final_df_students$Response_SubjPE), length.out = 100)

# Calculating predicted Response_H and its CI bounds across the SubjPE range
predicted_Response_H_students <- intercept_Response_H_students + slope_Response_H_students * Response_SubjPE_range_students
CI_lower_students <- predicted_Response_H_students - CI_multiplier * sqrt(SE_intercept_students^2 + (Response_SubjPE_range_students^2 * SE_slope_students^2))
CI_upper_students <- predicted_Response_H_students + CI_multiplier * sqrt(SE_intercept_students^2 + (Response_SubjPE_range_students^2 * SE_slope_students^2))

# Preparing a data frame for ggplot
CI_data_students <- data.frame(Response_SubjPE=Response_SubjPE_range_students, CI_lower=CI_lower_students, CI_upper=CI_upper_students)

#18-25 prolific
model_pro_18_25 <- lme4::lmer(Response_H ~ Response_SubjPE + Social_Anxiety + (Response_SubjPE | Random_ID), data = final_df_18_25_Pro, control=lmerControl(optimizer="bobyqa"))
coef_fixed_pro_18_25 <- fixef(model_pro_18_25)
intercept_Response_H_pro_18_25 <- coef_fixed_pro_18_25[1]
slope_Response_H_pro_18_25 <- coef_fixed_pro_18_25[2]
standard_beta_Response_SubjPE_pro_18_25<- parameters:: standardise_parameters (model_pro_18_25)

#standard errors for intercept and slope
CI_multiplier <- 1.96  # 95% CI multiplier for a normal distribution
# Extract the standard errors for intercept and slope from the model summary
model_summary_pro_18_25 <- summary(model_pro_18_25)
SE_intercept_pro_18_25 <- model_summary_pro_18_25$coefficients[1, "Std. Error"]  # Standard error for the intercept
SE_slope_pro_18_25 <- model_summary_pro_18_25$coefficients[2, "Std. Error"]      # Standard error for the slope

# Creating a sequence of `SubjPE` values for prediction
Response_SubjPE_range_pro_18_25 <- seq(min(final_df_18_25_Pro$Response_SubjPE), max(final_df_18_25_Pro$Response_SubjPE), length.out = 100)

# Calculating predicted Response_H and its CI bounds across the SubjPE range
predicted_Response_H_pro_18_25 <- intercept_Response_H_pro_18_25 + slope_Response_H_pro_18_25 * Response_SubjPE_range_pro_18_25
CI_lower_pro_18_25 <- predicted_Response_H_pro_18_25 - CI_multiplier * sqrt(SE_intercept_pro_18_25^2 + (Response_SubjPE_range_pro_18_25^2 * SE_slope_pro_18_25^2))
CI_upper_pro_18_25 <- predicted_Response_H_pro_18_25 + CI_multiplier * sqrt(SE_intercept_pro_18_25^2 + (Response_SubjPE_range_pro_18_25^2 * SE_slope_pro_18_25^2))

# Preparing a data frame for ggplot
CI_data_pro_18_25 <- data.frame(Response_SubjPE=Response_SubjPE_range_pro_18_25, CI_lower=CI_lower_pro_18_25, CI_upper=CI_upper_pro_18_25)

#18-25 com
model_com_18_25 <- lme4::lmer(Response_H ~ Response_SubjPE + Social_Anxiety + (Response_SubjPE | Random_ID), data = final_df_18_25_com, control=lmerControl(optimizer="bobyqa"))
coef_fixed_com_18_25 <- fixef(model_com_18_25)
intercept_Response_H_com_18_25 <- coef_fixed_com_18_25[1]
slope_Response_H_com_18_25 <- coef_fixed_com_18_25[2]
standard_beta_Response_SubjPE_com_18_25 <- parameters::standardise_parameters(model_com_18_25)

#standard errors for intercept and slope
CI_multiplier <- 1.96  # 95% CI multiplier for a normal distribution
# Extract the standard errors for intercept and slope from the model summary
model_summary_com_18_25 <- summary(model_com_18_25)
SE_intercept_com_18_25 <- model_summary_com_18_25$coefficients[1, "Std. Error"]  # Standard error for the intercept
SE_slope_com_18_25 <- model_summary_com_18_25$coefficients[2, "Std. Error"]      # Standard error for the slope

# Creating a sequence of `SubjPE` values for prediction
Response_SubjPE_range_com_18_25 <- seq(min(final_df_18_25_com$Response_SubjPE), max(final_df_18_25_com$Response_SubjPE), length.out = 100)

# Calculating predicted Response_H and its CI bounds across the SubjPE range
predicted_Response_H_com_18_25 <- intercept_Response_H_com_18_25 + slope_Response_H_com_18_25 * Response_SubjPE_range_com_18_25
CI_lower_com_18_25 <- predicted_Response_H_com_18_25 - CI_multiplier * sqrt(SE_intercept_com_18_25^2 + (Response_SubjPE_range_com_18_25^2 * SE_slope_com_18_25^2))
CI_upper_com_18_25 <- predicted_Response_H_com_18_25 + CI_multiplier * sqrt(SE_intercept_com_18_25^2 + (Response_SubjPE_range_com_18_25^2 * SE_slope_com_18_25^2))

# Preparing a data frame for ggplot
CI_data_com_18_25 <- data.frame(Response_SubjPE=Response_SubjPE_range_com_18_25, CI_lower=CI_lower_com_18_25, CI_upper=CI_upper_com_18_25)

#26-45 pro
model_pro_26_45 <- lme4::lmer(Response_H ~ Response_SubjPE + Social_Anxiety + (Response_SubjPE | Random_ID), data = final_df_26_45_Pro, control=lmerControl(optimizer="bobyqa"))
coef_fixed_pro_26_45 <- fixef(model_pro_26_45)
intercept_Response_H_pro_26_45 <- coef_fixed_pro_26_45[1]
slope_Response_H_pro_26_45 <- coef_fixed_pro_26_45[2]
standard_beta_Response_SubjPE_pro_26_45 <- parameters::standardise_parameters(model_pro_26_45)

#standard errors for intercept and slope
CI_multiplier <- 1.96  # 95% CI multiplier for a normal distribution
# Extract the standard errors for intercept and slope from the model summary
model_summary_pro_26_45 <- summary(model_pro_26_45)
SE_intercept_pro_26_45 <- model_summary_pro_26_45$coefficients[1, "Std. Error"]  # Standard error for the intercept
SE_slope_pro_26_45 <- model_summary_pro_26_45$coefficients[2, "Std. Error"]      # Standard error for the slope

# Creating a sequence of `SubjPE` values for prediction
Response_SubjPE_range_pro_26_45 <- seq(min(final_df_26_45_Pro$Response_SubjPE), max(final_df_26_45_Pro$Response_SubjPE), length.out = 100)

# Calculating predicted Response_H and its CI bounds across the SubjPE range
predicted_Response_H_pro_26_45 <- intercept_Response_H_pro_26_45 + slope_Response_H_pro_26_45 * Response_SubjPE_range_pro_26_45
CI_lower_pro_26_45 <- predicted_Response_H_pro_26_45 - CI_multiplier * sqrt(SE_intercept_pro_26_45^2 + (Response_SubjPE_range_pro_26_45^2 * SE_slope_pro_26_45^2))
CI_upper_pro_26_45 <- predicted_Response_H_pro_26_45 + CI_multiplier * sqrt(SE_intercept_pro_26_45^2 + (Response_SubjPE_range_pro_26_45^2 * SE_slope_pro_26_45^2))

# Preparing a data frame for ggplot
CI_data_pro_26_45 <- data.frame(Response_SubjPE=Response_SubjPE_range_pro_26_45, CI_lower=CI_lower_pro_26_45, CI_upper=CI_upper_pro_26_45)


library(ggplot2)
library(dplyr)

# Combine data into a single data frame
CI_data_students$Group <- "Students"
CI_data_pro_18_25$Group <- "Prolific 18-25"
CI_data_com_18_25$Group <- "Community 18-25"
CI_data_pro_26_45$Group <- "Prolific 26-45"

# Bind all datasets together
CI_data_all <- bind_rows(CI_data_students, CI_data_pro_18_25, CI_data_com_18_25, CI_data_pro_26_45)

# Create the ribbon plot
ggplot(CI_data_all, aes(x = Response_SubjPE, ymin = CI_lower, ymax = CI_upper, fill = Group)) +
  geom_ribbon(alpha = 0.3) +
  geom_line(aes(y = (CI_lower + CI_upper) / 2, color = Group), size = 1) +  # Mean line
  theme_minimal() +
  labs(title = "Confidence Intervals of Mood by Group",
       x = "PE",
       y = "Mood",
       fill = "Group",
       color = "Group") +
  theme(legend.position = "top")



