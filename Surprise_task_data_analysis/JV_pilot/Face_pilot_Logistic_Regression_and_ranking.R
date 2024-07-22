library(tidyverse)
library(conflicted)
library(dplyr)
library(ggplot2)
library(stringr)

# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/marjan/Downloads/data_exp_140743-v17-2")

JV_1 <- read.csv("/Users/marjan/Downloads/data_exp_140743-v17-2/JV/data_exp_139282-v42_task-5tah_1.csv")
JV_2 <- read.csv("/Users/marjan/Downloads/data_exp_140743-v17-2/JV/data_exp_139282-v42_task-ak18_1.csv")
JV_3 <- read.csv("/Users/marjan/Downloads/data_exp_140743-v17-2/JV/data_exp_140743-v17_task-5tah_2.csv")
JV_4 <- read.csv("/Users/marjan/Downloads/data_exp_140743-v17-2/JV/data_exp_140743-v17_task-ak18_2.csv")


# Combine the two datasets
JV <- rbind(JV_1, JV_2,JV_3,JV_4)


# extract only the response rows (not continue presses, fixation cross, etc.)
JV_response <- subset(JV_1_2, Response.Type == "response")

# Extract the letter before the ".jpg" or ""jpeg" and create a new column
JV_response <- JV_response %>%
  mutate(Emotion_Types = str_extract(Spreadsheet..stimulus, "(?<=-)[A-Z](?=\\.jpg$|\\.jpeg$)"))


# Remove rows containing sad images
JV_response_noSad <- JV_response[JV_response$Emotion_Types != "S",]

# Keep rows containing only the valence condition
JV_response_noSad_V <- JV_response_noSad[JV_response_noSad$Display == "V",]

# Keep rows containing only the judginess condition
JV_response_noSad_J <- JV_response_noSad[JV_response_noSad$Display == "J",]


# calculate mean and SD of RT per subject, then see which reaction times are bigger or smaller than
# 2SD from the mean within subjects to exclude them.
# ==> Remember to exclude the corresponding J image for the correlation between J and V, otherwise the size will be unbalance.

# Calculate mean and SD for each subject
mean_sd <- aggregate(`Reaction.Time` ~ Participant.Public.ID, data = JV_response_noSad_V, function(x) c(mean = mean(x), sd = sd(x)))

# Function to identify RT outliers for a given subject
find_outliers <- function(subject_id) {
  subject_data <- JV_response_noSad_V[JV_response_noSad_V$Participant.Public.ID == subject_id, "Reaction Time"]
  mean_val <- mean_sd[mean_sd$Participant.Public.ID == subject_id, "Reaction Time"]$mean
  sd_val <- mean_sd[mean_sd$Participant.Public.ID == subject_id, "Reaction Time"]$sd
  outliers <- subject_data < (mean_val - 2 * sd_val) | subject_data > (mean_val + 2 * sd_val)
  return(JV_response_noSad_V[JV_response_noSad_V$Participant.Public.ID == subject_id & outliers, ])
}

# Apply the function to each subject to find trials with RT outliers per subject
outliers <- do.call(rbind, lapply(unique(JV_response_noSad_V$Participant.Public.ID), find_outliers))
# There were 0 outliers for the 45 pilot subjects so far.

# repeat the same thing for judginess condition
# Calculate mean and SD for each subject
mean_sd <- aggregate(`Reaction.Time` ~ Participant.Public.ID, data = JV_response_noSad_J, function(x) c(mean = mean(x), sd = sd(x)))

# Function to identify RT outliers for a given subject
find_outliers <- function(subject_id) {
  subject_data <- JV_response_noSad_J[JV_response_noSad_J$Participant.Public.ID == subject_id, "Reaction Time"]
  mean_val <- mean_sd[mean_sd$Participant.Public.ID == subject_id, "Reaction Time"]$mean
  sd_val <- mean_sd[mean_sd$Participant.Public.ID == subject_id, "Reaction Time"]$sd
  outliers <- subject_data < (mean_val - 2 * sd_val) | subject_data > (mean_val + 2 * sd_val)
  return(JV_response_noSad_J[JV_response_noSad_J$Participant.Public.ID == subject_id & outliers, ])
}

# Apply the function to each subject to find trials with RT outliers per subject
outliers <- do.call(rbind, lapply(unique(JV_response_noSad_J$Participant.Public.ID), find_outliers))
# There were 0 outliers for the 45 pilot subjects so far.


# Calculate mean and SD for each subject
mean_sd <- aggregate(`Reaction.Time` ~ Participant.Public.ID, data = JV_response_noSad_V, function(x) c(mean = mean(x), sd = sd(x)))

# Function to identify RT outliers for a given subject
find_outliers <- function(subject_id) {
  subject_data <- JV_response_noSad_V[JV_response_noSad_V$Participant.Public.ID == subject_id, "Reaction Time"]
  mean_val <- mean_sd[mean_sd$Participant.Public.ID == subject_id, "Reaction Time"]$mean
  sd_val <- mean_sd[mean_sd$Participant.Public.ID == subject_id, "Reaction Time"]$sd
  outliers <- subject_data < (mean_val - 2 * sd_val) | subject_data > (mean_val + 2 * sd_val)
  return(JV_response_noSad_V[JV_response_noSad_V$Participant.Public.ID == subject_id & outliers, ])
}

# Apply the function to each subject to find trials with RT outliers per subject
outliers <- do.call(rbind, lapply(unique(JV_response_noSad_V$Participant.Public.ID), find_outliers))
# There were 0 outliers for the 3 pilot subjects so far.


# Run a logistic regression to predict image types from ratings
# Make sure column with emotion names  is a factor
JV_response_noSad_V$Emotion_Types <- as.factor(JV_response_noSad_V$Emotion_Types)

logistic_model <- glm(Emotion_Types ~ Response, data = JV_response_noSad_V, family = binomial) 



# To run the ROC (Receiver Operating Characteristic) curve (a popular tool for evaluating the performance of a binary classification model, such as logistic regression):
# install.packages("pROC")
library(pROC)

# Make Predictions: Use your logistic regression model to predict the response probabilities for your data 
probabilities <- predict(logistic_model, type = "response")


# Create the ROC Curve:
roc_obj <- roc(JV_response_noSad_V$Emotion_Types, probabilities)
plot(roc_obj, main="ROC Curve", col="blue")


# Calculate the AUC:
auc(roc_obj)
# Area under the curve: 0.9632

# The ROC curve will give you a visual understanding of how well your model is discriminating between the two classes ("P" and "N" in your case). The AUC is a single value that summarizes the ROC curve; a value of 0.5 indicates no discrimination, while a value closer to 1 indicates better discrimination.


# To produce a confusion or classification matrix:
# Make Predictions: You'll need to predict the classes for your data. You can use a threshold of 0.5, or choose a different threshold that makes sense for your problem.

# probabilities <- predict(logistic_model, type = "response")
predictions <- ifelse(probabilities > 0.5, "N", "H") 


# Create the Confusion Matrix: You can use the table function in R to create a confusion matrix, comparing the predicted classes to the actual classes.

actual_classes <- JV_response_noSad_V$Emotion_Types # Adjust based on your data frame and column name
confusion_matrix <- table(Predicted = predictions, Actual = actual_classes)
print(confusion_matrix)

# doing it differently to compare with the above, gives us more measures of model quality
library(ggplot2)
library(lattice)
library(caret)

predictions <- factor(predictions)
actual_classes <- factor(actual_classes)

cm <- confusionMatrix(predictions, actual_classes)
print(cm)

# Kappa gives a measure of how much better the predictions are than what would be expected by chance. A kappa of 1 means perfect agreement between model predictions and actual values, while a kappa near 0 suggests the model isn't performing better than random chance.

# calculate Odds Ratio for my model

# Get the coefficients of the logistic regression model
coefficients <- coef(logistic_model)

# Calculate the odds ratios by exponentiating the coefficients
odds_ratios <- exp(coefficients)

# Display the odds ratios
print(odds_ratios)



# create wins/losses/rankings
names(JV_response_noSad_J)[names(JV_response_noSad_J) == "Spreadsheet..stimulus"] <- "faces"
names(JV_response_noSad_J)[names(JV_response_noSad_J) == "Participant.Public.ID"] <- "ID"

JV_response_noSad_J$Response <- as.numeric(JV_response_noSad_J$Response)

JV_response_noSad_J <- JV_response_noSad_J %>%
  group_by(ID) %>%
  mutate(
    p25 = quantile(Response, 0.25, na.rm = TRUE),   # Calculate the 10th percentile
    p75 = quantile(Response, 0.75, na.rm = TRUE),   # Calculate the 90th percentile
    Critical = ifelse(Response <= p25, 1, 0),  # Flag the 'response' values below or equal to the 10th percentile
    Easy = ifelse(Response > p75, 1, 0)  # Flag the 'response' values above the 90th percentile
  ) %>%
  ungroup() %>%
  select(-p25, -p75)  # Remove the temporary p10 and p90 columns


# Count number of 1's per crticial versus Easy column across all subjects,
# So we should have 100 rows since we have removed the sad images


combined_df <- JV_response_noSad_J %>%
  group_by(faces) %>%
  summarise(
    Count_Critical = sum(Critical, na.rm = TRUE),
    Percentage_Critical = round((Count_Critical / n()) * 100, 2),
    Count_Easy = sum(Easy, na.rm = TRUE),
    Percentage_Easy = round((Count_Easy / n()) * 100, 2)
  )

combined_df <- combined_df %>%
  mutate(
    rank_Easy = dense_rank(desc(Count_Easy)),
    rank_Critical = dense_rank(desc(Count_Critical))
  )

# hist(combined_df$Percentage_Critical)


# Relationship response_J and response_V
# Compute the linear model
model <- lm(Response_V ~ Response_J, data = data_wide_JV)

# Extract coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]



p <- ggplot(data_wide_JV, aes(x = Response_J, y = Response_V, color = Participant.Public.ID...12)) +
  geom_point(alpha = 0.6) +  # alpha sets the transparency, which can be helpful if there's overlap
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "black")+
  theme_minimal() +
  labs(title = "Relationship between Judiginess and Valence ratings",
       x = "Judginess",
       y = "Valence",
       color = "Subject ID") +
  theme(legend.position = "right")  # Adjust the legend position if needed

# Add the annotations
p + annotate("text", x = Inf, y = Inf, 
             label = sprintf("Intercept: %.2f\nSlope: %.2f", intercept, slope), 
             hjust = "right", vjust = "top", 
             size = 4, color = "blue")

# this is across all subjects (ties warning)
cor.test(data_wide_JV$Response_J, data_wide_JV$Response_V, method = "spearman")
# below I calculate the correlation per unique subject ID, and then we can average them
correlations_per_ID <- data_wide_JV %>%
  group_by(Participant.Public.ID...12) %>%
  summarize(correlation = cor(Response_J, Response_V, method = "spearman", use = "complete.obs"))

average_correlation <- mean(correlations_per_ID$correlation, na.rm = TRUE) #average correlation is rs=0.78 as opposed to rs=0.83 doing it across all subjects at once


# Doing a proportion test comparing the percentages of wins (the sum across subjects, of being within the 20th percentile per subject), being different than 50%
# To test whether the proportion in Percentage_Critical is significantly different from 50% for each observation, and similarly for Percentage_Easy

# Treat the percentage value for each observation as the number of successes.
# Treat the total possible count (i.e., 100 if these are percentages) as the number of trials.
# Run the proportion test comparing the observed successes to the expected (50% of trials).

combined_df$Critical_pvalue <- apply(combined_df, 1, function(row) {
  x_val <- round(as.numeric(row["Percentage_Critical"]))
  prop.test(x = x_val, n = 100, p = 0.5, alternative = "greater")$p.value
})

#do a t-test
combined_df$Easy_pvalue <- apply(combined_df, 1, function(row) {
  x_val <- round(as.numeric(row["Percentage_Easy"]))
  prop.test(x = x_val, n = 100, p = 0.5, alternative = "greater")$p.value
})


# filtering the significant p-values
alpha <- 0.05
significant_critical <- combined_df[combined_df$Critical_pvalue < alpha, ]
significant_easy <- combined_df[combined_df$Easy_pvalue < alpha, ]


write_csv(significant_critical,"significant_critical_images.csv") 
write_csv(significant_easy,"significant_easy_images.csv") 
write_csv(combined_df,"all_image_rankings.csv") 

# # doing a t-test instead of prop-test
# combined_df$Critical_pvalue <- apply(combined_df, 1, function(row) {
#   x_val <- round(as.numeric(row["Percentage_Critical"]))
#   t.test(data_wide_JV$Response_J, mu = 50, alternative = "two.sided")$p.value
# })
# 
# #do a t-test
# combined_df$Easy_pvalue <- apply(combined_df, 1, function(row) {
#   x_val <- round(as.numeric(row["Percentage_Easy"]))
#   t.test(data_wide_JV$Response_J, mu = 50, alternative = "two.sided")$p.value
# })
# 
# 
# # filtering the significant p-values
# alpha <- 0.05
# significant_critical <- combined_df[combined_df$Critical_pvalue < alpha, ]
# significant_easy <- combined_df[combined_df$Easy_pvalue < alpha, ]





# just a check that the wide concatenation has worked perfectly for each row and each image/ID
# all_equal <- all(data_wide_JV$Participant.Public.ID...12 == data_wide_JV$Participant.Public.ID...73)
# print(all_equal) # It will return TRUE if all values are the same, otherwise FALSE
# 
# all_equal <- all(data_wide_JV$Spreadsheet..stimulus...58 == data_wide_JV$Spreadsheet..stimulus...119)
# print(all_equal) # It will return TRUE if all values are the same, otherwise FALSE


# t.test(combined_df$Percentage_Critical, mu = 50, alternative = "two.sided")

