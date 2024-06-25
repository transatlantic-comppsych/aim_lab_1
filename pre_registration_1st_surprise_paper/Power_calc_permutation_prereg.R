library(tidyverse)
set.seed(123)
data <- read.csv("pilots_power_calculation.csv")

# Function to randomly select participants and subset columns
randomly_select_and_subset <- function(data) {
  data %>%
    group_by(Trial.Number) %>%
    sample_n(25, replace = TRUE)
}


# Perform permutation testing 500 times
permutation_results_list <- replicate(500, randomly_select_and_subset(data), simplify = FALSE)

# Optionally, you can name the list elements for better organization
permutation_results_list <- setNames(permutation_results_list, paste0("permutation_", 1:500))

x <- permutation_results_list$permutation_1
# Assuming you have a list of data frames named permutation_results_list
# Each data frame should have columns: y, x, group


# Anxiety  ----------------------------------------------------------------
# Load the necessary library

library(lmerTest)
conflicts_prefer(lmerTest::lmer)
# Initialize a counter for significant p-values
significant_count <- 0
coefficients <- numeric(length(permutation_results_list)) # or coefficients <- list()

for (i in 1:length(permutation_results_list)) {
  # Fit the LME model
  model <- lmer(Response_Ax ~ Response_SubjPE + mini_SPIN_total + (1 | Random_ID), data = permutation_results_list[[i]])
  
  # Extract p-value for the fixed effect (Response_SubjPE)
  p_value <- summary(model)$coefficients["Response_SubjPE","Pr(>|t|)"]
  #, "Pr(>|t|)"]
  
  # Check if the p-value is significant (you can adjust the threshold)
  if (!is.na(p_value) && p_value < 0.05) {
    significant_count <- significant_count + 1# Store the fixed effect coefficient
    coefficients[i] <- fixef(model)["Response_SubjPE"]
  }
}

# Calculate the percentage of significant p-values
percentage_significant <- (significant_count / length(permutation_results_list)) * 100

# Calculate the average coefficient for the fixed effect
average_coefficient <- mean(coefficients, na.rm = TRUE)

# Print the results
cat("Percentage of significant p-values:", percentage_significant, "%\n")
cat("Average coefficient for the fixed effect (Response_SubjPE):", average_coefficient, "\n")

# Plot anxiety--------------------------------------------------------------------
# Load necessary libraries
library(dplyr)

# Function to randomly select trials for each participant and subset columns
randomly_select_and_subset <- function(data, sample_size) {
  data %>%
    group_by(Trial.Number) %>%
    sample_n(sample_size, replace = TRUE)
}

# Initialize an empty vector to store coefficients
coefficients <- numeric(500)

# Initialize a vector to store percentages of significant p-values
percentage_significant_values <- numeric(0)

sample_sizes <- c(5, 25, 50, 100, 150, 200)
percentage_sig_list <- numeric()


# Loop over the desired sample size
for (sample_size in sample_sizes) {
  # Perform permutation testing 500 times
  permutation_results_list <- replicate(500, randomly_select_and_subset(data, sample_size), simplify = FALSE)
  
  # Optionally, you can name the list elements for better organization
  permutation_results_list <- setNames(permutation_results_list, paste0("permutation_", 1:500))
  
  # Initialize a counter for significant p-values
  significant_count <- 0
  
  for (i in 1:length(permutation_results_list)) {
    # Fit the LME model
    model <- lmer(Response_Ax ~ Response_SubjPE + mini_SPIN_total + (1 | Random_ID), data = permutation_results_list[[i]])
    
    # Extract p-value for the fixed effect (Response_SubjPE)
    p_value <- summary(model)$coefficients["Response_SubjPE","Pr(>|t|)"]
    
    # Check if the p-value is significant (you can adjust the threshold)
    if (!is.na(p_value) && p_value < 0.05) {
      significant_count <- significant_count + 1
      # Store the fixed effect coefficient
      coefficients[i] <- fixef(model)["Response_SubjPE"]
    }
  }
  
  # Calculate the percentage of significant p-values
  percentage_significant <- (significant_count / length(permutation_results_list)) * 100
  percentage_significant_values <- c(percentage_significant_values, percentage_significant)
  
  percentage_sig_list <- c(percentage_sig_list, percentage_significant)
  print(percentage_sig_list)
  
}

# Create a data frame
plot_data <- data.frame(sample_sizes, percentage_sig_list)

# Create a line graph using ggplot2
ggplot(plot_data, aes(x = sample_sizes, y = percentage_sig_list)) + 
  geom_line() + geom_point()+
  labs(title = "Power Calculation using permutations",
       x = "Sample Size",
       y = "Percentage of Significant P-values: anxiety ~ PE + mini_SPIN")+ ylim(0, 100) + geom_hline(yintercept = 90, linetype = "dotted", color = "red") +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12))




# Plot Mood--------------------------------------------------------------------
# Load necessary libraries

# Function to randomly select trials for each participant and subset columns
randomly_select_and_subset <- function(data, sample_size) {
  data %>%
    group_by(Trial.Number) %>%
    sample_n(sample_size, replace = TRUE)
}

# Initialize an empty vector to store coefficients
coefficients <- numeric(500)

# Initialize a vector to store percentages of significant p-values
percentage_significant_values <- numeric(0)

sample_sizes <- c(5, 25, 50, 100, 150, 200)
percentage_sig_list <- numeric()


# Loop over the desired sample size
for (sample_size in sample_sizes) {
  # Perform permutation testing 500 times
  permutation_results_list <- replicate(500, randomly_select_and_subset(data, sample_size), simplify = FALSE)
  
  # Optionally, you can name the list elements for better organization
  permutation_results_list <- setNames(permutation_results_list, paste0("permutation_", 1:500))
  
  # Initialize a counter for significant p-values
  significant_count <- 0
  
  for (i in 1:length(permutation_results_list)) {
    # Fit the LME model
    model <- lmer(Response_H ~ Response_SubjPE + mini_SPIN_total + (1 | Random_ID), data = permutation_results_list[[i]])
    
    # Extract p-value for the fixed effect (Response_SubjPE)
    p_value <- summary(model)$coefficients["Response_SubjPE","Pr(>|t|)"]
    
    # Check if the p-value is significant (you can adjust the threshold)
    if (!is.na(p_value) && p_value < 0.05) {
      significant_count <- significant_count + 1
      # Store the fixed effect coefficient
      coefficients[i] <- fixef(model)["Response_SubjPE"]
    }
  }
  
  # Calculate the percentage of significant p-values
  percentage_significant <- (significant_count / length(permutation_results_list)) * 100
  percentage_significant_values <- c(percentage_significant_values, percentage_significant)
  
  percentage_sig_list <- c(percentage_sig_list, percentage_significant)
  print(percentage_sig_list)
  
}

# Create a data frame
plot_data <- data.frame(sample_sizes, percentage_sig_list)

ggplot(plot_data, aes(x = sample_sizes, y = percentage_sig_list)) + 
  geom_line() + geom_point()+
  labs(title = "Power Calculation using permutations",
       x = "Sample Size",
       y = "Percentage of Significant P-values: mood ~ PE + mini_SPIN")+ ylim(0, 100) + geom_hline(yintercept = 90, linetype = "dotted", color = "red") +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12))


