library(tidyverse)
library(conflicted)
library(dplyr)
library(ggplot2)
library(stringr)


# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/marjan/Downloads")

JV_1 <- read.csv("/Users/marjan/Downloads/data_exp_140743-v17-2/JV/data_exp_139282-v42_task-5tah_1.csv")
JV_2 <- read.csv("/Users/marjan/Downloads/data_exp_140743-v17-2/JV/data_exp_139282-v42_task-ak18_1.csv")
JV_3 <- read.csv("/Users/marjan/Downloads/data_exp_140743-v17-2/JV/data_exp_140743-v17_task-5tah_2.csv")
JV_4 <- read.csv("/Users/marjan/Downloads/data_exp_140743-v17-2/JV/data_exp_140743-v17_task-ak18_2.csv")


# Combine the two datasets
df <- rbind(JV_1, JV_2,JV_3,JV_4)

df_resp <- subset(df, Response.Type == "response")
df_resp$Response <- as.numeric(df_resp$Response)


natural_order <- function(x) {
  as.numeric(gsub("[^0-9]", "", x))
}


# Split data based on the value
dataJ <- df_resp %>% 
  dplyr::filter(Screen == "Judginess") %>% 
  select(-Screen) %>%
  rename(Response_J = Response)

# arranging the data according to pictures names, to have the same row for both J and V later on
dataJ_sorted <- dataJ %>% 
  arrange(desc(Spreadsheet..stimulus))

dataV <- df_resp %>% 
  dplyr::filter(Screen == "Valence") %>% 
  select(-Screen) %>%
  rename(Response_V = Response)

dataV_sorted <- dataV %>% 
  arrange(desc(Spreadsheet..stimulus))


# Combine data into wide format
data_wide_JV <- bind_cols(dataJ_sorted, dataV_sorted)
# write_csv(data_wide_JV,"data_wide_JV.csv")
# data_wide_JV now contains two columns that we can use to look at the correlations between rating of valence and judginess
# Response_V and Response_J

# normality testing 
# Perform Shapiro-Wilk test
# The null hypothesis for this test is that the data is normally distributed. If the p-value is less than the chosen alpha level (e.g., 0.05), then you would reject the null hypothesis, and conclude that the data is not normally distributed.
test_resultJ <- shapiro.test(data_wide_JV$Response_J)
print(test_resultJ)

test_resultV <- shapiro.test(data_wide_JV$Response_V)
print(test_resultV)


# hist(data_wide_JV, main="Response_V", xlab="Response_V")
# qqnorm(data)
# qqline(data)
# Calculate Spearman's rank correlation
spearman_cor <- cor.test(data_wide_JV$Response_V, data_wide_JV$Response_J, method = "spearman")
# without p-values: cor(data_wide_JV$Response_V, data_wide_JV$Response_J, method = "spearman")
# Print the result
print(spearman_cor)

ggplot(df_resp, aes(x=Screen, y=Response)) + 
  geom_point(aes(color=Screen)) +  # Coloring points based on x value
  labs(title="Scatterplot with ggplot2", x="X values", y="Y values") +
  theme_minimal()


ggplot(df_resp, aes(x = Screen, y = Response)) +
  geom_density_2d_filled() +
  theme_minimal() +
  labs(title = "distribution of conditions",
       x = "Valence or Judginess",
       y = "Ratings (0-100)")

# df_subset_valence <- subset(df, Screen == "Valence")
# df_subset_judg <- subset(df, Screen == "Judginess")
# 
# df_subset_valence$Response <- as.numeric(df_subset_valence$Response)
# df_subset_judg$Response <- as.numeric(df_subset_judg$Response)

# write_csv(df_subset_valence,"df_subset_valence.csv")
# write_csv(df_subset_valence,"df_subset_judg.csv")


