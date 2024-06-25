library(simr)
library(tidyverse)
library(dplyr)
library(ggplot2)
conflicts_prefer(simr::fixed)

data <- read.csv("pilots_power_calculation.csv")

# Assuming your data frame is called df and Random_ID is the variable containing unique IDs
data <- data %>% 
  mutate(new_id = match(Random_ID, unique(Random_ID)))


# -------------------------power calculation for anxiety-----------------------------------------

# Fit the initial model
model <- lmer(Response_Ax ~ Response_SubjPE + mini_SPIN_total + (1 | new_id), data = data)

# Extracting the residual std, the fixed effects estimates, and the random effects (co)variances
rand <- as.data.frame(VarCorr(model))[1, "vcov"]
res <- sigma(model)
fixedEffects <- fixef(model)

# Creating a simR fitted model object to use as input in powerSim
modelsimr <- makeLmer(Response_Ax ~ Response_SubjPE + mini_SPIN_total + (1 | new_id), 
                      data = data, fixef = fixedEffects, VarCorr = list(new_id = rand), sigma = res)

# Extend the model for power analysis
model_ext <- extend(modelsimr, along = "new_id", n = 200)

# Generate power curve
p_curve <- powerCurve(model_ext, along = "new_id", seed = 42, nsim = 200, breaks = seq(5, 200, by = 10), 
                      test = fixed("Response_SubjPE", "t"))

print(p_curve)
plot(p_curve)


# since we want to have the red line going through 90 instead of the simr's default of 80, I am creating a plot using ggplot. Simr really does not make it easy to extract the power within the power curve (nested list) format
sample_size <- p_curve$xval
power <- c(73, 99, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)

p_curve_data <- data.frame(
  sample_size = sample_size,
  power = power
)


ggplot(p_curve_data, aes(x = sample_size, y = power)) + 
  geom_line() + geom_point()+
  labs(title = "Power Calculation using simr package",
       x = "Sample Size",
       y = "Power Curve for the model: anxiety ~ PE + mini_SPIN")+ ylim(20, 100) + geom_hline(yintercept = 90, linetype = "dotted", color = "red") +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12))

# -------------------------power calculation for mood-----------------------------------------
model <- lmer(Response_H ~ Response_SubjPE + mini_SPIN_total + (1 | new_id), data = data)

# Extracting the residual std, the fixed effects estimates, and the random effects (co)variances
rand <- as.data.frame(VarCorr(model))[1, "vcov"]
res <- sigma(model)
fixedEffects <- fixef(model)

# Creating a simR fitted model object to use as input in powerSim
modelsimr <- makeLmer(Response_H ~ Response_SubjPE + mini_SPIN_total + (1 | new_id), 
                      data = data, fixef = fixedEffects, VarCorr = list(new_id = rand), sigma = res)

# Extend the model for power analysis
model_ext <- extend(modelsimr, along = "new_id", n = 500)

# Generate power curve
p_curve <- powerCurve(model_ext, along = "new_id", seed = 42, nsim = 500, breaks = seq(5, 500, by = 50), 
                      test = fixed("Response_SubjPE", "t"))

print(p_curve)
plot(p_curve)


# -------------------------power calculation for mood ~ PE * CESD-----------------------------------------
model <- lmer(Response_H ~ Response_SubjPE * CESD_score + (1 | new_id), data = data)

# Extracting the residual std, the fixed effects estimates, and the random effects (co)variances
rand <- as.data.frame(VarCorr(model))[1, "vcov"]
res <- sigma(model)
fixedEffects <- fixef(model)

# Creating a simR fitted model object to use as input in powerSim
modelsimr <- makeLmer(Response_H ~ Response_SubjPE * CESD_score + (1 | new_id), 
                      data = data, fixef = fixedEffects, VarCorr = list(new_id = rand), sigma = res)

# Extend the model for power analysis
model_ext <- extend(modelsimr, along = "new_id", n = 700)

# Generate power curve
p_curve <- powerCurve(model_ext, along = "new_id", seed = 42, nsim = 700, breaks = seq(5, 700, by = 100), 
                      test = fixed("Response_SubjPE:CESD_score", "t"))

print(p_curve)
plot(p_curve)

