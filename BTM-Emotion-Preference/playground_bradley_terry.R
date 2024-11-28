library(tidyverse)

#Playing around with Bradley-Terry models

##########################
# BRADLEY TERRY MODEL
#########################

#setting up a dataset. this is a nxn matrix containing all items,
#and how often the other one won agaist it in a forced rank choice

#here, simulate preferences for different treatments
#read: Row won against column

treatments <- c("antidepressant", "anxiolytic", "CBT", "mindfulness")

dat <- data.frame(matrix(nrow = length(treatments), ncol = length(treatments)))
rownames(dat) <- treatments
colnames(dat) <- treatments


#now simulate a random set of abilities for each treatment

abilities <- data.frame(matrix(ncol = 1, nrow = length(treatments)))
                        
abilities[,1] <- c(1, runif(n = length(treatments) - 1, min = 0, max = 2))
rownames(abilities) <- treatments
colnames(abilities) <- c("ability")

#the probability that treatment i wins over treatment j is given by
# ability_i / (ability_i + ability_j)

win_probabilities <- dat

for (i in treatments) {
  for (j in treatments) {
    win_probabilities[i, j] = abilities[i, "ability"] / (abilities[i, "ability"] + abilities[j, "ability"])
    
    if(i == j) win_probabilities[i, j] = 0
  }
}


n_decisions <- matrix(data = round(runif(length(treatments)^2, min = 10, max = 100)), nrow = length(treatments), ncol = length(treatments))
n_decisions <- n_decisions * upper.tri(n_decisions, diag = FALSE) + t(n_decisions * upper.tri(n_decisions, diag = FALSE))


#now simulate a dataset including an unbalanced number of comparisons for every variable
dat <- round(win_probabilities * n_decisions)

#a cool aspect of the bradley terry model, is that it can recover even parameters that are absent in the data
#so, delete any comparisons between the first two treatments
n_decisions[1, 2] <- 0
n_decisions[2, 1] <- 0

#analyse with Bradley-Terry2 package for R
library(BradleyTerry2)

#transform data into binomial count data
dat.binomial <- countsToBinomial(dat)

#fit model

model <- BTm(cbind(win1, win2), player1, player2, ~ player, id = "player", data = dat.binomial)

#check if the model recovered original ability parameters
#abilities predicted by the model
model_abilities <- exp(model$coefficients)
model_abilities

#can also be accessed conveniently using the BTabilities function
BTabilities(model)

#actual abilities
abilities

#now, we can estimate how often the first two treatments(antidepressant & anxiolytic) would win against each other
#even though we never actually compared them in the data
#note that we fixed the ability of the first treatment to 1.

p = 1 / (model_abilities[1] + 1)

#this is not very far off from the true win probability
win_probabilities[1, 2]



#########################################
# BRADLEY TERRY MODEL for multiple participants
#########################################

#fit an analogous model, but for multiple participants

