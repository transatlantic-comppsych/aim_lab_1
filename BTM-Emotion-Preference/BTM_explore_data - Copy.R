library(tidyverse)
library(BradleyTerry2)
library(psychotree)
library(partykit)
library(gridExtra)

#load data
#for now, exclude the one sub-threshold coded participants
dat <- readRDS(file = "data_BTM.rds") %>% 
  filter(status %in% c("HV", "MDD"))

items <- c(
  "I never want to be sad again",
  "I feel that occasional sadness is important",
  "I enjoy feeling melancholy sometimes",
  "It is not always bad to feel low",
  "It is never good to feel low",
  "When I feel low, I can see things more clearly",
  "Feeling sad sometimes is part of normal life",
  "I have learnt a lot about myself by being happy",
  "I have learnt a lot about myself by being sad",
  "How one feels is not the most important thing",
  "I am always trying to be myself, even if this makes me feel low.",
  "Feeling anger is always good",
  "Feeling anger is never good",
  "Anger can be important in life"
)


####################################
########### MODEL FITTING ##########
####################################
#Using method by Dittrich et al. (1997)

#MODEL 1: Intercept-only BT-model

model1 <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2, id = "item", data = dat)
res1 <- summary(model1)

abilities1 <- BTabilities(model1) %>% as.data.frame()
abilities1$probability <- exp(abilities1[1]) / (exp(abilities1[1]) + 1)
abilities1$description <- items
colnames(abilities1) <- c("ability", "se", "p", "description")

abilities1[order(abilities1$ability, decreasing = TRUE),]


#MODEL 2: Predict Abilities from Depression status and Demographics

model2 <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2, 
             formula = ~ item + item * status + item * group + item * sex, id = "item", data = dat)
res2 <- summary(model2)

#get abilities, where p = probability of winning against reference item (= item 1)
abilities2 <- BTabilities(model2) %>% as.data.frame()
abilities2$probability <- exp(abilities2[1]) / (exp(abilities2[1]) + 1)
abilities2$description <- items
colnames(abilities2) <- c("ability", "se", "p", "description")

abilities2[order(abilities2$ability, decreasing = TRUE),]

#perform a chi-square test
comp <- anova(model1, model2, test = "Chisq")

#the test finds a residual difference for 39DF = significant at alpha = 0.05 level
#model2 wins this comparison.
#What if we remove demographics?

#MODEL 3: Can we remove sex?

model3 <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2, 
              formula = ~ item + item * status + item * group, id = "item", data = dat)
res3 <- summary(model3)

#perform a chi-square test
anova(model2, model3, test = "Chisq") #sex should be retained


#MODEL 4: Can we remove age?

model4 <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2, 
              formula = ~ item + item * status + item * sex, id = "item", data = dat)
res4 <- summary(model4, test = "Chisq")

#perform a chi-square test
anova(model2, model4) #age should be retained

#Model 5: Can we remove depression?

model5 <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2, 
              formula = ~ item + item * group + item * sex, id = "item", data = dat)
res5 <- summary(model5)

#perform a chi-square test
anova(model2, model5, test = "Chisq") #depression should be retained

###################################################
####### GAIN MORE INSIGHT BY GROUPING ITEMS #######
###################################################

dat_group_comp <- dat

sad_bad_list <- c("i1", "i5", "i8")
sad_good_list <- c("i2", "i3", "i4", "i6", "i7", "i11")
anger_good_list <- c("i12", "i14")
neutral_list <- c("i10")
anger_bad_list <- c("i13")

#Contrasts for hypothesis 1: Items involving acceptance/learning will be rated highly
most_pref_list <- c("i2", "i4", "i7", "i8", "i9")
#Contrasts for hypothesis 2: Items involving 'global' claims (always, never) will be rated low
least_pref_list <- c("i5", "i12", "i13")
#Contrasts for hypothesis 3 More depressed -> More likely to see depression as useful
learning_list <- c("i6", "i9")
#Contrasts for hypothesis 4: Younger -> More likely to accept feeling low for authenticity
myself_list <- c("i11")

#reformat for groupwise comparisons
dat_group_comp$item1 <- data.frame(item= dat$item1, sad_bad = ifelse(dat$item1 %in% sad_bad_list, 1, 0), 
                                   sad_good = ifelse(dat$item1 %in% sad_good_list, 1, 0), 
                                   anger_good = ifelse(dat$item1 %in% anger_good_list, 1, 0),
                                   anger_bad = ifelse(dat$item1 %in% anger_bad_list, 1, 0),
                                   neutral = ifelse(dat$item1 %in% neutral_list, 1, 0),
                                   most_pref = ifelse(dat$item1 %in% most_pref_list, 1, 0),
                                   least_pref = ifelse(dat$item1 %in% least_pref_list, 1, 0),
                                   learning = ifelse(dat$item1 %in% learning_list, 1, 0),
                                   myself = ifelse(dat$item1 %in% myself_list, 1, 0)
)

dat_group_comp$item2 <- data.frame(item= dat$item2, sad_bad = ifelse(dat$item2 %in% sad_bad_list, 1, 0), 
                                   sad_good = ifelse(dat$item2 %in% sad_good_list, 1, 0), 
                                   anger_good = ifelse(dat$item2 %in% anger_good_list, 1, 0),
                                   anger_bad = ifelse(dat$item2 %in% anger_bad_list, 1, 0),
                                   neutral = ifelse(dat$item2 %in% neutral_list, 1, 0),
                                   most_pref = ifelse(dat$item2 %in% most_pref_list, 1, 0),
                                   least_pref = ifelse(dat$item2 %in% least_pref_list, 1, 0),
                                   learning = ifelse(dat$item2 %in% learning_list, 1, 0),
                                   myself = ifelse(dat$item2 %in% myself_list, 1, 0)
)

#run comparisons grouped by items

model_grouped_main <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2,
                          formula = ~
                            most_pref + least_pref + learning + myself, 
                            id = "item", data = dat_group_comp)

res_groupwise <- data.frame()
res_itemwise <- data.frame()

#fit BT models for the sub-sets
for (i in c("MDD", "HV")) {
  for (j in c("adult", "adolescent")) {
    this_dat <- dat_group_comp %>% 
      filter(
        status == i,
        group == j
      )
    
    groupwise <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2,
                              formula = ~
                              most_pref + least_pref + learning + myself, 
                              id = "item", data = this_dat)
    
    sum <- summary(groupwise)$coefficients 
    sum <- cbind(rownames(sum), sum)
    colnames(sum)[1] <- "category"
    
    res_groupwise <- sum %>%
      as.data.frame() %>% 
      mutate(
        status = i,
        group = j,
        cat = paste(j, i, sep = "_"),
        category = gsub('[[:digit:]]+', '', category)
      ) %>% rbind(res_groupwise, .)

    itemwise <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2,
                    formula = ~ item, 
                    id = "item", data = this_dat)
    
    sum <- summary(itemwise)$coefficients 
    sum <- cbind(2:14, sum)
    colnames(sum)[1] <- "item"
    
    res_itemwise <- sum %>%
      as.data.frame() %>% 
      mutate(
        status = i,
        group = j,
        cat = paste(j, i, sep = "_")
      ) %>% rbind(res_itemwise, .)
    
   }
}

model_grouped_interactions <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2,
                             formula = ~ most_pref + least_pref + learning + myself +
                               most_pref * status + most_pref * group + most_pref * sex +
                               least_pref * status + least_pref * group + least_pref * sex +
                               learning * status + learning * group + learning * sex +
                               myself * status + myself * group + myself * sex,
                               id = "item", data = dat_group_comp)

coefs <- summary(model_grouped_interactions)$coefficients

p <- data.frame(
  item = rep(paste("i", 1:14, sep = ""), 8))

model_hypotheses <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2,
                        formula = ~ most_pref + least_pref + learning * CESD_score + myself * age,
                        id = "item", data = dat_group_comp)
res_hypotheses <- summary(model_hypotheses)

#generate plots for descriptives

dat_plots <- dat %>% 
  group_by(across(all_of(c("group", "status", "item1", "item2")))) %>% 
  summarise(
    wins1 = sum(win1),
    wins2 = sum(win2)
  ) %>% 
  mutate(
    n_comparisons = wins1 + wins2
  )


#generate a list of how often each item wins in each group
dat_wins <- data.frame(
  item = 1:14,
  item_str = paste("i", 1:14, sep = ""),
  adult_MDD = rep(0, 14),
  adolescent_MDD = rep(0, 14),
  adult_HV = rep(0, 14),
  adolescent_HV = rep(0, 14)
)

for(i in 1:length(1:14)) {
  for (j in 1:nrow(dat_plots)) {
    if (!is.na(dat_plots$item1[j])) {
      if (dat_plots$item1[j] == paste("i", i, sep = "")) {
        
         if(dat_plots$group[j] == "adult" & dat_plots$status[j] == "MDD") {
           
              dat_wins$adult_MDD[i] = dat_wins$adult_MDD[i] + dat_plots$wins1[j]
              
              
          } else if (dat_plots$group[j] == "adult" & dat_plots$status[j] == "HV") {
              dat_wins$adult_HV[i] = dat_wins$adult_HV[i] + dat_plots$wins1[j]
              
          } else if (dat_plots$group[j] == "adolescent" & dat_plots$status[j] == "HV") {
            dat_wins$adolescent_HV[i] = dat_wins$adolescent_HV[i] + dat_plots$wins1[j]
            
          } else if (dat_plots$group[j] == "adolescent" & dat_plots$status[j] == "MDD") {
            dat_wins$adolescent_MDD[i] = dat_wins$adolescent_MDD[i] + dat_plots$wins1[j]
          } 
      }
    }
    
    if (dat_plots$item2[j] == paste("i", i, sep = "")) {
      
      if(dat_plots$group[j] == "adult" & dat_plots$status[j] == "MDD") {
        
        dat_wins$adult_MDD[i] = dat_wins$adult_MDD[i] + dat_plots$wins2[j]
        
        
      } else if (dat_plots$group[j] == "adult" & dat_plots$status[j] == "HV") {
        dat_wins$adult_HV[i] = dat_wins$adult_HV[i] + dat_plots$wins2[j]
        
      } else if (dat_plots$group[j] == "adolescent" & dat_plots$status[j] == "HV") {
        dat_wins$adolescent_HV[i] = dat_wins$adolescent_HV[i] + dat_plots$wins2[j]
        
      } else if (dat_plots$group[j] == "adolescent" & dat_plots$status[j] == "MDD") {
        dat_wins$adolescent_MDD[i] = dat_wins$adolescent_MDD[i] + dat_plots$wins2[j]
      } 
    }
  }
}

#number of expected wins per item/group with prior belief = 0.5
#0.5 * n_participants * n_contests_with_item
#there are 14 items, if every item is compared to itself and all other items that's 13 contests per item
expected <- data.frame(
  cat = c("adult_MDD", "adult_HV", "adolescent_MDD", "adolescent_HV"),
  expected = c(
    filter(dat, status == "MDD" & group == "adult")$participant %>% unique() %>% length() * 0.5 * 13,
    filter(dat, status == "HV" & group == "adult")$participant %>% unique() %>% length() * 0.5 *13,
    filter(dat, status == "MDD" & group == "adolescent")$participant %>% unique() %>% length() * 0.5 * 13,
    filter(dat, status == "HV" & group == "adolescent")$participant %>% unique() %>% length() * 0.5 * 13
    )
  )

dat_wins <- dat_wins %>% 
  pivot_longer(cols = c("adult_MDD", "adolescent_MDD", "adult_HV", "adolescent_HV"), names_to = "cat", values_to = "wins") %>% 
  full_join(expected, ., by = "cat") %>% 
  mutate(
    group = ifelse(grepl("adolescent", cat), "adolescent", "adult"),
    status = ifelse(grepl("MDD", cat), "MDD", "HV"),
    wins_rel = wins / expected,
    most_pref = ifelse(item_str %in% most_pref_list, 1, 0),
    least_pref = ifelse(item_str %in% least_pref_list, 1, 0),
    learning = ifelse(item_str %in% learning_list, 1, 0),
    myself = ifelse(item_str %in% myself_list, 1, 0),
    category = ifelse(most_pref == 1, "most_pref",
                      ifelse(least_pref == 1, "least_pref",
                             ifelse(learning == 1, "learning",
                                    ifelse(myself == 1, "myself", "No Category")
                             )
                          )
                      )
  )

dat_plotting <- full_join(res_itemwise, dat_wins) %>% 
  mutate(
    Estimate = ifelse(item == 1, 0, Estimate),
    group = ifelse(group == "adolescent", "Adolescent", "Adult")
  )

plot_itemwise <- ggplot(data = dat_plotting, aes(x = item)) +
  facet_grid(row = vars(status), col = vars(group)) + 
  labs(title = "Results Itemwise", 
       subtitle = "Actual Wins (Gray) and Estimated Item Abilities (Black)") + 
  geom_col(aes(y = wins_rel), fill = rgb(0.8, 0.8, 0.8)) +
  geom_hline(yintercept = 1, colour = rgb(0.4, 0.4, 0.4)) + 
  geom_errorbar(
    aes(x = item, 
        ymin = (as.numeric(Estimate + 2) - 2 * as.numeric(`Std. Error`))/2, 
        ymax = (as.numeric(Estimate + 2) + 2 * as.numeric(`Std. Error`))/ 2)
  ) +
  geom_point(aes(x = item, y = as.numeric((Estimate + 2)/2))) +
  xlab("Item") +
  scale_x_continuous(breaks = c(1:14)) + 
  scale_y_continuous(
    limits = c(-0.1, 2.5),
    name = "Ratio of actual wins / expected wins (Gray)",
    sec.axis = sec_axis(~.* 2 - 2, name = "Estimated Ability (Black)")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold")
  )

plot_itemwise

dat_plotting_groupwise <- dat_wins %>% 
  group_by(across(all_of(c("cat", "category")))) %>% 
  summarise(
    wins = sum(wins)
  ) %>% 
  full_join(res_groupwise,.) %>% 
  mutate(
    group = ifelse(group == "adolescent", "Adolescent", "Adult")
  )

plot_groupwise<- ggplot(data = dat_plotting_groupwise, aes(x = item)) +
  facet_grid(row = vars(status), col = vars(group)) + 
  labs(title = "Results Groupwise", 
       subtitle = "Actual Wins (Gray) and Estimated Item Abilities (Black)") + 
  geom_col(aes(y = wins_rel), fill = rgb(0.8, 0.8, 0.8)) +
  geom_hline(yintercept = 1, colour = rgb(0.4, 0.4, 0.4)) + 
  geom_errorbar(
    aes(x = item, 
        ymin = (as.numeric(Estimate + 2) - 2 * as.numeric(`Std. Error`))/2, 
        ymax = (as.numeric(Estimate + 2) + 2 * as.numeric(`Std. Error`))/ 2)
  ) +
  geom_point(aes(x = item, y = as.numeric((Estimate + 2)/2))) +
  xlab("Category") +
  scale_x_continuous(breaks = c(1:14)) + 
  scale_y_continuous(
    limits = c(-0.1, 2.5),
    name = "Ratio of actual wins / expected wins (Gray)",
    sec.axis = sec_axis(~.* 2 - 2, name = "Estimated Ability (Black)")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold")
  )


plot_wins_groupwise <- ggplot(data = dat_wins, aes(x = category)) +
  facet_grid(row = vars(status), col = vars(group)) + 
  geom_col(aes(y = wins_rel)) +
  geom_hline(yintercept = 1) +
  ggtitle("Ratio of actual versus expected wins per category") +
  xlab("Item") + 
  ylab("Ratio of wins / expected wins")+
  theme_bw()


plot_abilities_itemwise <- ggplot(res_itemwise) +
  facet_grid(rows = vars(status), cols = vars(group)) + 
  geom_point(aes(x = item, y = as.numeric(Estimate))) +
  geom_errorbar(aes(x = item, ymin = as.numeric(Estimate) - 2 * as.numeric(`Std. Error`), ymax = as.numeric(Estimate) + 2 * as.numeric(`Std. Error`)))

plot_abilities_groupwise <- ggplot(res_groupwise) +
  facet_grid(rows = vars(status), cols = vars(group)) + 
  geom_point(aes(x = category, y = as.numeric(Estimate))) +
  geom_errorbar(aes(x = category, ymin = as.numeric(Estimate) - 2 * as.numeric(`Std. Error`), ymax = as.numeric(Estimate) + 2 * as.numeric(`Std. Error`)))


plot_itemwise_adolescent <- ggplot(data = dat_wins, aes(x = item)) +
  geom_col(aes(y = ado_rel)) +
  geom_hline(yintercept = 1) +
  ggtitle("A) Adolescents") +
  xlab("Item") + 
  ylab("Ratio of wins / expected wins")+
  theme_bw()

plot_itemwise_adult <- ggplot(data = dat_wins, aes(x = item)) +
  geom_col(aes(y = adult_rel)) +
  geom_hline(yintercept = 1) +
  ggtitle("B) Adults") +
  xlab("Item") + 
  ylab("Ratio of wins / expected wins")+
  theme_bw()

plot_itemwise_HV <- ggplot(data = dat_wins, aes(x = item)) +
  geom_col(aes(y = HV_rel)) + 
  geom_hline(yintercept = 1) +
  ggtitle("C) Healthy Volunteers") +
  xlab("Item") + 
  ylab("Ratio of wins / expected wins")+
  theme_bw()

plot_itemwise_MDD <- ggplot(data = dat_wins, aes(x = item)) +
  geom_col(aes(y = MDD_rel)) + 
  geom_hline(yintercept = 1) +
  ggtitle("D) MDD") +
  xlab("Item") + 
  ylab("Ratio of wins / expected wins")+
  theme_bw()

plot_itemwise <- grid.arrange(plot_itemwise_adolescent, plot_itemwise_adult,
                              plot_itemwise_HV, plot_itemwise_MDD,
                              top = "Itemwise Odds of Winning")

dat_groupwise <- dat_wins %>% 
  group_by(across(all_of(c("category")))) %>% 
  summarise(
    MDD = sum(MDD) / sum(expected_wins_MDD),
    HV = sum(HV) / sum(expected_wins_HV),
    adult = sum(adult) / sum(expected_wins_adult),
    adolescent = sum(adolescent) / sum(expected_wins_ado),
  )


plot_groupwise_adolescent <- ggplot(data = dat_groupwise, aes(x = category)) +
  geom_col(aes(y = adolescent)) +
  geom_hline(yintercept = 1) +
  ggtitle("A) Adolescents") +
  xlab("Item") + 
  ylab("Category")+
  theme_bw() +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

plot_groupwise_adult <- ggplot(data = dat_groupwise, aes(x = category)) +
  geom_col(aes(y = adult)) +
  geom_hline(yintercept = 1) +
  ggtitle("B) Adults") +
  xlab("Item") + 
  ylab("Category")+
  theme_bw() +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

plot_groupwise_HV <- ggplot(data = dat_groupwise, aes(x = category)) +
  geom_col(aes(y = HV)) + 
  geom_hline(yintercept = 1) +
  ggtitle("C) Healthy Volunteers") +
  xlab("Item") + 
  ylab("Category")+
  theme_bw() +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

plot_groupwise_MDD <- ggplot(data = dat_groupwise, aes(x = category)) +
  geom_col(aes(y = MDD)) + 
  geom_hline(yintercept = 1) +
  ggtitle("D) MDD") +
  xlab("Category") + 
  ylab("Ratio wins / expected wins")+
  theme_bw() +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

plot_groupwise <- grid.arrange(plot_groupwise_adolescent, plot_groupwise_adult,
                               plot_groupwise_HV, plot_groupwise_MDD,
                               top = "Odds of Winning by Category")


###################################################
########### SETTING UP A PROBABILITY TREE #########
###################################################
#Using algorithm from Strobl et al. (2011)
#https://doi.org/10.3102/1076998609359791

#Visualise output with a probability tree, which divides participants into 'groups'

#get everything into right format

dat_tree <- dat %>% 
  group_by(across(all_of(c("participant","item1", "item2")))) %>% 
  summarise(win1 = sum(win1, na.rm = TRUE),
            win2 = sum(win2, na.rm= TRUE),
            age = age,
            sex = sex,
            CESD_score = CESD_score) %>% 
  ungroup() %>% 
  mutate(preference = win1 - win2,
         comparison = gsub("i", "", paste("preference.", item1, ":", item2, sep = ""))) %>% 
  filter(!is.na(item1) & !is.na(item2)) %>%
  select(-item1, item2) %>% 
  pivot_wider(
    id_cols = c(participant, sex, age, CESD_score),
    names_from = comparison,
    values_from = preference
  )

preference <- paircomp(select(dat_tree, starts_with("preference")), labels = paste("i", 1:14, sep = ""))

dat_tree <- dat_tree %>% 
  select(-contains("preference")) %>% 
  cbind(preference)
  

tree <- bttree(formula = preference ~ CESD_score, data = dat_tree, ref = "i1")
plot(x = tree)

#include age as age has been shown not to discriminate between groups
tree_intercept <- bttree(formula = preference ~ age, data = dat_tree, ref = "i1")
plot(x = tree_int)





dat_wins <- dat_wins%>% 
  mutate(total_wins = adult + adolescent,
         #number of expected wins when guessing at random
         # = chance * number of total comparisons made involving this group / n_items
         # Attention, this formula assumes that the dataset is complete and balanced!
         expected_wins_ado = 0.5 * nrow(filter(dat, group == "adolescent" & status = )) / 14,
         expected_wins_adult = 0.5 * nrow(filter(dat, group == "adult")) / 14,
         expected_wins_MDD = 0.5 * nrow(filter(dat, status == "MDD")) / 14,
         expected_wins_HV = 0.5 * nrow(filter(dat, status == "HV")) / 14,
         MDD_rel = MDD / expected_wins_MDD,
         HV_rel = HV / expected_wins_HV,
         ado_rel = adolescent / expected_wins_ado,
         adult_rel = adult / expected_wins_adult,
         most_pref = ifelse(item_str %in% most_pref_list, 1, 0),
         least_pref = ifelse(item_str %in% least_pref_list, 1, 0),
         learning = ifelse(item_str %in% learning_list, 1, 0),
         myself = ifelse(item_str %in% myself_list, 1, 0),
         category = ifelse(most_pref == 1, "Most Preferred",
                           ifelse(least_pref == 1, "Least preferred",
                                  ifelse(learning == 1, "Learning",
                                         ifelse(myself == 1, "Authenticity", "No Category")
                                  )
                           )
         )
  )
