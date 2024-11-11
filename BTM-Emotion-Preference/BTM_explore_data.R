library(tidyverse)
library(BradleyTerry2)
library(psychotree)
library(partykit)
library(gridExtra)

#load data
#for now, exclude the one sub-threshold coded participants
dat <- readRDS(file = "data_BTM.rds") %>% 
  filter(status %in% c("HV", "MDD"),
         sex == "f") %>% 
  mutate(
    status = factor(status, levels = c("HV", "MDD"))
  )

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

#######################################
########### EXAMINING DESCRIPTIVES#####
#######################################

# get an idea of sample sizes in each sub-group

sample_sizes <- dat %>% 
  select(participant, status, group, sex) %>% 
  unique() %>%
  group_by(status, group, sex) %>% 
  summarize(
    n = n()
  )

sample_sizes

# Looks like adulescent males are severely underrepresented in the sample
# This means that analyses by sex will likely be under-powered, and probably not reliable.

#next, plot the raw number of wins for each item across groups

#aggregate wins across items
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


#the dataset contains every contest in exactly once in one row
#this makes it hard to summarise data with the sum() function
#work around this with a little loop
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

#plot raw wins by group

raw_wins <- dat_wins %>% 
  pivot_longer(cols = c("adult_MDD", "adolescent_MDD", "adult_HV", "adolescent_HV"), names_to = "group", values_to = "wins")

raw_wins_plot <- ggplot(raw_wins) +
  facet_grid(~group) +
  geom_col(aes(x = item, y = wins))

#housekeeping
rm(dat_plots)

# Now plot the ranks of each item as violin plot

#aggregate wins across items
dat_plots1 <- dat %>% 
  group_by(across(all_of(c("group", "status", "sex", "item1", "participant")))) %>% 
  summarise(
    wins1 = sum(win1)
  ) %>% 
  rename(item = item1)

dat_plots2 <- dat %>% 
  group_by(across(all_of(c("group", "status", "sex", "item2", "participant")))) %>% 
  summarise(
    wins2 = sum(win2)
  ) %>% 
  rename(item = item2)

dat_plots <- full_join(dat_plots1, dat_plots2) %>% 
  mutate(
    wins1 = ifelse(is.na(wins1), 0, wins1),
    wins2 = ifelse(is.na(wins2), 0, wins2),
    wins = wins1 + wins2
  ) %>% 
  group_by(status, group, sex, participant) %>% 
  mutate(
    rank = rank(wins)
  ) %>% 
  select(-wins1, -wins2) %>% 
  group_by(item)


raw_wins_violin <- ggplot(dat_plots) +
  labs(title = "Wins per item per participant", caption = "Note: Higher ranks indicate more wins.\nBlack points indicate median number of wins.\nGray points are individual datapoints.") +
  ylab("Rank") +
  xlab("Item") + 
  theme_bw()+
  facet_grid(row = vars(status, sex), col = vars(group)) +
  geom_violin(aes(x = item, y = wins)) +
  geom_jitter(aes(x = item, y = wins), colour = rgb(0.5, 0.5, 0.5),
              width = 0.2, height = 0.0, size = 0.5) +
  stat_summary(aes(x = item, y = rank), geom = "point", fun = "median")

raw_wins_violin

ranked_wins_violin <- ggplot(dat_plots) +
  labs(title = "Ranks per item", caption = "Note: Higher ranks indicate more wins.\nBlack points indicate median rank.\nGray points are individual datapoints.") +
  ylab("Rank") +
  xlab("Item") + 
  theme_bw()+
  facet_grid(row = vars(status, sex), col = vars(group)) +
  geom_violin(aes(x = item, y = rank)) +
  geom_jitter(aes(x = item, y = rank), colour = rgb(0.5, 0.5, 0.),
              width = 0.2, height = 0.0, size = 0.5) +
  stat_summary(aes(x = item, y = rank), geom = "point", fun = "median")
  

ranked_wins_violin

# now show ranks of item per participants

#housekeeping
rm(list = c("dat_plots", "dat_plots1", "dat_plots2"))


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

#Model 3: Predict abilities from MDD, demographics, and their interaction

model3 <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2, 
              formula = ~ item + item * status + item * group + item * sex + item * group * status + item * group * sex + item * sex * status + item * group * status * sex, id = "item", data = dat)
res3 <- summary(model3)


model4 <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2, 
              formula = ~ item + item * status + item * group + item * sex + item * group * status + item * group * sex + item * sex * status, id = "item", data = dat)
res4 <- summary(model3)

comp <- anova(model3, model4, test = "Chisq")


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

#Contrasts for hypothesis 1: Items involving acceptance/learning will be rated highly
most_pref_list <- c("i2", "i4", "i7", "i8", "i9")
#Contrasts for hypothesis 2: Items involving 'global' claims (always, never) will be rated low
least_pref_list <- c("i5", "i12", "i13")
#Contrasts for hypothesis 3 More depressed -> More likely to see depression as useful
learning_list <- c("i6", "i9")
#Contrasts for hypothesis 4: Younger -> More likely to accept feeling low for authenticity
myself_list <- c("i11")
no_category_list <- c("i1", "i3", "i10", "i14")

#reformat for groupwise comparisons
dat_group_comp$item1 <- data.frame(item= dat$item1, 
                                   most_pref = ifelse(dat$item1 %in% most_pref_list, 1, 0),
                                   least_pref = ifelse(dat$item1 %in% least_pref_list, 1, 0),
                                   learning = ifelse(dat$item1 %in% learning_list, 1, 0),
                                   myself = ifelse(dat$item1 %in% myself_list, 1, 0)
)

dat_group_comp$item2 <- data.frame(item= dat$item2,
                                   most_pref = ifelse(dat$item2 %in% most_pref_list, 1, 0),
                                   least_pref = ifelse(dat$item2 %in% least_pref_list, 1, 0),
                                   learning = ifelse(dat$item2 %in% learning_list, 1, 0),
                                   myself = ifelse(dat$item2 %in% myself_list, 1, 0)
)

#run comparisons grouped by items without any predictors
model_grouped_main <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2,
                          formula = ~
                            most_pref + least_pref + learning + myself, 
                            id = "item", data = dat_group_comp)

#same model, but with predictors
model_grouped_interactions <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2,
                                  formula = ~ most_pref + least_pref + learning + myself +
                                    most_pref * status + most_pref * group + most_pref * sex +
                                    least_pref * status + least_pref * group + least_pref * sex +
                                    learning * status + learning * group + learning * sex +
                                    myself * status + myself * group + myself * sex,
                                  id = "item", data = dat_group_comp)



#plotting is difficult as constructing standard errors for BT model linear predictions
#is not intuitive. For convenience, simply fit separate models to all subgroups
#(Note: this will lead to over-estimation of standard-errors, as each model is run on less data)
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
    
    #fit model for groupwise analysis
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
    
    #fit model for itemwise analysis
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

model_hypotheses <- BTm(outcome = cbind(win1, win2), player1 = item1, player2 = item2,
                        formula = ~ most_pref + least_pref + learning * CESD_score + myself * age,
                        id = "item", data = dat_group_comp)
res_hypotheses <- summary(model_hypotheses)


#number of expected wins per item/group with prior belief = 0.5
#0.5 * n_participants * n_contests_with_item
#there are 14 items, if every item is compared to itself and all other items that's 13 contests per item
#the number of expected wins per category then is simply the expected wins for a given item multiplied by the number of items in that category
#Note that items within each category were put in contests with itself. 
expected <- data.frame(
  cat = c("adult_MDD", "adult_HV", "adolescent_MDD", "adolescent_HV"),
  expected = c(
    filter(dat, status == "MDD" & group == "adult")$participant %>% unique() %>% length() * 0.5 * 13,
    filter(dat, status == "HV" & group == "adult")$participant %>% unique() %>% length() * 0.5 *13,
    filter(dat, status == "MDD" & group == "adolescent")$participant %>% unique() %>% length() * 0.5 * 13,
    filter(dat, status == "HV" & group == "adolescent")$participant %>% unique() %>% length() * 0.5 * 13
    )
  )

#pair data on wins with results for a nice overlayed plot
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
                      ),
    expected_cat = ifelse(most_pref == 1, expected * length(most_pref_list),
                      ifelse(least_pref == 1, expected * length(least_pref_list),
                             ifelse(learning == 1, expected * length(learning_list),
                                    ifelse(myself == 1, expected * length(myself_list), 
                                           expected * length(no_category_list))
                             )
                      )
    ),
    wins_rel_cat = wins / expected_cat
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

l1 <- filter(dat_wins, cat == "adolescent_HV" & learning == 1)$wins %>% sum()
l2 <- filter(dat_wins, cat == "adolescent_MDD" & learning == 1)$wins %>% sum()
l3 <- filter(dat_wins, cat == "adult_HV" & learning == 1)$wins %>% sum()
l4 <- filter(dat_wins, cat == "adult_MDD" & learning == 1)$wins %>% sum()

dat_plotting_groupwise <- dat_wins %>% 
  group_by(across(all_of(c("cat", "category", "expected")))) %>% 
  summarise(
    wins = sum(wins),
  ) %>%
  ungroup() %>% 
  mutate(
    #note that our labeling algorithm above assumed that each item is in one category,
    #but this is not the case. Here this leads to us not counting wins on item 9, correct manually. 
    
    wins = ifelse(category != "learning", wins,
                  ifelse(cat == "adolescent_HV", l1,
                      ifelse(cat == "adolescent_MDD", l2,
                            ifelse(cat == "adult_HV", l3,
                               l4)))),
    
    
    expected_cat = ifelse(category == "most_pref", expected * as.numeric(length(most_pref_list)),
                          ifelse(category == "least_pref", expected * as.numeric(length(least_pref_list)),
                                 ifelse(category == "learning", expected * as.numeric(length(learning_list)),
                                        ifelse(category == "myself", expected * as.numeric(length(myself_list)), 
                                               expected * as.numeric(length(no_category_list)))
                                 )
                          )
    ), 
    
    #sum up to get relative win rate, again considering the above caveat
    wins_rel_cat = ifelse(category == "learning",
                          wins / (expected * as.numeric(length(learning_list))), wins / expected_cat),
    
    
  ) %>% 
  full_join(res_groupwise,., by = c("category", "cat")) %>% 
  mutate(
    group = ifelse(group == "adolescent", "Adolescent", "Adult")
  ) %>% 
  filter(
    #exclude items without category from plot.
    category != "No Category"
  )



plot_groupwise <- ggplot(data = dat_plotting_groupwise, aes(x = category)) +
  facet_grid(row = vars(status), col = vars(group)) + 
  labs(title = "Results Groupwise", 
       subtitle = "Actual Wins (Gray) and Estimated Item Abilities (Black)") + 
  geom_col(aes(y = wins_rel_cat), fill = rgb(0.8, 0.8, 0.8)) +
  geom_hline(yintercept = 1, colour = rgb(0.4, 0.4, 0.4)) + 
  geom_errorbar(
    aes(x = category, 
        ymin = (as.numeric(Estimate)  + 2 - 2 * as.numeric(`Std. Error`))/2, 
        ymax = (as.numeric(Estimate)  + 2 + 2 * as.numeric(`Std. Error`))/ 2)
  ) +
  geom_point(aes(x = category, y = (as.numeric(Estimate)+ 2)/2)) +
  xlab("Category") +
  scale_y_continuous(
    limits = c(-0.1, 2.5),
    name = "Ratio of actual wins / expected wins (Gray)",
    sec.axis = sec_axis(~.* 2 - 2, name = "Estimated Ability (Black)")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold")
  )

plot_groupwise
#note that there is a discrepancy between item predictions and actual wins
#this is because the actual wins counts wins that happen within the category