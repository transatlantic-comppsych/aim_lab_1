library(tidyverse)
#author: Johannes Keil

#######################################
############ LOAD AND CLEAN DATA#######
#######################################

#CLEAN DATA FOR ADULTS

dat_pairwise <- read.csv(file = paste(getwd(), "/adult_data/full_pairwise_res.csv", sep = "")) %>% 
  as.data.frame() %>% 
  mutate(
    participant = as.double(id),
    item1 = factor(item_0, levels = 1:14, labels = paste("i", 1:14, sep = "")),
    item2 = factor(item_1, levels = 1:14, labels = paste("i", 1:14, sep = "")),
    win2 = choice,
    win1 = ifelse(choice == 0, 1, 0)
  ) %>% 
  select(-id, -item_0, -item_1) %>% 
  filter(
    !is.na(win1) & !is.na(win2)
  )

dat_depression <- read.csv(file = paste(getwd(), "/adult_data/full_cesd_res.csv", sep = "")) %>% 
  as.data.frame()

dat_dep_summarised <- dat_depression %>% 
  rename(
    participant = "id"
  ) %>% 
  group_by(participant) %>% 
  summarise(
    CESD_score = sum(CESD_response, na.rm = TRUE),
    CESD_rt = mean(CESD_rt, na.rm = TRUE),
    status = factor(ifelse(CESD_score > 15, 1, 0), levels = c(0, 1), labels = c("HV", "MDD"))
  )

dat_demo <- read.csv(file = paste(getwd(), "/adult_data/full_demo_res.csv", sep = "")) %>% as.data.frame() %>% 
  rename(
    participant = "id"
  ) %>% 
  mutate(
    group = "adult"
  ) %>% 
  select(participant, duration, sex, age, group)

dat_adult <- full_join(dat_pairwise, dat_dep_summarised, by = "participant") %>% 
  full_join(., dat_demo, by = "participant") %>% 
  mutate(
    MFQ_score = NA,
    MFQ_rt = NA
  )

#CLEAN DATA FOR ADOLESCENTS

dat_part_status <- read.csv(file = paste(getwd(), "/adolescent_data/part_type.csv", sep = "")) %>% as.data.frame()
dat_demo_ado <- read.csv(file = paste(getwd(), "/adolescent_data/demo_res_20220114.csv", sep = "")) %>% 
  as.data.frame() %>% 
  rename(
    age_original= "age"
  ) %>% 
  full_join(., dat_part_status, by = "SDAN") %>% 
  mutate(
    group = "adolescent"
  ) %>% 
  rename(
    status = "Participant_Type",
    participant = "id"
  ) %>% 
  select(participant, duration, sex, age, status, group)

dat_pairwise_adolescents <- read.csv(file = paste(getwd(), "/adolescent_data/pairwise_res_20220114.csv", sep = "")) %>% 
  as.data.frame() %>% 
  rename(participant = "id")

dat_dep_ado <- read.csv(file = paste(getwd(), "/adolescent_data/mfq_res_20220114.csv", sep = "")) %>% 
  as.data.frame()

dat_dep_ado_summarised <- dat_dep_ado %>% 
  rename(
    participant = "id"
  ) %>% 
  group_by(participant) %>% 
  summarise(
    MFQ_score = sum(MFQ_response, na.rm = TRUE),
    MFQ_rt = mean(MFQ_rt, na.rm = TRUE),
  )

dat_ado <- dat_pairwise_adolescents %>% 
  full_join(dat_demo_ado, by = "participant") %>% 
  full_join(dat_dep_ado_summarised, by = "participant") %>% 
  mutate(
    item1 = factor(item_0, levels = 1:14, labels = paste("i", 1:14, sep = "")),
    item2 = factor(item_1, levels = 1:14, labels = paste("i", 1:14, sep = "")),
    win2 = choice,
    win1 = ifelse(choice == 0, 1, 0),
    log_rt = log(rt),
    CESD_score = NA,
    CESD_rt = NA
  ) %>% 
  select(-item_0, -item_1)

dat <- rbind(dat_adult, dat_ado)

#save data
saveRDS(dat, file = "data_BTM.rds")

