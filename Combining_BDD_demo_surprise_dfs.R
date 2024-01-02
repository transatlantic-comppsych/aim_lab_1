library(dplyr)
library(tidyr)
library(ggplot2)
library(parameters)
library(lme4)
library(lmerTest)


df_surprises <- read.csv("/Users/marjan/Desktop/aim_lab_1/all_pilots_with_mini_spin.csv")

# Reading all the BDD data
bdd_p6 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_nrnd_typ/SUP_PRF_pilot_fdbk_nrnd_typ_v3/SUP_PRF_pilot_fdbk_nrnd_typ_v3_biq.csv")
bdd_p7 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_typ/SUP_PRF_pilot_fdbk_bignrnd_typ_v3/SUP_PRF_pilot_fdbk_bignrnd_typ_v3_biq.csv")
bdd_p8 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_biggestnrnd_typ/SUP_PRF_pilot_fdbk_biggestnrnd_typ_v3/SUP_PRF_pilot_fdbk_biggestnrnd_typ_v3_biq.csv")
bdd_p9 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_typ_YPAG/SUP_PRF_pilot_fdbk_bignrnd_typ_YPAG_v6/SUP_PRF_pilot_fdbk_bignrnd_typ_YPAG_v6_biq.csv")
bdd_p10 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_v5/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_v5_biq.csv")
bdd_p11 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_v7/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_v7_biq.csv")

# Reading all the demo data
demo_p6 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_nrnd_typ/SUP_PRF_pilot_fdbk_nrnd_typ_v3/SUP_PRF_pilot_fdbk_nrnd_typ_v3_clinical_srv.csv")
demo_p7 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_typ/SUP_PRF_pilot_fdbk_bignrnd_typ_v3/SUP_PRF_pilot_fdbk_bignrnd_typ_v3_clinical_srv.csv")
demo_p8 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_biggestnrnd_typ/SUP_PRF_pilot_fdbk_biggestnrnd_typ_v3/SUP_PRF_pilot_fdbk_biggestnrnd_typ_v3_clinical_srv.csv")
demo_p9 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_typ_YPAG/SUP_PRF_pilot_fdbk_bignrnd_typ_YPAG_v6/SUP_PRF_pilot_fdbk_bignrnd_typ_YPAG_v6_clinical_srv.csv")
demo_p10 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_v5/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_v5_clinical_srv.csv")
demo_p11 <- read.csv("/Users/marjan/Desktop/aim_lab_1/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_v7/SUP_PRF_pilot_fdbk_bignrnd_YPAG_vid_v7_clinical_srv.csv")

# rename ID for subject who has taken part in both studies
demo_p7 <- demo_p7 %>%
  mutate(Random_ID = ifelse(Random_ID == "SUPPRF78461", "SUPPRF78461_p7", Random_ID))
demo_p11 <- demo_p11 %>%
  mutate(Random_ID = ifelse(Random_ID == "SUPPRF78461", "SUPPRF78461_p11", Random_ID))
bdd_p7 <- bdd_p7 %>%
  mutate(Random_ID = ifelse(Random_ID == "SUPPRF78461", "SUPPRF78461_p7", Random_ID))
bdd_p11 <- bdd_p11 %>%
  mutate(Random_ID = ifelse(Random_ID == "SUPPRF78461", "SUPPRF78461_p11", Random_ID))

# bdd_all <- rbind(bdd_p6, bdd_p7, bdd_p8,bdd_p9,bdd_p10, bdd_p11)
BIQ_data <- rbind(bdd_p6, bdd_p7, bdd_p8,bdd_p9,bdd_p10, bdd_p11)

BIQ_data <- data.frame(BIQ_data$Random_ID, BIQ_data$Response, BIQ_data$Object.ID)
BIQ_data <- subset(BIQ_data, BIQ_data.Response != "" & BIQ_data.Response != "END" &
                     BIQ_data.Response != "BEGIN" & BIQ_data.Random_ID != "")

BIQ_data <- reshape(BIQ_data, idvar = "BIQ_data.Random_ID", timevar = "BIQ_data.Object.ID", direction = "wide")


BIQ_data$`BIQ_data.Response.object-5` <- as.numeric(BIQ_data$`BIQ_data.Response.object-5`)
BIQ_data$`BIQ_data.Response.object-18` <- as.numeric(BIQ_data$`BIQ_data.Response.object-18`)
BIQ_data$`BIQ_data.Response.object-80` <- as.numeric(BIQ_data$`BIQ_data.Response.object-80`)
BIQ_data$`BIQ_data.Response.object-259` <- as.numeric(BIQ_data$`BIQ_data.Response.object-259`)
BIQ_data$`BIQ_data.Response.object-36` <- as.numeric(BIQ_data$`BIQ_data.Response.object-36`)
BIQ_data$`BIQ_data.Response.object-86` <- as.numeric(BIQ_data$`BIQ_data.Response.object-86`)
BIQ_data$`BIQ_data.Response.object-53` <- as.numeric(BIQ_data$`BIQ_data.Response.object-53`)
BIQ_data$`BIQ_data.Response.object-60` <- as.numeric(BIQ_data$`BIQ_data.Response.object-60`)
BIQ_data$`BIQ_data.Response.object-67` <- as.numeric(BIQ_data$`BIQ_data.Response.object-67`)

BIQ_data <- BIQ_data %>%
    rename(
      Random_ID = "BIQ_data.Random_ID",
      Q1 = "BIQ_data.Response.object-5",
      Q2 = "BIQ_data.Response.object-18",
      Q3 =  "BIQ_data.Response.object-80",
      Q4 =  "BIQ_data.Response.object-259",
      Q5 = "BIQ_data.Response.object-36",
      Q6 = "BIQ_data.Response.object-86",
      Q7 = "BIQ_data.Response.object-53",
      Q8 = "BIQ_data.Response.object-60",
      Q9 = "BIQ_data.Response.object-67",
      )

# columns mismatch between data frames, it is due to the ealier two pilots having different names but the columns collect the same data
# let's keep the name from other pilots
names(demo_p6) <- names(demo_p10)
names(demo_p7) <- names(demo_p10)

demo_all <- rbind(demo_p6, demo_p7, demo_p8,demo_p9,demo_p10, demo_p11)

# merging this data to the rest
demo_data <- demo_all[, c("Random_ID", "Dropdown.object.19.Response", "Sex.object.25.Response", "Gender.object.24.Response")]
names(demo_data)[names(demo_data) == "Dropdown.object.19.Response"] <- "age"
names(demo_data)[names(demo_data) == "Sex.object.25.Response"] <- "sex"
names(demo_data)[names(demo_data) == "Gender.object.24.Response"] <- "Gender"


# missing_ids <- anti_join(demo_dfs, surprises_df, by = "Random_ID")
# # SUPPRF09251, SUPPRF28707, SUPPRF78461, SUPPRF78461
# trial_range_per_ID <- df %>%
#   group_by(Random_ID) %>%
#   summarize(
#     Min_Trial = min(Trial.Number),
#     Max_Trial = max(Trial.Number)
#   )
# These people had less trials but are kept in the data
# SUPPRF43235 (36), SUPPRF66080(18), SUPPRF83676(13), SUPPRF87060 (47) 


# ####################################
#  extracting slope for mood model   #
# ####################################
pilots <- unique(df_surprises$pilot_nr)
my_splits <- (split(df_surprises, df_surprises$pilot_nr))
my_splits <- my_splits[pilots]
mix_models_per_pilot <- list() # the lme objects for each pilot
mix_models_coefficients <- list() # the coefficients for each pilot
std_param_mix_models_per_pilot <- list() # the standardised coefficients for each lme object
dfs_RE_raw_pe_mood <- list() # the dataframes that contain raw values and coefficeints (may not need this)
for(i in 1: length(my_splits)){
  
  mix_models_per_pilot[[i]] <-  lmerTest::lmer(Mood ~ SubjPE + (SubjPE| Random_ID), data = 
                                                 df_surprises[df_surprises$pilot_nr==pilots[i],], 
                                               REML = FALSE, 
                                               control = lmerControl(optimizer = "bobyqa"))
  std_param_mix_models_per_pilot[[i]] <- parameters:: standardise_parameters( mix_models_per_pilot[[i]])
  
  mix_models_coefficients[[i]] <-  coef(mix_models_per_pilot[[i]])
  mix_models_coefficients[[i]] <- data.frame(mix_models_coefficients[[i]]$Random_ID)
  mix_models_coefficients[[i]]$Random_ID <- rownames(mix_models_coefficients[[i]])
  colnames(mix_models_coefficients[[i]]) <-c( "intercept_mood", "slope_mood", "Random_ID")
  
  #now merge these datasets with the raw values 
  dfs_RE_raw_pe_mood[[i]] <- left_join(my_splits[[i]], mix_models_coefficients[[i]], by = "Random_ID" )
  
}
names(std_param_mix_models_per_pilot) <- pilots

# Put all datasets together now 
df_all_surprise_experiments_mood <- do.call(rbind,dfs_RE_raw_pe_mood)


# #####################################
# extracting slope for anxiety model  #
# ####################################

mix_models_per_pilot <- list() # the lme objects for each pilot
mix_models_coefficients <- list() # the coefficients for each pilot
std_param_mix_models_per_pilot <- list() # the standardised coefficients for each lme object
dfs_RE_raw_pe_anxiety <- list() # the dataframes that contain raw values and coefficeints (may not need this)
for(i in 1: length(my_splits)){
  
  mix_models_per_pilot[[i]] <-  lmerTest::lmer(Anxiety ~ SubjPE + (SubjPE| Random_ID), data = 
                                                 df_surprises[df_surprises$pilot_nr==pilots[i],], 
                                               REML = FALSE, 
                                               control = lmerControl(optimizer = "bobyqa"))
  std_param_mix_models_per_pilot[[i]] <- parameters:: standardise_parameters( mix_models_per_pilot[[i]])
  
  mix_models_coefficients[[i]] <-  coef(mix_models_per_pilot[[i]])
  mix_models_coefficients[[i]] <- data.frame(mix_models_coefficients[[i]]$Random_ID)
  mix_models_coefficients[[i]]$Random_ID <- rownames(mix_models_coefficients[[i]])
  colnames(mix_models_coefficients[[i]]) <-c( "intercept_anxiety", "slope_anxiety", "Random_ID")
  
  #now merge these datasets with the raw values 
  dfs_RE_raw_pe_anxiety[[i]] <- left_join(my_splits[[i]], mix_models_coefficients[[i]], by = "Random_ID" )
  
}
names(std_param_mix_models_per_pilot) <- pilots

# Put all datasets together now 
df_all_surprise_experiments_anxiety <- do.call(rbind,dfs_RE_raw_pe_anxiety)


df_slopes <- df_all_surprise_experiments_mood %>%
  left_join(df_all_surprise_experiments_anxiety[, c("Random_ID", "Trial.Number", "slope_anxiety")], 
            by = c("Random_ID", "Trial.Number"))


# Let's select only first mood and anxiety ratings
df_slopes <- subset(df_slopes, Trial.Number == 1)
task_data <- df_slopes[, c("Random_ID", "Trial.Number", "Mood", "Anxiety", "pilot_nr", "mini_SPIN_total", "Social_Anxiety", "slope_anxiety", "slope_mood")]

df_final_merged <- task_data %>%
  left_join(BIQ_data, by = "Random_ID") %>%
  left_join(demo_data, by = "Random_ID")


write.csv(df_final_merged, "Pilots_6_to_9_with_mini_spin_BDD_demographic_data.csv", row.names = FALSE)