#load packages -----------------------------------------------------------
library(tidyverse)
library(conflicted)
library(dplyr)
library(stringr)
library(readxl)
# Set your working directory to the folder in which all your CSV files are located
setwd("/Users/Elena/Downloads/copy_data_exp_140743-v17")


#load ASRS dataset and clean------------
#ASRS_data <- read.csv('C:/Users/Elena/Downloads/data_exp_142752-v5/data_exp_142752-v5_questionnaire-qfgj.csv')
ASRS_data <- read.csv('C:/Users/Elena/Downloads/copy_data_exp_140743-v17/data_exp_140743-v17_questionnaire-sii6.csv')
ASRS_data_clean <- ASRS_data[, c(33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63, 65, 67, 68)]

ASRS_data_clean<- ASRS_data_clean %>%
  rename(
    ID = Random_ID,
    Q1= Multiple.Choice.Grid.object.2.1..How.often.do.you.have.trouble.wrapping.up.the.final.details.of.a.project..once.the.challenging.parts.have.been.done..Quantised,
    Q2= Multiple.Choice.Grid.object.2.2..How.often.do.you.have.difficulty.getting.things.in.order.when.you.have.to.do.a.task.that.requires.organization..Quantised,
    Q3= Multiple.Choice.Grid.object.2.3..How.often.do.you.have.problems.remembering.appointments.or.obligations..Quantised,
    Q4= Multiple.Choice.Grid.object.2.4..When.you.have.a.task.that.requires.a.lot.of.thought..how.often.do.you.avoid.or.delay.getting.started..Quantised,
    Q5= Multiple.Choice.Grid.object.2.5..How.often.do.you.fidget.or.squirm.with.your.hands.or.feet.when.you.have.to.sit.down.for.a.long.time..Quantised,
    Q6= Multiple.Choice.Grid.object.2.6..How.often.do.you.feel.overly.active.and.compelled.to.do.things..like.you.were.driven.by.a.motor..Quantised,
    Q7= Multiple.Choice.Grid.object.7.7..How.often.do.you.make.careless.mistakes.when.you.have.to.work.on.a.boring.or.difficult.project..Quantised,
    Q8= Multiple.Choice.Grid.object.7.8..How.often.do.you.have.difficulty.keeping.your.attention.when.you.are.doing.boring.or.repetitive.work..Quantised,
    Q9= Multiple.Choice.Grid.object.7.9...How.often.do.you.have.difficulty.concentrating.on.what.people.say.to.you..even.when.they.are.speaking.to.you.directly..Quantised,
    Q10= Multiple.Choice.Grid.object.7.10..How.often.do.you.misplace.or.have.difficulty.finding.things.at.home.or.at.work..Quantised,
    Q11= Multiple.Choice.Grid.object.7.11..How.often.are.you.distracted.by.activity.or.noise.around.you..Quantised,
    Q12= Multiple.Choice.Grid.object.7.12..How.often.do.you.leave.your.seat.in.meetings.or.other.situations.in.which.you.are.expected.to.remain.seated..Quantised,
    Q13= Multiple.Choice.Grid.object.7.13..How.often.do.you.feel.restless.or.fidgety..Quantised,
    Q14= Multiple.Choice.Grid.object.7.14..How.often.do.you.have.difficulty.unwinding.and.relaxing.when.you.have.time.to.yourself..Quantised,
    Q15= Multiple.Choice.Grid.object.7.15..How.often.do.you.find.yourself.talking.too.much.when.you.are.in.social.situations..Quantised,
    Q16= Multiple.Choice.Grid.object.7.16..When.you.re.in.a.conversation..how.often.do.you.find.yourself.finishing.the.sentences.of.the.people.you.are.talking.to..before.they.can.finish.them.themselves..Quantised,
    Q17= Multiple.Choice.Grid.object.7.17..How.often.do.you.have.difficulty.waiting.your.turn.in.situations.when.turn.taking.is.required..Quantised,
    Q18= Multiple.Choice.Grid.object.7.18..How.often.do.you.interrupt.others.when.they.are.busy..Quantised
    )

#rows_to_remove <- c(1, nrow(ASRS_data_clean))
#ASRS_data_clean <- ASRS_data_clean[-rows_to_remove, ]



# Add scoring threshold for each question ---------------------------------

# Scoring thresholds for each question (customize as needed)
thresholds <- c(
  Q1 = 3, 
  Q2 = 3,
  Q3 = 3,
  Q4 = 4,
  Q5 = 4,
  Q6 = 4,
  Q7 = 4,
  Q8 = 4,
  Q9 = 3,
  Q10 = 4,
  Q11 = 4,
  Q12 = 3,
  Q13 = 4,
  Q14 = 4,
  Q15 = 4,
  Q16 = 3,
  Q17 = 4,
  Q18 = 3)

part_a_questions <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6")
part_b_questions <- c("Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18")

# Scores ------------------------------------------------------------------
Part_A_score <-  numeric(length(ASRS_data_clean$ID))
Part_B_score <-  numeric(length(ASRS_data_clean$ID))
total_scores <- numeric(length(ASRS_data_clean$ID))

part_A_scores <- 0
part_B_scores <- 0
total_scores <- 0
for (i in 1:nrow(ASRS_data_clean)) {
  part_A_score <- sum(ASRS_data_clean[i, part_a_questions] > thresholds[part_a_questions])
  part_B_score <- sum(ASRS_data_clean[i, part_b_questions] > thresholds[part_b_questions])
  total_score <- part_A_score + part_B_score
  
  part_A_scores[i] <- part_A_score
  part_B_scores[i] <- part_B_score
  total_scores[i] <- total_score
}

part_B_scores <- ifelse(is.na(part_B_scores), 0, part_B_scores)
total_scores <- ifelse(is.na(total_scores), 0, total_scores)

# Add the scores as new columns to the data frame
ASRS_data_clean$Part_A_Score <- part_A_scores
ASRS_data_clean$Part_B_Score <- part_B_scores
ASRS_data_clean$TotalScore <- total_scores



# View the updated data frame with scores
print(ASRS_data_clean)

#Note: Participants are only asked to do part B if they score above 4 in part A.
#and this is strongly consistent with an ADHD diagnosis
ASRS_data_clean$Did_Part_B <- ifelse (ASRS_data_clean$Part_A_Score >= 4, 1, 0) # 1 is yes, 0 is no
ASRS_data_clean <- ASRS_data_clean %>%
  mutate(Percentile = ecdf(TotalScore)(TotalScore)*100)

