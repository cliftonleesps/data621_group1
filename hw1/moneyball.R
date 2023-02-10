library(tidyverse)
library(dplyr)

training <- read.csv("moneyball-training-data.csv", header=TRUE)

## Impute missing values with the mean of each column
training$TEAM_BATTING_SO[is.na(training$TEAM_BATTING_SO)] <- mean(training$TEAM_BATTING_SO, na.rm=TRUE)
training$TEAM_BASERUN_SB[is.na(training$TEAM_BASERUN_SB)] <- mean(training$TEAM_BASERUN_SB, na.rm=TRUE)
training$TEAM_BASERUN_CS[is.na(training$TEAM_BASERUN_CS)] <- mean(training$TEAM_BASERUN_CS, na.rm=TRUE)
training$TEAM_BATTING_HBP[is.na(training$TEAM_BATTING_HBP)] <- mean(training$TEAM_BATTING_HBP, na.rm=TRUE)
training$TEAM_PITCHING_SO[is.na(training$TEAM_PITCHING_SO)] <- mean(training$TEAM_PITCHING_SO, na.rm=TRUE)
training$TEAM_FIELDING_DP[is.na(training$TEAM_FIELDING_DP)] <- mean(training$TEAM_FIELDING_DP, na.rm=TRUE)



model <- lm(TARGET_WINS ~., data= training)
summary(model)

## We see the below independent variables are significant:
##   TEAM_BATTING_H
##   TEAM_BATTING_3B
##   TEAM_BATTING_SO
##   TEAM_BASERUN_SB
##   TEAM_FIELDING_E
##   TEAM_FIELDING_DP


## Make a tuned model
tuned_model <- lm(TARGET_WINS ~ TEAM_BATTING_H+TEAM_BASERUN_SB+TEAM_FIELDING_E+TEAM_FIELDING_DP, data= training)
summary(tuned_model)

## all of the predictors are 'significant'


## Read the evaluation data and eliminate NA's
evaluation <- read.csv("moneyball-evaluation-data.csv", header=TRUE)

evaluation$TEAM_BATTING_H[is.na(evaluation$TEAM_BATTING_H)]   <- mean(evaluation$TEAM_BATTING_H, na.rm=TRUE)
##evaluation$TEAM_BATTING_3B[is.na(evaluation$TEAM_BATTING_3B)] <- mean(evaluation$TEAM_BATTING_3B, na.rm=TRUE)
##evaluation$TEAM_BATTING_SO[is.na(evaluation$TEAM_BATTING_SO)] <- mean(evaluation$TEAM_BATTING_SO, na.rm=TRUE)
evaluation$TEAM_BASERUN_SB[is.na(evaluation$TEAM_BASERUN_SB)] <- mean(evaluation$TEAM_BASERUN_SB, na.rm=TRUE)
evaluation$TEAM_FIELDING_E[is.na(evaluation$TEAM_FIELDING_E)] <- mean(evaluation$TEAM_FIELDING_E, na.rm=TRUE)
evaluation$TEAM_FIELDING_DP[is.na(evaluation$TEAM_FIELDING_DP)] <- mean(evaluation$TEAM_FIELDING_DP, na.rm=TRUE)


## Make predictions with the tuned model
prediction <- predict(tuned_model, newdata=evaluation)

prediction


