library(tidyverse)
library(dplyr)
library(mice)
library(readr)
library(MASS)
library(forecast)


## This script reads the training data and cleans the insurance data:
##  - Removes z_ prefix from character values
##  - Creates factors for: "PARENT1","CAR_TYPE","JOB","CAR_USE","URBANICITY","RED_CAR","REVOKED","MSTATUS","EDUCATION","SEX")
##  - Converts to integers: INCOME, HOME_VAL, BLUEBOOK, OLDCLAIM
##  - Imputes NAs with the mice library


#train_df <- read.csv("insurance_training_data.csv")
train_df <- read.csv("https://raw.githubusercontent.com/cliftonleesps/data621_group1/main/hw4/insurance_training_data.csv")

## Remove z_ from character class values
z_vars <- c("MSTATUS","SEX","JOB","CAR_TYPE","URBANICITY","EDUCATION")
for (v in z_vars) {
  train_df <- train_df %>% mutate(!!v := str_replace(get(v),"z_",""))
}

## Update RED_CAR, replace [no,yes] values with [No, Yes] values
train_df <- train_df %>% mutate( RED_CAR = ifelse(RED_CAR == "no","No","Yes"))

## Convert from character class to integer
## INCOME
## HOME_VAL
## BLUEBOOK
## OLDCLAIM
currency_vars <- c("INCOME","HOME_VAL","BLUEBOOK","OLDCLAIM")

for (v in currency_vars) {
  train_df <- train_df %>% mutate(!!v := parse_number(get(v)))
}

## Convert the below character variables to factors
## PARENT1
## CAR_TYPE
## JOB
## CAR_USE
## URBANICITY
## RED_CAR
## REVOKED
## MSTATUS
## EDUCATION
## SEX

factor_vars <- c("PARENT1","CAR_TYPE","JOB","CAR_USE","URBANICITY","RED_CAR","REVOKED","MSTATUS","EDUCATION","SEX")

for (v in factor_vars) {
  train_df <- train_df %>% mutate(!!v := factor(get(v)))
}

## show column number with missing values
print(which(colSums(is.na(train_df))>0))

## show only columns with missing values
n <- names(which(colSums(is.na(train_df))>0))

## show how many missing values we have for each
for (i in n) {
    c <- train_df %>% filter(is.na(get(i))) %>% count()
    print (paste0(i, " ", c))
}


## We need to impute the AGE, YOJ, CAR_AGE, INCOME, HOME_VAL and CAR_AGE variables
imputed_Data <- mice(train_df, m=5, maxit = 20, method = 'pmm', seed = 500, printFlag=FALSE)
train_df <- complete(imputed_Data)


## We have one row with a car age < 0! just set it to zero. Assume it's a brand new car
train_df <- rows_update(train_df, tibble(INDEX = 8772, CAR_AGE = 0))


print(summary(train_df))



## Use basic log transformation on INCOME
par(mfrow=c(1,2));hist(train_df$INCOME,breaks=100); hist(log(train_df$INCOME), breaks=100)
train_df$INCOME <- log(train_df$INCOME)


# use Box-Cox on TRAVTIME, BLUEBOOK, TIF
bluebook_boxcox <- boxcox(lm(train_df$BLUEBOOK ~ 1))
bluebook_lambda <- bluebook_boxcox$x[which.max(bluebook_boxcox$y)]
bluebook_trans <- BoxCox(train_df$BLUEBOOK, bluebook_lambda)

## see what differences there are between using the Box-Cox and a regular log transformation
par(mfrow=c(1,3));hist(train_df$BLUEBOOK, breaks=100);hist(bluebook_trans, breaks=100);hist(log(train_df$BLUEBOOK), breaks=100);

## just use the Box Cox
train_df$BLUEBOOK <- bluebook_trans


## Transform TRAVTIME
travtime_boxcox <- boxcox(lm(train_df$TRAVTIME ~ 1))
travtime_lambda <- travtime_boxcox$x[which.max(travtime_boxcox$y)]
travtime_trans <- BoxCox(train_df$TRAVTIME, travtime_lambda)
par(mfrow=c(1,4));hist(train_df$TRAVTIME, breaks=100);hist(log(train_df$TRAVTIME), breaks=100);hist(travtime_trans, breaks=100);hist(sqrt(train_df$TRAVTIME),breaks=100)

## just use the Box Cox for TRAVTIME
train_df$TRAVTIME <- travtime_trans


## Create CAR_CRASH variable from the target flag for later exploration
train_df <- train_df %>% mutate(CAR_CRASH = ifelse(TARGET_FLAG == 1, 'Yes', 'No'))


## exploratory factor analysis for
##  KIDSDRIV; HOMEKIDS; CLM_FREQ; MVR_PTS; TIF; CAR_AGE

## put into bins:  TIF; CAR_AGE; HOMEKIDS; HOME_VAL
q <- quantile(train_df$CAR_AGE)
q <- c(-1,  1,  8, 12, 28)
train_df <- train_df %>% mutate(CAR_AGE_BIN = cut(CAR_AGE, breaks=q, labels=FALSE))

q <- quantile(train_df$HOME_VAL)
q[1] <- -1
train_df <- train_df %>% mutate(HOME_VAL_BIN = cut(HOME_VAL, breaks=q, labels=FALSE))

q <- quantile(train_df$TIF); q[1] <- -1
train_df <- train_df %>% mutate(TIF_BIN = cut(TIF, breaks=q, labels=FALSE))

a <- train_df %>% select_if(is.numeric) %>% dplyr::select(!(c(INDEX,TARGET_FLAG, TARGET_AMT))) %>% data.table::melt()
a %>% ggplot(aes(x=value))+ geom_density(alpha=.2,fill="#FF6666") + facet_wrap(~variable, scales='free')



