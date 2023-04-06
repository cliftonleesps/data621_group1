library(tidyverse)
library(dplyr)
library(mice)
library(readr)

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


a <- train_df %>% dplyr::select(c(4,5,6,7,8,10,15,17,18,22,24,25)) %>% data.table::melt()
a %>% ggplot(aes(x=value))+ geom_density(alpha=.2,fill="#FF6666") + facet_wrap(~variable, scales='free')


# use Box-Cox on INCOME, TRAVTIME, BLUEBOOK, TIF
train_df$INCOME <- BoxCox(train_df$INCOME, BoxCox.lambda(train_df$INCOME))

#i <- BoxCox(train_df$TRAVTIME, BoxCox.lambda(train_df$TRAVTIME))
#hist(i,breaks=50)

#i <- BoxCox(train_df$BLUEBOOK, BoxCox.lambda(train_df$BLUEBOOK))
#i <- BoxCox(train_df$TIF, BoxCox.lambda(train_df$TIF))


## Create CAR_CRASH variable from the target flag for later exploration
train_df <- train_df %>% mutate(CAR_CRASH = ifelse(TARGET_FLAG == 1, 'Yes', 'No'))
