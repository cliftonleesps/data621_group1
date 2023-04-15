library(tidyverse)
library(haven)
library(skimr)

setwd('/Users/jeff/Documents/Learning/CUNY/Classes/DATA 621 - Modeling/Final Project/data/future-of-families/')

# baseline
df_baseline <- read_sas('FF_wave1_2020v2_SAS_baseline.sas7bdat')
df_baseline_skim <- skim(df_baseline)

sink(file='df_baseline_skim.txt')
print(df_baseline_skim)
sink(file=NULL)

# year 1
df_year1 <- read_sas('FF_wave2_2020v2_SAS_year1.sas7bdat')
df_year1_skim <- skim(df_year1)

sink(file='df_year1_skim.txt')
print(df_year1_skim)
sink(file=NULL)

## Challenge dataset

setwd('/Users/jeff/Documents/Learning/CUNY/Classes/DATA 621 - Modeling/Final Project/data/future-of-families-challenge/FFChallenge_v5')

df_challenge <- read_csv('background.csv')

sink(file='df_challenge_skim.txt')
print(skim(df_challenge))
sink(file=NULL)

