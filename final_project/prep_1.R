# examine datasets, generate skim summaries

library(tidyverse)
library(haven)
library(skimr)

## challenge dataset
data_dir <- '../../data/future-of-families-challenge/FFChallenge_v5'
df_challenge <- read_csv(file.path(data_dir,'background.csv'))
df_challenge_skim <- skim(df_challenge)

sink(file='./df_challenge_skim.txt')
print(skim(df_challenge))
sink(file=NULL)


# baseline
data_dir <- ('../../data/future-of-families')
df_baseline <- read_sas(file.path(data_dir,'FF_wave1_2020v2_SAS_baseline.sas7bdat'))
df_baseline_skim <- skim(df_baseline)

sink(file='./df_baseline_skim.txt')
print(df_baseline_skim)
sink(file=NULL)

# year 1
data_dir <- ('../../data/future-of-families')
df_year1 <- read_sas(file.path(data_dir,'FF_wave2_2020v2_SAS_year1.sas7bdat'))
df_year1_skim <- skim(df_year1)

sink(file='./df_year1_skim.txt')
print(df_year1_skim)
sink(file=NULL)


