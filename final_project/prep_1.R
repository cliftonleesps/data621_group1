# examine datasets, generate skim summaries

library(tidyverse)
library(haven)
library(skimr)

# add data directories to a local parent folder .. don't save to github
data_orig_dir <- '../../data/future-of-families-challenge/data_orig'
data_model_dir <- '../../data/future-of-families-challenge/data_model'

## challenge dataset
df_challenge <- read_csv(file.path(data_orig_dir,'background.csv'))
df_challenge_skim <- skim(df_challenge)

sink(file=file.path(data_model_dir,'df_challenge_skim.txt'))
print(skim(df_challenge))
sink(file=NULL)




