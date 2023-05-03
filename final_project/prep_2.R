library(tidyverse)

## challenge dataset
data_orig_dir <- '../../data/future-of-families-challenge/data_orig'
df_challenge <- read_csv(file.path(data_orig_dir,'background.csv'))

# suppressed variables
suppressed_vars <- scan(file=file.path(data_orig_dir,'constantVariables.txt'), 
                        what=character())

df_challenge_suppressed <- df_challenge[,!(names(df_challenge)) %in% suppressed_vars]

# save as RData
data_model_dir <- '../../data/future-of-families-challenge/data_model'
save(df_challenge_suppressed, file = file.path(data_model_dir,'df_challenge_suppressed.RData'))

