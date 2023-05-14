## initial EDA
## run data_load.R and data_eda_transcripts.R first

## libraries
library(tidyverse)
library(labelled)
library(skimr)

## functions to inspect/convert labelled vectors
#
# val_label(df$var, index) # see one value/label pair
# val_labels(df$var) # see all value/label pairs
# unname(val_labels(df$var)) # see all values

get_labels <- function(vec){
  labels <- c() # init
  for (v in val_labels(vec)){labels <- append(labels, val_label(vec,v))}
  return(labels)
}

labelvec_to_df <- function(vec){
  return(data.frame(levels=unname(val_labels(vec)), values=get_labels(vec)))
}

## change to your local data dir outside the repo
local_data_dir <- '../../data/theop'

# load data
load(file.path(local_data_dir,'data_model/df_applications_orig.RData'))
load(file.path(local_data_dir,'data_model/df_meta_terms.RData'))

# working dfs
df_applications <- df_applications_orig

# save memory
remove(df_applications_orig)

# -----

df_skim_missing <- skim(df_applications) %>%
  dplyr::select(skim_variable, complete_rate, n_missing) %>%
  arrange(complete_rate,skim_variable)

# -----

## convert coded numerics into actuals

# -----

## convert termdes to 'academic term order' where Fall = 1
## merge in termapp for A&M ? 

val_labels(df_applications$termdes)
val_labels(df_applications$termapp)


df_qa <- df_applications %>%
  group_by(univ, termdes, termapp) %>%
  summarize(n()) %>%
  arrange(univ,termdes,termapp) %>%
  filter(univ=='amk')


df_qa2 <- df_applications %>%
  filter(univ=='amk', !is.na(termdes)) %>%
  group_by(termapp, termdes) %>%
  summarize(n())
  

# Texas A&M Kingsville application deadlines (2023)
# Fall	August 1
# Spring	December 1
# Summer	May 1




df_temp <- df_meta_terms %>% select(term_level, term_order)

df_applications <- df_applications %>%
  left_join(df_temp, by = join_by(term==term_level)) %>%


# -----

## actual dates

# yeardes is calendar year of desired admission - match to academic term to determine 'academic year of desired admission'

# gradyear is a calendar year -- add calendar_gradyear for 'academic year of graduation'

# -----

## test scores

# what adjustments needed for test scores, 'centered' test scores .. 



# -----

## misc

# does major_field align to field in transcripts data?

# should decile and quartile be converted from factor levels to actual numerics?

df_applications %>%
  group_by(univ,hscentury) %>%
  summarize(n())

df_applications %>%
  group_by(univ,hslos) %>%
  summarize(n())



# -----

## imputation

# can we reliably impute missing vals for 'male' and 'ethnic' using other variables with MICE?  

# can we impute missing 'admit' and 'enroll' from the transcripts?  





