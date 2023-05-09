## initial EDA

# libraries
library(tidyverse)
library(labelled)

## change to your local data dir outside the repo
local_data_dir <- '../../data/theop'

# load data
load(file.path(local_data_dir,'data_model/df_applications.RData'))
load(file.path(local_data_dir,'data_model/df_transcripts.RData'))

# working dfs
df_app <- df_applications
df_tran <- df_transcripts

# term
val_labels(df_tran$term)

# create a new 'calendar term' in chrono order
df_tran <- df_tran %>%
  mutate(term_chron = paste(year, case_match(term,1~4,2~5,3~6,4~1,5~4,6~2), sep='-'))

# table of final terms
df_tran_final_terms <- df_tran %>%
  group_by(studentid_uniq) %>%
  summarize(term_chron = max(term_chron))

# table of final terms and gpas
df_tran_final_gpas <- df_tran_final_terms %>%
  inner_join(df_tran)





