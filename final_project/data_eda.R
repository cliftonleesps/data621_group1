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
df_apps <- df_applications
df_tran <- df_transcripts

# save memory
remove(df_applications)
remove(df_transcripts)

# -----

# df_terms: table of term meta
# term_name, term_level, term_order, term_month, term_length (mos)
# term levels don't align to academic calendar order; add real dates and term lengths

term_name <- c()

for (v in val_labels(df_tran$term)){
  term_name <- append(term_name, val_label(df_tran$term,v))
}

term_level <- c(1,2,3,4,5,6) # original levels
term_order <- c(3,5,6,1,4,2) # academic calendar order
term_month <- c(1,6,7,9,5,12) # the actual calendar month
term_length <- c(4,1,1,3,0,0) # length of term (months)

df_term_meta <- data.frame(term_name, term_level, term_order, term_month, term_length)

write_csv(df_term_meta, file.path(local_data_dir,'data_model/df_term_meta.csv'))

# -----

# df_tran: add terms_order and terms_month to transcripts dataset
# create academic_year (Fall 2000 and Spring 2001 = Academic Year 2001)
# create terms_code for aggregations (academic_year & term_order)
# academic_year, term_order, term_month (month term started)

df_temp <- df_term_meta %>% select(term_level, term_order, term_month)

df_tran <- df_tran %>%
  left_join(df_temp, b = join_by(term==term_level)) %>%
  rename(calendar_year = year) %>%
  mutate(academic_year = ifelse(term_order < 3, calendar_year+1, calendar_year)) %>%
  mutate(term_code = paste(academic_year,term_order,sep='-'))

remove(df_temp)

# -----

# df_term_dates:
# table of term dates

df_temp <- df_term_meta %>% select(term_month, term_length)

df_term_dates <- df_tran %>%
  distinct(term_code, calendar_year, term_month) %>%
  left_join(df_temp) %>%
  mutate(term_start_month = ym(paste(calendar_year,term_month)),
         term_end_month = ym(paste(calendar_year,(term_month))) +
           months(term_length)) %>%
  select(term_code, term_start_month, term_end_month) %>%
  arrange(term_code)

remove(df_temp)

# -----

## let's brainstorm some response variables.


# df_resp_terms: when did a student start/complete, how long did it take?
# first_term_month, last_term_month, total_months

df_resp_terms <- df_tran %>%
  group_by(studentid_uniq) %>%
  summarize(first_term = min(term_code),
            last_term = max(term_code)) %>%
  left_join(df_term_dates, by=join_by(first_term==term_code)) %>%
  rename(first_term_month=term_start_month) %>%
  select(!term_end_month) %>%
  left_join(df_term_dates, by=join_by(last_term==term_code)) %>%
  rename(last_term_month=term_end_month) %>%
  select(!term_start_month) %>%
  mutate(total_months = interval(first_term_month, last_term_month) %/% months(1) + 1)

# -----

# df_resp_finalgpa: what was each student's final GPA?




# ----

# scratch

df_temp <- df_tran %>%
  filter(studentid_uniq == 'am_000002') %>%
  arrange(term_code)

df_temp2 <- df_transcripts %>%
  filter(studentid_uniq == 'am_000002')






