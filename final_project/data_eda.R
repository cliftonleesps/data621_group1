## initial EDA
## run data_load.R first

# libraries
library(tidyverse)
library(labelled)

## change to your local data dir outside the repo
local_data_dir <- '../../data/theop'

# load data
load(file.path(local_data_dir,'data_model/df_applications_orig.RData'))
load(file.path(local_data_dir,'data_model/df_transcripts_orig.RData'))

# working dfs
df_applications <- df_applications_orig
df_transcripts <- df_transcripts_orig

# save memory
remove(df_applications_orig)
remove(df_transcripts_orig)

# -----

# df_terms: table of term meta
# term_name, term_level, term_order, term_month, term_length (mos)
# term levels don't align to academic calendar order; add term order, start months and lengths

term_name <- c()

for (v in val_labels(df_transcripts$term)){
  term_name <- append(term_name, val_label(df_transcripts$term,v))
}

term_level <- c(1,2,3,4,5,6) # original levels
term_order <- c(3,5,6,1,4,2) # academic calendar order
term_month <- c(1,6,7,9,5,12) # the actual calendar month
term_length <- c(4,1,1,3,0,0) # length of term (months)

df_meta_terms <- data.frame(term_name, term_level, term_order, term_month, term_length)
save(df_meta_terms, file=file.path(local_data_dir,'data_model/df_meta_terms.RData'))

# -----

# df_tran: add fields to sort by academic calendar, add actual dates, add cumulative hours
#
# create academic_year (Fall 2000 and Spring 2001 = Academic Year 2001)
# create terms_code for aggregations (academic_year & term_order)
# add cumulative sums for hrearn and gpahrs
#
# academic_year, term_order, term_month, chrearn, cgpahrs

df_temp <- df_meta_terms %>% select(term_level, term_order, term_month)

df_transcripts <- df_transcripts %>%
  left_join(df_temp, b = join_by(term==term_level)) %>%
  rename(calendar_year = year) %>%
  mutate(academic_year = ifelse(term_order < 3, calendar_year+1, calendar_year)) %>%
  mutate(term_code = paste(academic_year,term_order,sep='-')) %>%
  arrange(studentid_uniq, term_code) %>%
  group_by(studentid_uniq) %>%
  mutate(chrearn = cumsum(hrearn), cgpahrs = cumsum(gpahrs))

save(df_transcripts, file=file.path(local_data_dir,'data_model/df_transcripts.RData'))
remove(df_temp)

# -----

# df_meta_terms_dates:
# table of term dates

df_temp <- df_meta_terms %>% select(term_month, term_length)

df_meta_terms_dates <- df_transcripts %>%
  ungroup() %>%
  distinct(term_code, calendar_year, term_month) %>%
  left_join(df_temp) %>%
  mutate(term_start_month = ym(paste(calendar_year,term_month)),
         term_end_month = ym(paste(calendar_year,(term_month))) +
           months(term_length)) %>%
  select(term_code, term_start_month, term_end_month) %>%
  arrange(term_code)

save(df_meta_terms_dates, file=file.path(local_data_dir,'data_model/df_meta_terms_dates.RData'))
remove(df_temp)

# -----

## let's brainstorm some response variables.

# -----

# df_resp_terms: when did a student start/complete their program, how long did it take?
# first_term_month, last_term_month, total_months

df_resp_terms <- df_transcripts %>%
  group_by(studentid_uniq) %>%
  summarize(first_term = min(term_code),
            last_term = max(term_code)) %>%
  left_join(df_meta_terms_dates, by=join_by(first_term==term_code)) %>%
  rename(first_term_month=term_start_month) %>%
  select(!term_end_month) %>%
  left_join(df_meta_terms_dates, by=join_by(last_term==term_code)) %>%
  rename(last_term_month=term_end_month) %>%
  select(!term_start_month) %>%
  mutate(total_months = interval(first_term_month, last_term_month) %/% months(1) + 1)

save(df_resp_terms, file=file.path(local_data_dir,'data_model/df_resp_terms.RData'))

# -----

# df_resp_finalscores: what was each student's final GPA, hours earned, GPA hours?

df_temp <- df_transcripts %>%
  select(studentid_uniq, term_code, cgpa, chrearn, cgpahrs)

df_resp_finalscores <- df_transcripts %>%
  select(studentid_uniq, term_code) %>%
  group_by(studentid_uniq) %>%
  summarize(term_code=max(term_code)) %>%
  left_join(df_temp, by = join_by(studentid_uniq, term_code))

save(df_resp_finalscores, file=file.path(local_data_dir,'data_model/df_resp_finalscores.RData'))
remove(df_temp)

# ----

# df_resp_switchmajor: did a student switch majors during their program? (1/0 and n_count)

df_resp_switchmajor <- df_transcripts %>%
  select(studentid_uniq, term_major_dept, term_major_field) %>%
  group_by(studentid_uniq) %>%
  summarize(n_field = n_distinct(term_major_field), n_dept = n_distinct(term_major_dept)) %>%
  mutate(switch_field = ifelse(n_field > 1,1,0),
         switch_dept = ifelse(n_dept > 1,1,0),
         switch_major = ifelse((switch_field + switch_dept) > 0,1,0))

save(df_resp_switchmajor, file=file.path(local_data_dir,'data_model/df_resp_switchmajor.RData'))

# scratch







