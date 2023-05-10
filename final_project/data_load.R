## initial load of all applications & transcript files
## run this first

# libraries
library(tidyverse)
library(haven) # .dta

## change to your local data dir outside the repo
local_data_dir <- '../../data/theop'

## applications
df_applications_orig <- data.frame()
cat = 'data_applications'

files <- list.files(path = file.path(local_data_dir,cat), recursive=TRUE,
                    pattern = '\\.dta$', full.names = TRUE)

for (f in files){
  df <- as.data.frame(read_dta(f))
  univ <- str_extract(f,'theop_(.+?)_',1) # university code
  df$studentid_uniq <- paste(univ,df$studentid,sep='_') # unique student id
  df$univ <- univ
  df_applications_orig <- bind_rows(df_applications_orig,df)
}

# save to data_model folder
save(df_applications_orig,file=file.path(local_data_dir,'data_model/df_applications_orig.RData'))


## transcripts
df_transcripts_orig <- data.frame()
cat = 'data_transcripts'

files <- list.files(path = file.path(local_data_dir,cat), recursive=TRUE,
                    pattern = '\\.dta$', full.names = TRUE)

for (f in files){
  df <- as.data.frame(read_dta(f))
  univ <- str_extract(f,'theop_(.+?)_',1)
  df$studentid_uniq <- paste(univ,df$studentid,sep='_')
  df$univ <- univ
  df_transcripts_orig <- bind_rows(df_transcripts_orig,df)
}

# save to data_model folder
save(df_transcripts_orig,file=file.path(local_data_dir,'data_model/df_transcripts_orig.RData'))

# save memory
remove(df_applications_orig, df_transcripts_orig, df)
