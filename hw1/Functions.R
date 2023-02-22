
library(dplyr)




#' Load the Training Stats File
#'
#' @return Dataframe with Stats
#' @export
#'
#' @examples
loadClassData <- function() {

  df <- read.csv("https://raw.githubusercontent.com/cliftonleesps/data621_group1/main/hw1/moneyball-training-data.csv")
  return (df)

}




#' Load Dataframe with Lahman Stats
#'
#' @return Dataframe with clean stats
#' @export
#'
#' @examples
loadLahmanData <- function() {


  teamDF <- read.csv("https://raw.githubusercontent.com/cliftonleesps/data621_group1/main/hw1/Teams.csv")

  teamDF <- teamDF %>% dplyr::select('yearID','lgID','teamID','franchID','divID','Rank','G','W','L',
                                     'R','AB','H','X2B','X3B','HR','BB','SO','SB','CS','HBP','SF','RA','ER','ERA',
                                     'CG','SHO','SV','IPouts','HA','HRA','BBA','SOA','E','DP','FP','name')




  # normalize data 162 games
  teamDF$gFactor <-  162 / (teamDF$W + teamDF$L)

  teamDF$TARGET_WINS <-  teamDF$gFactor * teamDF$W
  teamDF$TEAM_BATTING_H <- teamDF$gFactor * teamDF$H
  teamDF$TEAM_BATTING_2B  <- teamDF$gFactor * teamDF$X2B
  teamDF$TEAM_BATTING_3B  <- teamDF$gFactor * teamDF$X3B
  teamDF$TEAM_BATTING_HR  <- teamDF$gFactor * teamDF$HR
  teamDF$TEAM_BATTING_BB  <- teamDF$gFactor * teamDF$BB
  teamDF$TEAM_BATTING_SO  <- teamDF$gFactor * teamDF$SO
  teamDF$TEAM_BASERUN_SB  <- teamDF$gFactor * teamDF$SB
  teamDF$TEAM_BASERUN_CS  <- teamDF$gFactor * teamDF$CS
  teamDF$TEAM_BATTING_HBP <- teamDF$gFactor * teamDF$HBP
  teamDF$TEAM_PITCHING_H  <- teamDF$gFactor * teamDF$HA
  teamDF$TEAM_PITCHING_HR <- teamDF$gFactor * teamDF$HRA
  teamDF$TEAM_PITCHING_BB <- teamDF$gFactor * teamDF$BBA
  teamDF$TEAM_PITCHING_SO <- teamDF$gFactor * teamDF$SOA
  teamDF$TEAM_FIELDING_E  <- teamDF$gFactor * teamDF$E
  teamDF$TEAM_FIELDING_DP <- teamDF$gFactor * teamDF$DP


  # round values
  teamDF <- teamDF %>% mutate_if(is.numeric, round)

  # Pythagorean winning percentage
  #  (runs scored ^ 2) / [(runs scored ^ 2) + (runs allowed ^ 2)]
  teamDF$pythPercent <- (teamDF$R^2) / ((teamDF$R^2) + (teamDF$RA^2))

  teamDF <- teamDF %>% mutate(era_cat = case_when(yearID >= 1969 ~ '1969+',
                                                  yearID >= 1900 & yearID < 1969 ~ '1900-1969',
                                                  yearID < 1900 ~ '1900-'))

  return (teamDF)

}






#' Load Dataframe with Shorter version of Lahman Stats
#'
#' @return Dataframe with clean stat
#' @export
#'
#' @examples
loadLahmanShortData <- function() {

  df <- loadLahmanData()


  df <- df %>% dplyr::select("yearID","era_cat","TARGET_WINS","TEAM_BATTING_H","TEAM_BATTING_2B",
                                        "TEAM_BATTING_3B",
                                        "TEAM_BATTING_HR","TEAM_BATTING_BB","TEAM_BATTING_SO","TEAM_BASERUN_SB",
                                        "TEAM_BASERUN_CS","TEAM_BATTING_HBP","TEAM_PITCHING_H","TEAM_PITCHING_HR",
                                        "TEAM_PITCHING_BB", "TEAM_PITCHING_SO","TEAM_FIELDING_E","TEAM_FIELDING_DP")
  return (df)

}




#' Filter outliers using the min and max from a filter dataframe
#'
#' @param filter_df
#' @param df
#' @param columnNames
#'
#' @return
#' @export
#'
#' @examples
filterOutliers <- function(filter_df, df, columnNames) {

  filter_df <- select_if(filter_df, is.numeric)

  min_lst <- round(sapply(filter_df, min, na.rm = TRUE),0)
  max_lst <- round(sapply(filter_df, max, na.rm = TRUE),0)


  df$omit <- FALSE

  for (i in columnNames) {
    tryCatch(
      df$omit[df[i] < min_lst[[i]] | df[i] > max_lst[[i]]] <- TRUE
      ,

      error=function(e) e
    )
  }

  df <- df %>% dplyr::filter(!omit)
  df <- df %>% dplyr::select(-c(omit))
  return (df)
}





#' Print General Stats
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
printStats <- function(df) {


  st_options(
    dfSummary.custom.2 =
      expression(
        paste(
          "Q1 - Q3 :",
          round(
            quantile(column_data, probs = .25, type = 2,
                     names = FALSE, na.rm = TRUE), digits = 1
          ), " - ",
          round(
            quantile(column_data, probs = .75, type = 2,
                     names = FALSE, na.rm = TRUE), digits = 1
          )
        )
      )
  )


  print(
    dfSummary(df,
              varnumbers   = TRUE,
              na.col       = FALSE,
              style        = "grid",
              plain.ascii  = FALSE,
              freq.silent  = TRUE,
              headings     = TRUE,
              graph.magnif = .8,
              tmp.img.dir  = "/tmp"),
    method = "viewer"
  )

}




#' Print Density Plots
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
  printDensityPlot <- function(df) {

    m_df <- df %>%  melt()

    p1 <- m_df %>% ggplot(aes(x= value)) +
      geom_density(color='#023020', fill='gray') + facet_wrap(~variable, scales = 'free',  ncol = 4) + theme_bw()
    print(p1)

    p2 <- m_df %>% ggplot(aes(x = value)) +
      geom_boxplot(outlier.color = 'red', outlier.shape = 1) +
      facet_wrap(vars(variable),scales = "free", ncol = 4)

    print(p2)

  }






