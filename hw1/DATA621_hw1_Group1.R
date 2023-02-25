## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------
library(tidyverse)
library(reshape2)
library(kableExtra)
library(Matrix)
library(MASS)
library(mice)
library(Hmisc)
library(corrplot)
library(performance)
library(naniar)
library(psych)
library(GGally)
library(campfin)
library(caret)
library(yardstick)
library(summarytools)
library(sjPlot)
library(car)
library(rsample)
library(olsrr) 

knitr::opts_chunk$set(echo = F, 
                      warning = F, 
                      message = F, 
                      eval = T , 
                      results="asis", 
                      fig.height=6, 
                      fig.width=8)
set.seed(1234)


## ----functions------------------------------------------------------------------------------------------------------------------

st_css()

 st_options(
   plain.ascii = FALSE,
   style = 'grid',
   dfSummary.style ='grid',
   freq.silent  = TRUE,
   headings     = FALSE,
   tmp.img.dir  = "./tmp",
   dfSummary.custom.1 =
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

source('./hw1/Functions.R', local = knitr::knit_global())
 


## ----load_data------------------------------------------------------------------------------------------------------------------
trainDf <- read.csv("https://raw.githubusercontent.com/cliftonleesps/data621_group1/main/hw1/moneyball-training-data.csv")
evalDf <- read.csv("https://raw.githubusercontent.com/cliftonleesps/data621_group1/main/hw1/moneyball-evaluation-data.csv")


## ----df_summary-----------------------------------------------------------------------------------------------------------------
print(
  dfSummary(trainDf, 
            varnumbers   = TRUE,
            na.col       = TRUE,
            graph.magnif = .8,
            tmp.img.dir  = "/tmp"),
  method = "render"
)



## ----density_plot---------------------------------------------------------------------------------------------------------------
m_df <- trainDf %>% dplyr::select(-c(INDEX)) %>% melt() 

m_df %>% ggplot(aes(x= value)) + 
geom_density(color='#023020', fill='gray') + facet_wrap(~variable, scales = 'free',  ncol = 4) + theme_bw()


## ----boxplot--------------------------------------------------------------------------------------------------------------------

m_df %>% ggplot(aes(x = value)) +
  geom_boxplot(outlier.color = 'red', outlier.shape = 1) +
  facet_wrap(vars(variable),scales = "free", ncol = 4)



## ----feature_plot---------------------------------------------------------------------------------------------------------------
featurePlot(trainDf[,2:ncol(trainDf)], trainDf[,1], plot = "scatter", type = c("p", "smooth"), span = 1)


## ----correlation_matrix---------------------------------------------------------------------------------------------------------
rcore <- rcorr(as.matrix(trainDf %>% dplyr::select(where(is.numeric) & -INDEX & -TEAM_BATTING_HBP)))
coeff <- rcore$r
corrplot(coeff, tl.cex = .7 , method = 'circle')


## ----corr_numbers---------------------------------------------------------------------------------------------------------------
tst <- trainDf
tst <- tst[,-1 ]
kable(cor(drop_na(tst))[,1], "html", escape = F, col.names = c('Coefficient')) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(1, bold = T)


## ----missing_data,fig.height=4--------------------------------------------------------------------------------------------------
gg_miss_var(trainDf, show_pct=TRUE)


## ----dropped_df-----------------------------------------------------------------------------------------------------------------
droppedDf <- trainDf %>% dplyr::select(-c(INDEX, TEAM_BATTING_HBP))


## ----missing_flags--------------------------------------------------------------------------------------------------------------
columnNames <- names(droppedDf)
droppedDf <- flag_na(droppedDf, columnNames)
columnNames <- names(evalDf)
evalDf <- flag_na(evalDf, columnNames)


## ----density_plots_post,fig.height=4--------------------------------------------------------------------------------------------
imputeDf <- mice(droppedDf, m = 5, maxit = 50, seed = 123, printFlag = F)
cleanDf <- complete(imputeDf)
m_imputed <- melt(cleanDf)
densityplot(imputeDf)


## ----drop_na_flag---------------------------------------------------------------------------------------------------------------
cleanDf <- cleanDf %>% dplyr::select(-na_flag)
droppedDf <- droppedDf %>% dplyr::select(-na_flag)


## ----drop_outliers--------------------------------------------------------------------------------------------------------------

# list of columns included in in the outlier filtering
columnNames <- c('TEAM_BATTING_H','TEAM_BATTING_2B','TEAM_BATTING_3B','TEAM_BATTING_HR',
                 'TEAM_BATTING_BB','TEAM_BATTING_SO','TEAM_BASERUN_SB','TEAM_BASERUN_CS',
                 'TEAM_PITCHING_H','TEAM_PITCHING_HR','TEAM_PITCHING_BB','TEAM_PITCHING_SO',
                 'TEAM_FIELDING_E','TEAM_FIELDING_DP','TEAM_BATTING_1B')

# Filter 
filterDf <- loadLahmanShortData() %>% filter(yearID >= 1900)
droppedDf <- filterOutliers(filterDf, cleanDf, columnNames)

# print stats for new model
print(
  dfSummary(droppedDf, 
            varnumbers   = TRUE,
            na.col       = TRUE,
            graph.magnif = .8,
            tmp.img.dir  = "/tmp"),
  method = "render"
)

m_df <- droppedDf %>% melt() 

m_df %>% ggplot(aes(x= value)) + 
    geom_density(color='#023020', fill='gray') + facet_wrap(~variable, scales = 'free',  ncol = 4) + theme_bw()

m_df %>% ggplot(aes(x = value)) +
  geom_boxplot(outlier.color = 'red', outlier.shape = 1) +
  facet_wrap(vars(variable),scales = "free", ncol = 4)



## ----single_base_hits-----------------------------------------------------------------------------------------------------------
droppedDf <- droppedDf %>% mutate(TEAM_BATTING_1B = TEAM_BATTING_H - TEAM_BATTING_2B - TEAM_BATTING_3B - TEAM_BATTING_HR)


## ----transform_nonnormal--------------------------------------------------------------------------------------------------------
# Reminder of our distributions
m_imputed %>% ggplot(aes(x= value)) + 
    geom_density(fill='gray') + facet_wrap(~variable, scales = 'free', ncol=4) +
    theme(strip.text.x = element_text(size = 6, angle = 0)) +
    labs(title='Training w/Imputed Missing Values')

# Transform some non-normal variables.
transformed_df <- cleanDf |> mutate_each(funs(sqrt),
                   batting_hr_sqrt = TEAM_BATTING_HR,
                   batting_so_sqrt = TEAM_BATTING_SO,
                   baserun_cs_sqrt = TEAM_BASERUN_CS,
                   pitching_hr_sqrt = TEAM_PITCHING_HR) 

visualize_transformed <- transformed_df |> melt()

# Plot.
visualize_transformed %>% ggplot(aes(x= value)) + 
    geom_density(fill='gray') + facet_wrap(~variable, scales = 'free', ncol=4) +
    theme(strip.text.x = element_text(size = 6, angle = 0)) +
    labs(title='Training w/Imputed Missing Values + SQRT Transform')


## ----df_summary_normalized------------------------------------------------------------------------------------------------------
print(
  dfSummary(transformed_df, 
            varnumbers   = TRUE,
            na.col       = TRUE,
            graph.magnif = .8,
            tmp.img.dir  = "/tmp"),
  method = "render")



## ----lm1------------------------------------------------------------------------------------------------------------------------
trainDf_1 <- trainDf %>% dplyr::select(-c(INDEX, TEAM_BATTING_HBP))
lm1 <- lm(TARGET_WINS ~ . , data = trainDf_1)
lm1Sum <- summary(lm1)
tab_model(lm1Sum, show.df = FALSE, show.aic = TRUE, show.fstat=TRUE, show.se = TRUE, show.ci=FALSE, show.stat=TRUE, digits.p=4)


## ----check_lm1------------------------------------------------------------------------------------------------------------------
check_model(lm1, check=c('ncv','qq','homogeneity','outliers'))


## ----lm2------------------------------------------------------------------------------------------------------------------------
lm2 <- lm(formula = TARGET_WINS ~ TEAM_PITCHING_BB  + TEAM_BATTING_2B + TEAM_BATTING_3B +
   TEAM_FIELDING_E + TEAM_PITCHING_H + TEAM_BATTING_HR + TEAM_BATTING_H, data = cleanDf)
lm2Sum <- summary(lm2)
tab_model(lm2Sum, show.df = FALSE, show.aic = TRUE, show.fstat=TRUE, show.se = TRUE, show.ci=FALSE, show.stat=TRUE, digits.p=4)


## ----check_lm2------------------------------------------------------------------------------------------------------------------
check_model(lm2, check=c('ncv','qq','homogeneity','outliers'))


## ----lm3------------------------------------------------------------------------------------------------------------------------

lm3 <- lm(TARGET_WINS ~ . -TEAM_BATTING_H, data=droppedDf, by=na_flag)
lm3step <- stepAIC(lm3, trace = FALSE)
lm3sum <- summary(lm3step)
tab_model(lm3step, show.df = FALSE, show.aic = TRUE, show.fstat=TRUE, show.se = TRUE, show.ci=FALSE, show.stat=TRUE, digits.p=4)



## ----check_lm3------------------------------------------------------------------------------------------------------------------
check_model(lm3step, check=c('ncv','qq','homogeneity','outliers'))


## ----bptest_lm3-----------------------------------------------------------------------------------------------------------------
lmtest::bptest(lm3step)


## ----vif_lm3--------------------------------------------------------------------------------------------------------------------

vif_values <- vif(lm3step)
vif_values <- rownames_to_column(as.data.frame(vif_values), var = "var")

vif_values %>%
  ggplot(aes(y=vif_values, x=var)) +
  coord_flip() + 
  geom_hline(yintercept=5, linetype="dashed", color = "red") +
  geom_bar(stat = 'identity', width=0.3 ,position=position_dodge()) 

df <- droppedDf[ , vif_values$var] %>% drop_na()
coeff <- cor(df)
corrplot(coeff, tl.cex = .7 , diag = FALSE ,type = 'upper' ,method = 'number')



## ----lm3_final------------------------------------------------------------------------------------------------------------------
lm3step.final <- update(lm3step, . ~ . -TEAM_PITCHING_BB -TEAM_BATTING_BB -TEAM_PITCHING_SO - -TEAM_BATTING_SO -TEAM_PITCHING_H -TEAM_BATTING_2B)
lm3finalsum <- summary(lm3step.final)
tab_model(lm3step.final, show.df = FALSE, show.aic = TRUE, show.fstat=TRUE, show.se = TRUE, show.ci=FALSE, show.stat=TRUE, digits.p=4)


## ----check_lm3_final------------------------------------------------------------------------------------------------------------
check_model(lm3step.final, check=c('ncv','qq','homogeneity','outliers'))


## ----vif_lm3_final--------------------------------------------------------------------------------------------------------------

vif_values <- vif(lm3step.final)
vif_values <- rownames_to_column(as.data.frame(vif_values), var = "var")

vif_values %>%
  ggplot(aes(y=vif_values, x=var)) +
  coord_flip() + 
  geom_hline(yintercept=5, linetype="dashed", color = "red") +
  geom_bar(stat = 'identity', width=0.3 ,position=position_dodge()) 

df <- droppedDf[ , vif_values$var] %>% drop_na()
coeff <- cor(df)
corrplot(coeff, tl.cex = .7 , diag = FALSE ,type = 'upper' ,method = 'number')



## ----bptest_lm3_final-----------------------------------------------------------------------------------------------------------
lmtest::bptest(lm3step.final)


## ----tab_lm3_final--------------------------------------------------------------------------------------------------------------
tab_model(lm3, lm3step, lm3step.final, 
          dv.labels = c('model 3','model 3 (StepAIC)','model 3 (Final)'),
          show.df = FALSE, show.aic = TRUE, show.fstat=TRUE, show.se = TRUE, show.ci=FALSE, show.stat=TRUE, digits.p=4)


## ----lm4------------------------------------------------------------------------------------------------------------------------
lm4 <- lm(TARGET_WINS~. -TEAM_PITCHING_HR -TEAM_BATTING_SO -TEAM_BASERUN_CS -TEAM_BATTING_HR -TEAM_PITCHING_BB -TEAM_PITCHING_H -pitching_hr_sqrt, data=transformed_df)
lm4step <- stepAIC(lm4, trace=FALSE)
lm4sum <- summary(lm4step)
tab_model(lm4step, show.df = FALSE, show.aic = TRUE, show.fstat=TRUE, show.se = TRUE, show.ci=FALSE, show.stat=TRUE, digits.p=4)


## ----check_lm4------------------------------------------------------------------------------------------------------------------
check_model(lm4step, check=c('ncv','qq','homogeneity','outliers'))


## ----compare_models-------------------------------------------------------------------------------------------------------------
plot(compare_performance(lm1,lm2,lm3step,lm4step, rank=T))


## ----compare_r2-----------------------------------------------------------------------------------------------------------------
r <- c(lm1Sum$r.squared, lm2Sum$r.squared, lm3sum$r.squared, lm4sum$r.squared)
mse <- c(lm1Sum$sigma, lm2Sum$sigma, lm3sum$sigma, lm4sum$sigma)
adjusted.r <- c(lm1Sum$adj.r.squared, lm2Sum$adj.r.squared, lm3sum$adj.r.squared, lm4sum$adj.r.squared)
modelDf <- data.frame(r,mse,adjusted.r)
kable(modelDf, caption = "Moneyball Dataset", digits = 2, format = "html", row.names = F) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = F,
    position = "center"
  )


## ----predict_setup--------------------------------------------------------------------------------------------------------------
#removed columns
evalDroppedDf <- evalDf %>% dplyr::select(-c(INDEX, TEAM_BATTING_HBP))
evalDroppedDf <- evalDroppedDf %>% mutate(TEAM_BATTING_1B = TEAM_BATTING_H - TEAM_BATTING_2B - TEAM_BATTING_3B - TEAM_BATTING_HR)

#no NA's
evalcleanDf <- mice(evalDroppedDf, m = 5, maxit = 50, seed = 123, printFlag = F)
evalcleanDf <- complete(evalcleanDf)

#with transformations
transformed_eval <- evalcleanDf |> mutate_each(funs(sqrt),
                   batting_hr_sqrt = TEAM_BATTING_HR,
                   batting_so_sqrt = TEAM_BATTING_SO,
                   baserun_cs_sqrt = TEAM_BASERUN_CS,
                   pitching_hr_sqrt = TEAM_PITCHING_HR)



## ----predict--------------------------------------------------------------------------------------------------------------------
lm1Pred <- lm1 %>% predict(evalcleanDf)
lm2Pred <- lm2 %>% predict(evalcleanDf)
aiclm3 <- lm3step %>% predict(evalcleanDf)
aiclm4 <- lm4step %>% predict(transformed_eval)


## ----predict_report-------------------------------------------------------------------------------------------------------------
predsDf <- evalDf %>% 
  mutate(lm1 = lm1Pred, lm2 = lm2Pred, aic3 = aiclm3, aic4 = aiclm4) %>%
  dplyr::select(c(lm1, lm2, aic3, aic4))
  kable(head(predsDf, 10), caption = "Moneyball Dataset", digits = 2, format = "html", row.names = F) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = F,
    position = "center"
  )


## ----predict_plot---------------------------------------------------------------------------------------------------------------
xT <- evalcleanDf %>% 
  mutate(lm1 = lm1Pred, lm2 = lm2Pred, aic3 = aiclm3, aic4 = aiclm4)
par(mfrow=c(2,2))
plot(1:nrow(xT), xT$lm1, xlab="LM1", ylab="Wins", title="LM1")
plot(1:nrow(xT), xT$lm2, xlab="LM2", ylab="Wins", title="LM2")
plot(1:nrow(xT), xT$aic, xlab="AIC LM3", ylab="Wins", title="AIC LM3")
plot(1:nrow(xT), xT$aic4, xlab="AIC LM4", ylab="Wins", title="AIC LM4")


## ----lahman_df------------------------------------------------------------------------------------------------------------------
teamDF <- loadLahmanData()


## ----lahmanshort_df-------------------------------------------------------------------------------------------------------------
teamAdjDF <- loadLahmanShortData()

random_sample <- createDataPartition(teamAdjDF$TARGET_WINS,p = 0.8, list = FALSE)

trainingTeam_df <- teamAdjDF[random_sample, ]
testingTeam_df <- teamAdjDF[-random_sample, ]


## ----lahman_summary-------------------------------------------------------------------------------------------------------------
print(
  dfSummary(teamDF, 
            varnumbers   = TRUE,
            na.col       = TRUE,
            graph.magnif = .8,
            tmp.img.dir  = "/tmp"),
  method = "render")


## ----lahman_plot----------------------------------------------------------------------------------------------------------------
m_df <- teamAdjDF[random_sample, ] %>% melt() 

m_df %>% ggplot(aes(x= value)) + 
      geom_density(color='#023020', fill='gray') + 
      facet_wrap(~variable, scales = 'free',  ncol = 4) + 
      theme_bw() +
      labs(title = 'Variable Density Plots')


## ----lahman_density-------------------------------------------------------------------------------------------------------------
m_df <- teamAdjDF[random_sample, ] %>% filter(era_cat == '1969+') %>% melt() 

m_df %>% ggplot(aes(x= value)) + 
      geom_density(color='#023020', fill='gray') + 
      facet_wrap(~variable, scales = 'free',  ncol = 4) + 
      theme_bw() +
      labs(title = 'Variable Density Plots (1969+)')


## ----lahman_training_summary----------------------------------------------------------------------------------------------------
print(
  dfSummary(trainingTeam_df, 
            varnumbers   = TRUE,
            na.col       = TRUE,
            graph.magnif = .8,
            tmp.img.dir  = "/tmp"),
  method = "render")


## ----lahman_training_plot-------------------------------------------------------------------------------------------------------
m_df <- trainingTeam_df %>% melt() 

m_df %>% ggplot(aes(x= value)) + 
    geom_density(color='#023020', fill='gray') + facet_wrap(~variable, scales = 'free',  ncol = 4) + theme_bw()


m_df %>% ggplot(aes(x = value)) +
  geom_boxplot(outlier.color = 'red', outlier.shape = 1) +
  facet_wrap(vars(variable),scales = "free", ncol = 4)


## ----lahman_training_corr-------------------------------------------------------------------------------------------------------
rcore <- rcorr(as.matrix(trainingTeam_df %>% dplyr::select(where(is.numeric))))
coeff <- rcore$r
corrplot(coeff, tl.cex = .7 , method = 'pie')


## ----lahman_prep_data-----------------------------------------------------------------------------------------------------------
trainingTeam_df <- trainingTeam_df %>% dplyr::select(-c(TEAM_BASERUN_CS,TEAM_BATTING_HBP))
testingTeam_df <- testingTeam_df %>% dplyr::select(-c(TEAM_BASERUN_CS,TEAM_BATTING_HBP))


## ----lahman_prep_impute---------------------------------------------------------------------------------------------------------
imputeDf <- mice(trainingTeam_df, m = 5, maxit = 50, seed = 123, printFlag = F)
#imputeDf$meth
trainingTeam_df <- complete(imputeDf)
m_imputed <- melt(trainingTeam_df)
densityplot(imputeDf)


## ----lahman_lma1----------------------------------------------------------------------------------------------------------------
lmA1 <- lm(TARGET_WINS ~ . -yearID, data = trainingTeam_df, by=era_cat)
tab_model(lmA1, show.df = FALSE, show.aic = TRUE, show.fstat=TRUE, show.se = TRUE, show.ci=FALSE, show.stat=TRUE, digits.p=4)


## ----lahman_lma1_step-----------------------------------------------------------------------------------------------------------
lmA1.step <- stepAIC(lmA1, trace = FALSE, by=era_cat)
tab_model(lmA1.step, show.df = FALSE, show.aic = TRUE, show.fstat=TRUE, show.se = TRUE, show.ci=FALSE, show.stat=TRUE, digits.p=4)


## ----lahman_summary_lm1---------------------------------------------------------------------------------------------------------
lmA1.final <- update(lmA1.step, . ~ . -TEAM_BATTING_3B)
tab_model(lmA1.final, show.df = FALSE, show.aic = TRUE, show.fstat=TRUE, show.se = TRUE, show.ci=FALSE, show.stat=TRUE, digits.p=4)


## ----lahman_check_lm1-----------------------------------------------------------------------------------------------------------
check_model(lmA1.final, check=c('ncv','qq','homogeneity','outliers'))


## ----lahman_predict-------------------------------------------------------------------------------------------------------------
imputeDf <- mice(testingTeam_df, m = 5, maxit = 50, seed = 123, printFlag = F)
#imputeDf$meth
testingTeam_df <- complete(imputeDf)
m_imputed <- melt(testingTeam_df)
densityplot(imputeDf)


## ----lahman_predict_final-------------------------------------------------------------------------------------------------------
lmA1Pred.final <- lmA1.final %>% predict(testingTeam_df)


## ----lahman_predict_report------------------------------------------------------------------------------------------------------
multi_metric <- metric_set(mape, smape, mase, mpe, rmse, rsq)
m <- testingTeam_df %>% multi_metric(truth=testingTeam_df$TARGET_WINS, estimate=lmA1Pred.final)

kable(m, digits = 4, format = "html", row.names = T) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = F,
    position = "center"
  )


## ----lahman_predict_plot--------------------------------------------------------------------------------------------------------
n <- nrow(testingTeam_df)
x <- testingTeam_df$TARGET_WINS
e <- lmA1Pred.final - testingTeam_df$TARGET_WINS 


plot(x, e,  
     xlab = "wins", 
     ylab = "residuals",
     bg = "steelblue", 
     col = "darkgray", cex = 1.5, pch = 21, frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n) 
  lines(c(x[i], x[i]), c(e[i], 0), col = "red" , lwd = 1)


## ----py_df----------------------------------------------------------------------------------------------------------------------
random_sample <- createDataPartition(teamDF$TARGET_WINS, p = 0.8, list = FALSE)

trainingTeam_df <- teamDF[random_sample, ]
testingTeam_df <- teamDF[-random_sample, ]


## ----py_lm----------------------------------------------------------------------------------------------------------------------
lmp <- lm(TARGET_WINS ~ pythPercent, data = trainingTeam_df)

tab_model(lmp, show.df = FALSE, show.aic = TRUE, show.fstat=TRUE, show.se = TRUE, show.ci=FALSE, show.stat=TRUE, digits.p=4)


## ----py_check_lm----------------------------------------------------------------------------------------------------------------
check_model(lmp, check=c('ncv','qq','homogeneity','outliers'))


## ----py_predict-----------------------------------------------------------------------------------------------------------------
lmpPred <- lmp %>% predict(testingTeam_df)
hist(lmpPred)


## ----py_predict_report----------------------------------------------------------------------------------------------------------
m1 <- testingTeam_df %>% multi_metric(truth=testingTeam_df$TARGET_WINS, estimate=lmpPred)

kable(m1, digits = 4, format = "html", row.names = T) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = F,
    position = "center"
  )


## ----py_predict_plot------------------------------------------------------------------------------------------------------------
n <- nrow(testingTeam_df)
x <- testingTeam_df$TARGET_WINS
e <- lmpPred - testingTeam_df$TARGET_WINS 

plot(x, e,  
     xlab = "wins", 
     ylab = "residuals", 
     bg = "steelblue", 
     col = "darkgray", cex = 1.5, pch = 21, frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n) 
  lines(c(x[i], x[i]), c(e[i], 0), col = "red" , lwd = 1)

