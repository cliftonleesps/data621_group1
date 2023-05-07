## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------------------------
# chunks
knitr::opts_chunk$set(echo=FALSE, eval=TRUE, include=TRUE, 
message=FALSE, warning=FALSE, fig.align='center', fig.height=5)

# libraries
library(yardstick)
library(tidyverse)
library(janitor)
library(mice)
library(readr)
library(skimr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(MASS)
library(forecast)
library(kableExtra)
library(ggpubr)
library(fastDummies)
library(jtools)
library(performance)
library(broom)
library(pROC)

library(ggmosaic)
library(patchwork)

library(caTools)
library(corrplot)
library(Hmisc)
library(glmnet)
library(vip)
library(caret)

library(psych)
library(GGally)
library(campfin)

library(summarytools)
library(sjPlot)
library(olsrr) 
library(car)

## To load the below libraries you might have to do the following in the console first:
####  install.packages("devtools")
####  install.packages("see")
####  devtools::install_github("haleyjeppson/ggmosaic")
####  devtools::install_github("thomasp85/patchwork")

# ggplot
theme_set(theme_bw())


## ----common functions-----------------------------------------------------------------------------------------------------------------------------------------
#' nice_table
#' 
#' @param df
#' @param fw
nice_table <- function(df, cap=NULL, cols=NULL, dig=3, fw=F){
  if (is.null(cols)) {c <- colnames(df)} else {c <- cols}
  table <- df %>% 
    kable(caption=cap, col.names=c, digits=dig) %>% 
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      html_font = 'monospace',
      full_width = fw)
  return(table)
}

#' glmnet_cv_aicc
#'
#' @param fit
#' @param lambda
#'
#' @return
#' @export
#'
#' @examples
glmnet_cv_aicc <- function(fit, lambda = 'lambda.min'){
  whlm <- which(fit$lambda == fit[[lambda]])
  with(fit$glmnet.fit,
       {
         tLL <- nulldev - nulldev * (1 - dev.ratio)[whlm]
         k <- df[whlm]
         n <- nobs
         return(list('AICc' = - tLL + 2 * k + 2 * k * (k + 1) / (n - k - 1),
                     'BIC' = log(n) * k - tLL))
       })
}

#' coeff2dt
#'
#' @param fitobject 
#' @param s 
#'
#' @return
#' @export
#'
#' @examples
coeff2dt <- function(fitobject, s) {
  coeffs <- coef(fitobject, s) 
  coeffs.dt <- data.frame(name = coeffs@Dimnames[[1]][coeffs@i + 1], coefficient = coeffs@x) 

  # reorder the variables in term of coefficients
  return(coeffs.dt[order(coeffs.dt$coefficient, decreasing = T),])
}


## # only required for final knit

## h4, h5, .h4, .h5 {margin-top: 20px !important;}


## ----data-----------------------------------------------------------------------------------------------------------------------------------------------------
# read train and evaluation datasets
train_df <- read.csv("https://raw.githubusercontent.com/cliftonleesps/data621_group1/main/hw4/insurance_training_data.csv")
eval_df <- read.csv("https://raw.githubusercontent.com/cliftonleesps/data621_group1/main/hw4/insurance-evaluation-data.csv")


## ----summary1-------------------------------------------------------------------------------------------------------------------------------------------------
DT::datatable(
      train_df[1:25,],
      extensions = c('Scroller'),
      options = list(scrollY = 350,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     dom = 'lBfrtip',
                     fixedColumns = TRUE, 
                     searching = FALSE), 
      rownames = FALSE) 


## ----summary-traindf------------------------------------------------------------------------------------------------------------------------------------------
skim_without_charts(train_df) %>%
  dplyr::select(c(skim_variable, skim_type, n_missing, complete_rate,
                  numeric.p0, numeric.p100, numeric.mean)) %>%
  nice_table(cap='Summary: Training Dataset',cols=c('variable','type','missing',
                                    'complete%','min','max','mean'))


## ----summary-evaldf-------------------------------------------------------------------------------------------------------------------------------------------
skim_without_charts(eval_df) %>%
  dplyr::select(c(skim_variable, skim_type, n_missing, complete_rate,
                  numeric.p0, numeric.p100, numeric.mean)) %>%
  nice_table(cap='Summary: Evaluation Dataset',cols=c('variable','type','missing',
                                      'complete%','min','max','mean'))


## ----prep-----------------------------------------------------------------------------------------------------------------------------------------------------
# Drop INDEX
train_df <- train_df %>% dplyr::select(-INDEX)
 eval_df <- eval_df %>% dplyr::select(-c(INDEX, TARGET_FLAG, TARGET_AMT))

## Remove z_ from character class values
z_vars <- c("MSTATUS","SEX","JOB","CAR_TYPE","URBANICITY","EDUCATION")
for (v in z_vars) {
  train_df <- train_df %>% mutate(!!v := str_replace(get(v),"z_",""))
  eval_df <- eval_df %>% mutate(!!v := str_replace(get(v),"z_",""))
}

# Update RED_CAR, replace [no,yes] values with [No, Yes] values
train_df <- train_df %>% mutate( RED_CAR = ifelse(RED_CAR == "no","No","Yes"))
eval_df <- eval_df %>% mutate( RED_CAR = ifelse(RED_CAR == "no","No","Yes"))

# Instead of level "" will be "Unknown"
train_df$JOB[train_df$JOB==""] <- "Unknown"
eval_df$JOB[eval_df$JOB==""] <- "Unknown"

# Convert currency columns from character class to integer
dollar_vars <- c("INCOME","HOME_VAL","BLUEBOOK","OLDCLAIM")
for (v in dollar_vars) {
  train_df <- train_df %>% mutate(!!v := str_replace(get(v),"[\\$,]",""))
  eval_df <- eval_df %>% mutate(!!v := str_replace(get(v),"[\\$,]",""))
}

currency_vars <- c("INCOME","HOME_VAL","BLUEBOOK","OLDCLAIM")

for (v in currency_vars) {
  train_df <- train_df %>% mutate(!!v := parse_number(get(v)))
  eval_df <- eval_df %>% mutate(!!v := parse_number(get(v)))
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
dist_df <- train_df %>% dplyr::select(where(is.numeric)) %>%
  pivot_longer(!c(TARGET_FLAG,TARGET_AMT), 
               names_to = 'variable', 
               values_to = 'value') %>% 
  drop_na()

dist_df %>% ggplot(aes(x=value, group=TARGET_FLAG, fill=TARGET_FLAG)) +
  geom_density(color='#023020') + 
  facet_wrap(~variable, scales = 'free',  ncol = 4) + 
  theme_bw()


## ----discrete_plot--------------------------------------------------------------------------------------------------------------------------------------------
train_df[,c("HOMEKIDS","KIDSDRIV", "CLM_FREQ")] %>%
  gather("variable","value") %>%
  group_by(variable) %>%
  count(value) %>%
  mutate(value = factor(value,levels=5:0)) %>%
  mutate(percent = n*100/8161) %>%
  ggplot(.,
  aes(variable,percent)) +
  geom_bar(stat = "identity", aes(fill = value)) +
  xlab("Variable") +
  ylab("Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = rev(c("#003f5c","#58508d", "#bc5090", "#ff6361", "#ffa600","#CC79a7")))


## ----vars_to_factors------------------------------------------------------------------------------------------------------------------------------------------
factor_vars <- c("PARENT1","CAR_TYPE","JOB","CAR_USE",
                 "URBANICITY","RED_CAR","REVOKED","MSTATUS","EDUCATION","SEX")

for (v in factor_vars) {
  train_df <- train_df %>% mutate(!!v := factor(get(v)))
  eval_df <- eval_df %>% mutate(!!v := factor(get(v)))
}

train_df$TARGET_FLAG <- as.factor(train_df$TARGET_FLAG)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
train_df[,factor_vars] %>%
  gather("variable","value") %>%
  group_by(variable) %>%
  count(value) %>%
  mutate(value = factor(value)) %>%
  mutate(percent = n*100/8161) %>%
  ggplot(.,
  aes(variable,percent)) +
  geom_bar(stat = "identity", aes(fill = value)) +
  xlab("Variable") +
  ylab("Percent") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## ----boxplot--------------------------------------------------------------------------------------------------------------------------------------------------
dist_df %>% ggplot(aes(x=value, group=TARGET_FLAG, fill=TARGET_FLAG)) + 
  geom_boxplot(color='#023020') + 
  facet_wrap(~variable, scales = 'free',  ncol = 4) + 
  theme_bw()


## ----scatterplot----------------------------------------------------------------------------------------------------------------------------------------------
dist_df %>% drop_na() %>% ggplot(aes(x=value, y=TARGET_AMT)) + 
  geom_smooth(method='glm', se=TRUE, na.rm=TRUE) +
  geom_point(color='#023020', alpha=0.5) + 
  facet_wrap(~variable, scales = 'free',  ncol = 4) + 
  theme_bw()


## ----corr-----------------------------------------------------------------------------------------------------------------------------------------------------
rcore <- rcorr(as.matrix(train_df %>% dplyr::select(where(is.numeric))))
coeff <- rcore$r
corrplot(coeff, tl.cex = .5, tl.col="black", method = 'color', addCoef.col = "black",
         type="upper", order="hclust", number.cex=0.7, diag=FALSE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## Check JOB values where YOJ==0 and INCOME is NA
# train_df %>% filter(YOJ == 0 & is.na(INCOME)) %>% dplyr::select(JOB) %>% unique()

# Manual correction of missing/outlier values
train_df <- train_df %>%
  mutate(CAR_AGE = ifelse(CAR_AGE < 0, 0, CAR_AGE)) %>% 
  mutate(INCOME = ifelse(YOJ == 0 & is.na(INCOME), 0, INCOME))

eval_df <- eval_df %>% 
  mutate(INCOME = ifelse(YOJ == 0 & is.na(INCOME), 0, INCOME))


## ----impute---------------------------------------------------------------------------------------------------------------------------------------------------
# MICE imputation of missing values
imputed_Data <- mice(train_df, m=5, maxit = 20, method = 'pmm', seed = 500, printFlag=FALSE)
train_df <- complete(imputed_Data)

eval_imputed_Data <- mice(eval_df, m=5, maxit = 20, method = 'pmm', seed = 500, printFlag=FALSE)
eval_df <- complete(eval_imputed_Data)


## ----dummy_vars-----------------------------------------------------------------------------------------------------------------------------------------------
## Dummy Variables for factors with two levels
dummy_vars <- function(df){
  df %>%
    mutate(
      MALE = factor(ifelse(SEX == "M", 1, 0)), 
      MARRIED = factor(ifelse(MSTATUS == "Yes", 1, 0)),
      LIC_REVOKED = factor(ifelse(REVOKED == "Yes", 1, 0)),
      CAR_RED = factor(ifelse(RED_CAR == "Yes", 1, 0)),
      PRIVATE_USE = factor(ifelse(CAR_USE == "Private", 1, 0)),
      SINGLE_PARENT = factor(ifelse(PARENT1 == "Yes", 1, 0)),
      URBAN = factor(ifelse(URBANICITY == "Highly Urban/ Urban", 1, 0)),
      KIDSDRIV = factor(ifelse(KIDSDRIV > 0, 1, 0)),
      HOMEKIDS = factor(ifelse(HOMEKIDS > 0, 1, 0))
    ) %>% 
    dplyr::select(-c(SEX, MSTATUS, REVOKED, RED_CAR, CAR_USE, PARENT1, URBANICITY))
      }

train_df <- dummy_vars(train_df)
eval_df <- dummy_vars(eval_df)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# keep original datasets for further lasso models, use transformed df for further transformations
train_transformed <- train_df
eval_transformed <- eval_df


## ----bin_vars-------------------------------------------------------------------------------------------------------------------------------------------------
## bin TIF, CAR_AGE, HOME_VAL
q <- quantile(train_transformed$CAR_AGE)
q <- c(-1,  1,  8, 12, 28)
train_transformed <- train_transformed %>% 
  mutate(CAR_AGE_BIN = cut(CAR_AGE, breaks=q, labels=FALSE))

q <- quantile(train_transformed$HOME_VAL)
q[1] <- -1
train_transformed <- train_transformed%>% 
  mutate(HOME_VAL_BIN = cut(HOME_VAL, breaks=q, labels=FALSE))

q <- quantile(train_transformed$TIF) 
q[1] <- -1
train_transformed <- train_transformed %>% 
  mutate(TIF_BIN = cut(TIF, breaks=q, labels=FALSE))

q <- quantile(eval_transformed$CAR_AGE)
q <- c(-1,  1,  8, 12, 28)
eval_transformed <- eval_transformed %>% 
  mutate(CAR_AGE_BIN = cut(CAR_AGE, breaks=q, labels=FALSE))

q <- quantile(eval_transformed$HOME_VAL)
q[1] <- -1
eval_transformed <- eval_transformed%>% 
  mutate(HOME_VAL_BIN = cut(HOME_VAL, breaks=q, labels=FALSE))

q <- quantile(eval_transformed$TIF) 
q[1] <- -1
eval_transformed <- eval_transformed %>% 
  mutate(TIF_BIN = cut(TIF, breaks=q, labels=FALSE))


## ----boxcox, fig.keep="none"----------------------------------------------------------------------------------------------------------------------------------
# use Box-Cox on BLUEBOOK, TRAVTIME, TIF
bluebook_boxcox <- boxcox(lm(train_transformed$BLUEBOOK ~ 1))
bluebook_lambda <- bluebook_boxcox$x[which.max(bluebook_boxcox$y)]
bluebook_trans <- BoxCox(train_transformed$BLUEBOOK, bluebook_lambda)
train_transformed$BLUEBOOK <- bluebook_trans

bluebook_boxcox <- boxcox(lm(eval_transformed$BLUEBOOK ~ 1))
bluebook_lambda <- bluebook_boxcox$x[which.max(bluebook_boxcox$y)]
bluebook_trans <- BoxCox(eval_transformed$BLUEBOOK, bluebook_lambda)
eval_transformed$BLUEBOOK <- bluebook_trans

travtime_boxcox <- boxcox(lm(train_transformed$TRAVTIME ~ 1))
travtime_lambda <- travtime_boxcox$x[which.max(travtime_boxcox$y)]
travtime_trans <- BoxCox(train_transformed$TRAVTIME, travtime_lambda)
train_transformed$TRAVTIME <- travtime_trans

travtime_boxcox <- boxcox(lm(eval_transformed$TRAVTIME ~ 1))
travtime_lambda <- travtime_boxcox$x[which.max(travtime_boxcox$y)]
travtime_trans <- BoxCox(eval_transformed$TRAVTIME, travtime_lambda)
eval_transformed$TRAVTIME <- travtime_trans

tif_boxcox <- boxcox(lm(train_transformed$TIF ~ 1))
tif_lambda <- tif_boxcox$x[which.max(tif_boxcox$y)]
tif_trans <- BoxCox(train_transformed$TIF, tif_lambda)
train_transformed$TIF <- tif_trans

tif_boxcox <- boxcox(lm(eval_transformed$TIF ~ 1))
tif_lambda <- tif_boxcox$x[which.max(tif_boxcox$y)]
tif_trans <- BoxCox(eval_transformed$TIF, tif_lambda)
eval_transformed$TIF <- tif_trans

## Use basic log transformation on TARGET_AMT, OLDCLAIM
train_transformed <- train_transformed %>% 
  mutate(TARGET_AMT = case_when(TARGET_FLAG==1 ~ log(TARGET_AMT) , TARGET_FLAG==0 ~ TARGET_AMT))
train_transformed$OLDCLAIM <- log(train_transformed$OLDCLAIM + 1)
train_transformed$INCOME <- log(train_transformed$INCOME + 1)

eval_transformed$OLDCLAIM <- log(eval_transformed$OLDCLAIM + 1)
eval_transformed$INCOME <- log(eval_transformed$INCOME + 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
skim_without_charts(train_transformed) %>%
  dplyr::select(c(skim_variable, skim_type, n_missing, complete_rate,
                  numeric.p0, numeric.p100, numeric.mean)) %>%
  nice_table(cap='Summary: Transformed Training Dataset',cols=c('variable','type','missing',
                                    'complete%','min','max','mean'))


## ----box_hists------------------------------------------------------------------------------------------------------------------------------------------------
## Create temporary CAR_CRASH variable from the target_flag for visualization
vis_df <- train_transformed %>% 
  mutate(CAR_CRASH = ifelse(TARGET_FLAG == 1, 1, 0)) %>% 
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(!c(CAR_AGE_BIN, HOME_VAL_BIN, TIF_BIN)) %>% # exclude binned
  pivot_longer(!c(CAR_CRASH,TARGET_AMT), 
               names_to = 'variable', 
               values_to = 'value') %>% 
  drop_na()

vis_df %>% ggplot(aes(x=value, group=CAR_CRASH, fill=CAR_CRASH)) +
  geom_density(color='#023020') +
  facet_wrap(~variable, scales = 'free',  ncol = 4) +
  theme_bw()


## ----split----------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1233)

# For Lasso Logistic model, original data without NAs, some vars to factors and opposite
log_original <- createDataPartition(train_df$TARGET_FLAG, p=0.75, list = FALSE)

train_data <- train_df[log_original, ] %>% dplyr::select(-c(TARGET_AMT))
valid_data <- train_df[-log_original, ] %>% dplyr::select(-c(TARGET_AMT))

# For other logistic models, transformed data, removed bin variables
log_transformed <- createDataPartition(train_transformed$TARGET_FLAG, p=0.75, list = FALSE)

log_train <- train_transformed[log_transformed,] %>% 
  dplyr::select(-c(TARGET_AMT, TIF, CAR_AGE, HOME_VAL))

log_test <- train_transformed[-log_transformed,] %>% 
  dplyr::select(-c(TARGET_AMT, TIF, CAR_AGE, HOME_VAL))

# For Lasso Linear model, original data without NAs, some vars to factors and opposite, only for TARGET_FLAG=1
original_yes_crash <- train_df %>% 
  filter(TARGET_FLAG==1)

transformed_yes_crash <- train_transformed %>% 
  filter(TARGET_FLAG==1) 

linear_original <- createDataPartition(original_yes_crash$TARGET_AMT, p=0.75, list = FALSE)

linear_train_data <- original_yes_crash[linear_original, ] %>% 
  dplyr::select(-c(TARGET_FLAG))

linear_valid_data <- original_yes_crash[-linear_original, ] %>% 
  dplyr::select(-c(TARGET_FLAG))

# For other linear models, transformed data, removed bin variables
linear_transformed <- createDataPartition(transformed_yes_crash$TARGET_AMT, p=0.75, list = FALSE)

amt_train <- transformed_yes_crash[linear_transformed, ] %>% 
  dplyr::select(-c(TARGET_FLAG, TIF, CAR_AGE, HOME_VAL))

amt_test <- transformed_yes_crash[-linear_transformed, ] %>% 
  dplyr::select(-c(TARGET_FLAG, TIF, CAR_AGE, HOME_VAL))

eval_transformed <- eval_transformed %>% 
  dplyr::select(-c(TIF, CAR_AGE, HOME_VAL))


## ----log_model1, warning=FALSE, message=FALSE-----------------------------------------------------------------------------------------------------------------
log_model_1 <- glm(train_data, formula = TARGET_FLAG ~ ., family = binomial(link = "logit"))


## ----log_model1_aic, warning=FALSE, message=FALSE-------------------------------------------------------------------------------------------------------------
log_model_1_aic <- log_model_1 %>% stepAIC(trace = FALSE)
summ(log_model_1_aic)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
as.data.frame(summ(log_model_1_aic)$coeftable) %>%
  dplyr::select(Est.,p) %>%
  filter(p < 0.06 & Est. > 0.2) %>%
  arrange(desc(Est.)) %>%
  nice_table(cap='Positive Coefficients', cols=c('Est','p'))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
as.data.frame(summ(log_model_1_aic)$coeftable) %>%
  dplyr::select(Est.,p) %>%
  filter(p < 0.06 & Est. < -0.2) %>%
  arrange(Est.) %>%
  nice_table(cap='Negative Coefficients', cols=c('Est','p'))


## ----log_model_1_confusionMatrix, fig.height=4----------------------------------------------------------------------------------------------------------------
log_testing <- valid_data

log1_pred <- predict.glm(log_model_1_aic, log_testing, "response")

log_testing$prob_model_1 <- log1_pred
log_testing$pred_model_1 <- ifelse(log1_pred >= 0.5, 1, 0)

# Confusion Matrix
log_matrix_1 <- confusionMatrix(factor(log_testing$pred_model_1), 
                                factor(log_testing$TARGET_FLAG), "1")

log_matrix_1

# Table
results_log <- tibble(
  Model = "Model #1: Original data", 
  Accuracy=log_matrix_1$overall["Accuracy"],
  "Classification error rate" = 1 - log_matrix_1$overall["Accuracy"],
  F1 = log_matrix_1$byClass[7],
  Deviance= log_model_1_aic$deviance,
  R2 = 1 - log_model_1_aic$deviance / log_model_1_aic$null.deviance,
  Sensitivity = log_matrix_1$byClass["Sensitivity"],
  Specificity = log_matrix_1$byClass["Specificity"],
  Precision = precision(factor(log_testing$pred_model_1), 
                        factor(log_testing$TARGET_FLAG), "1"),
  AUC = auc(log_testing$TARGET_FLAG, log_testing$pred_model_1),
  AIC= log_model_1_aic$aic
  )

# ROC Curve
plot(roc(log_testing$TARGET_FLAG, 
         log_testing$pred_model_1, 
         plot = FALSE, print.auc = TRUE), 
     quiet=TRUE, main="Model 1 - ROC Curve")


## ----check_log1-----------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(log_model_1_aic)


## ----model_1_residuals, fig.height=4--------------------------------------------------------------------------------------------------------------------------
log_model_1_aic.data <- augment(log_model_1_aic) %>% 
  mutate(index = 1:n())

ggplot(log_model_1_aic.data, aes(index, .std.resid)) + 
  geom_point(aes(color = TARGET_FLAG), alpha = .5) +
  theme_bw()


## ----warning=FALSE, message=FALSE-----------------------------------------------------------------------------------------------------------------------------
car::mmps(log_model_1_aic, span = 3/4, layout = c(2, 2))


## ----log_model_1_vif------------------------------------------------------------------------------------------------------------------------------------------
as.data.frame(car::vif(log_model_1_aic)) %>% 
  arrange(desc(GVIF)) %>%
  nice_table(cap='Variance Inflation Factor', cols=c('GVIF','df','adj'))


## ----general_box, warning=FALSE, message=FALSE----------------------------------------------------------------------------------------------------------------
log_model_2 <- glm(log_train, formula = TARGET_FLAG ~ . , family = binomial(link = "logit"))


## ----log_model2_aic, warning=FALSE, message=FALSE-------------------------------------------------------------------------------------------------------------
log_model_2_aic <- log_model_2 %>% stepAIC(trace = FALSE)
summ(log_model_2_aic)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
as.data.frame(summ(log_model_2_aic)$coeftable) %>%
  dplyr::select(Est.,p) %>%
  filter(p < 0.06 & Est. > 0.2) %>%
  arrange(desc(Est.)) %>%
  nice_table(cap='Positive Coefficients')


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
as.data.frame(summ(log_model_2_aic)$coeftable) %>%
  dplyr::select(Est.,p) %>%
  filter(p < 0.06 & Est. < -0.2) %>%
  arrange(Est.) %>%
  nice_table(cap='Negative Coefficients')


## ----log_model_2_confusionMatrix, fig.height=4----------------------------------------------------------------------------------------------------------------
testing2 <- log_test
#testing2$model_2 <- ifelse(predict.glm(log_model_2_aic, testing2 , "response") >= 0.5, 1, 0)

log2_pred <- predict.glm(log_model_2_aic, testing2, "response")

testing2$prob_model_2  <- log2_pred
testing2$pred_model_2 <- ifelse(log2_pred >= 0.5, 1, 0)

log_matrix_2 <- confusionMatrix(factor(testing2$pred_model_2), factor(testing2$TARGET_FLAG), "1")

results_log <- rbind(results_log, tibble(Model = "Model #2: Transformed data", Accuracy=log_matrix_2$overall["Accuracy"], 
                  "Classification error rate" = 1 - log_matrix_2$overall["Accuracy"],
                  F1 = log_matrix_2$byClass[7],
                  Deviance= log_model_2_aic$deviance, 
                  R2 = 1 - log_model_2_aic$deviance / log_model_2_aic$null.deviance,
                  Sensitivity = log_matrix_2$byClass["Sensitivity"],
                  Specificity = log_matrix_2$byClass["Specificity"],
                  Precision = precision(factor(testing2$pred_model_2), factor(testing2$TARGET_FLAG), "1"),
                  AUC = auc(testing2$TARGET_FLAG, testing2$pred_model_2),
                  AIC= log_model_2_aic$aic))

log_matrix_2

plot(roc(testing2$TARGET_FLAG, testing2$pred_model_2, plot = FALSE, print.auc = TRUE), 
     quiet=TRUE, main="Model 2 - ROC Curve")


## ----check_log2-----------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(log_model_2_aic)


## ----model_2_residuals, fig.height=4--------------------------------------------------------------------------------------------------------------------------
log_model_2_aic.data <- augment(log_model_2_aic) %>% 
  mutate(index = 1:n())

ggplot(log_model_2_aic.data, aes(index, .std.resid)) + 
  geom_point(aes(color = TARGET_FLAG), alpha = .5) +
  theme_bw()


## ----warning=FALSE, message=FALSE-----------------------------------------------------------------------------------------------------------------------------
car::mmps(log_model_2_aic, span = 3/4, layout = c(2, 2))


## ----log_model_2_vif------------------------------------------------------------------------------------------------------------------------------------------
as.data.frame(car::vif(log_model_2_aic)) %>% 
  arrange(desc(GVIF)) %>%
  nice_table(cap='Variance Inflation Factor', cols=c('GVIF','df','adj'))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
t_df <- train_data

# build X matrix and Y vector
X <- model.matrix(TARGET_FLAG ~ ., data=t_df)[,-1]
Y <- t_df[,"TARGET_FLAG"] 


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
lasso.log.model<- cv.glmnet(
  x=X,y=Y,
  family = "binomial",
  link = "logit",
  standardize = TRUE,
  nfold = 5,
  alpha=1) # alpha=1 is lasso

lasso.log.model

l.min <- lasso.log.model$lambda.min
l.1se <- lasso.log.model$lambda.1se


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# drop TARGET_AMT
t0_df <- valid_data 

# build X matrix and Y vector
X_test <- model.matrix(TARGET_FLAG ~ ., data=t0_df)[,-1]
Y_test <- t0_df[,"TARGET_FLAG"] 

# predict using coefficients at lambda.min
lassoPred <- predict(lasso.log.model, newx = X_test, type = "response", s = 'lambda.min')

pred_log_df <- valid_data %>% drop_na()
pred_log_df$TARGET_FLAG_PROB <- lassoPred[,1]
pred_log_df$TARGET_FLAG_PRED <- ifelse(lassoPred > 0.5, 1, 0)[,1]


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
confusion.glmnet(lasso.log.model, newx = X_test, newy = Y_test, s = 'lambda.min')


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
assess.glmnet(lasso.log.model,           
                newx = X,              
                newy = Y,
                family = "binomial",
                s = 'lambda.min'
              )    

print(glmnet_cv_aicc(lasso.log.model, 'lambda.min'))
print(glmnet_cv_aicc(lasso.log.model, 'lambda.1se'))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------

conf_matrix_3 = confusionMatrix(factor(pred_log_df$TARGET_FLAG_PRED),factor(pred_log_df$TARGET_FLAG), "1")

# build X matrix and Y vector
t0_df <- valid_data %>% drop_na()
X_test <- model.matrix(TARGET_FLAG ~ ., data=t0_df)[,-1]
Y_test <- t0_df[,"TARGET_FLAG"] 

lasso.r1 <- assess.glmnet(lasso.log.model,           
                                newx = X_test,              
                                newy = Y_test,
                                family = "binomial",
                                s = 'lambda.min'
                          )   
lasso.r2 <- glmnet_cv_aicc(lasso.log.model, 'lambda.min')

results_log <- rbind(results_log,tibble(Model = "Model #3: Lasso", Accuracy=conf_matrix_3$overall[1], 
                  "Classification error rate" = 1 - conf_matrix_3$overall[1],
                  F1 = conf_matrix_3$byClass[7],
                  Deviance=lasso.r1$deviance[[1]], 
                  R2 = NA,
                  Sensitivity = conf_matrix_3$byClass["Sensitivity"],
                  Specificity = conf_matrix_3$byClass["Specificity"],
                  Precision = conf_matrix_3$byClass["Precision"],
                  AUC=lasso.r1$auc[1], 
                  AIC= lasso.r2$AICc))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))

plot(lasso.log.model)
plot(lasso.log.model$glmnet.fit, xvar="lambda", label=TRUE)
plot(lasso.log.model$glmnet.fit, xvar='dev', label=TRUE)

rocs <- roc.glmnet(lasso.log.model, newx = X, newy = Y )
plot(rocs,type="l")  


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
as.data.frame(as.matrix(coef(lasso.log.model, s = "lambda.min"))) %>%
  arrange(desc(s1)) %>%
  nice_table(cap='Model Coefficients', cols=c('Est'))


## ---- fig.height=4--------------------------------------------------------------------------------------------------------------------------------------------
vip(lasso.log.model, num_features=20 ,geom = "col", include_type=TRUE, lambda = "lambda.min")

coeffs.table <- coeff2dt(fitobject = lasso.log.model, s = "lambda.min")

coeffs.table %>% mutate(name = fct_reorder(name, desc(coefficient))) %>%
ggplot() +
  geom_col(aes(y = name, x = coefficient, fill = {coefficient > 0})) +
  xlab(label = "") +
  ggtitle(expression(paste("Lasso Coefficients with ", lambda, " = 0.0275"))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),legend.position = "none")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
results_log %>% 
  nice_table(cap='Logistic Model Comparison') %>% 
  scroll_box(width='100%')


## ----model_1--------------------------------------------------------------------------------------------------------------------------------------------------
df_train <- linear_train_data
df_test <- linear_valid_data

# build model
model_1 <- lm(TARGET_AMT ~ ., data = df_train)


## ----model_1_aic----------------------------------------------------------------------------------------------------------------------------------------------
# select features and refit by stepwise selection (AIC)
model_1_aic <- model_1 %>% stepAIC(trace = FALSE)
summ(model_1_aic, digits = getOption("jtools-digits", 4))


## ---- fig.height=4--------------------------------------------------------------------------------------------------------------------------------------------
# validate and calculate RMSE
model_1_aic.valid <- predict(model_1_aic, newdata = df_test)
model_1_aic.eval <- bind_cols(target = df_test$TARGET_AMT, predicted=unname(model_1_aic.valid))
model_1_aic.rmse <- sqrt(mean((model_1_aic.eval$target - model_1_aic.eval$predicted)^2)) 

# plot targets vs predicted
model_1_aic.eval %>%
  ggplot(aes(x = target, y = predicted)) +
  geom_point(alpha = .3) +
  geom_smooth(method="lm", color='grey', alpha=.3, se=FALSE) +
  labs(title=paste('RMSE:',round(model_1_aic.rmse,1)))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
multi_metric <- metric_set(mape, smape, mase, mpe, yardstick::rmse)
model1_df <- model_1_aic.eval %>% multi_metric(truth=target, estimate=predicted)
a <- summary(model_1_aic)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
results_lm_tbl <- tibble(
                      Model = character(),
                      mape = numeric(), 
                      smape = numeric(), 
                      mase = numeric(), 
                      mpe = numeric(), 
                      "RMSE" = numeric(),
                      AIC = numeric(),
                      "Adjusted R2" = numeric(),
                      "F-statistic" = numeric()
                )

results_lm_tbl <- results_lm_tbl %>% add_row(tibble_row(
                      Model = "Model 1: Original data",
                      mape = model1_df[[1,3]],
                      smape = model1_df[[2,3]],
                      mase = model1_df[[3,3]],
                      mpe = model1_df[[4,3]],
                      "RMSE" = model1_df[[5,3]],
                      AIC = AIC(model_1_aic)),
                      "Adjusted R2" = a$adj.r.squared,
                      "F-statistic" = a$fstatistic[1]
                     )


## ----check_lm1------------------------------------------------------------------------------------------------------------------------------------------------
check_model(model_1_aic, check=c('ncv','qq','homogeneity','outliers'))


## ----vif_lm1, fig.height=4------------------------------------------------------------------------------------------------------------------------------------
vif_values <- vif(model_1_aic)
vif_values <- rownames_to_column(as.data.frame(vif_values), var = "var")

vif_values %>%
  ggplot(aes(y=vif_values, x=var)) +
  coord_flip() + 
  geom_hline(yintercept=5, linetype="dashed", color = "red") +
  geom_bar(stat = 'identity', width=0.3 ,position=position_dodge()) 


## ----model_2--------------------------------------------------------------------------------------------------------------------------------------------------
# build model
model_2 <- lm(TARGET_AMT ~ ., data = amt_train)


## ----model_2_aic----------------------------------------------------------------------------------------------------------------------------------------------
# select features and refit by stepwise selection (AIC)
model_2_aic <- model_2 %>% stepAIC(trace = FALSE)
summ(model_2_aic, digits = getOption("jtools-digits", 4))


## ----fig.height=4---------------------------------------------------------------------------------------------------------------------------------------------
# validate and calculate RMSE
model_2_aic.valid <- predict(model_2_aic, newdata = amt_test)
model_2_aic.eval <- bind_cols(target = amt_test$TARGET_AMT, predicted=model_2_aic.valid)
model_2_aic.rmse <- sqrt(mean((model_2_aic.eval$target - model_2_aic.eval$predicted)^2)) 

# plot targets vs predicted
model_2_aic.eval %>%
  ggplot(aes(x = target, y = predicted)) +
  geom_point(alpha = .3) +
  geom_smooth(method="lm", color='grey', alpha=.3, se=FALSE) +
  labs(title=paste('RMSE:',round(model_2_aic.rmse,1)))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
model2_df <-model_2_aic.eval %>% multi_metric(truth=target, estimate=predicted)

b <- summary(model_2_aic)

results_lm_tbl <- results_lm_tbl %>% add_row(tibble_row(
                      Model = "Model 2: Transforms + Stepwise",
                      mape = model2_df[[1,3]],
                      smape = model2_df[[2,3]],
                      mase = model2_df[[3,3]],
                      mpe = model2_df[[4,3]],
                      "RMSE" = model2_df[[5,3]],
                      AIC = AIC(model_2_aic)),
                      "Adjusted R2" = b$adj.r.squared,
                      "F-statistic" = b$fstatistic[1]
                     )


## ----check_lm2------------------------------------------------------------------------------------------------------------------------------------------------
check_model(model_2_aic, check=c('ncv','qq','homogeneity','outliers'))


## ----vif_lm2, fig.height=4------------------------------------------------------------------------------------------------------------------------------------

vif_values <- vif(model_2_aic)
vif_values <- rownames_to_column(as.data.frame(vif_values), var = "var")

vif_values %>%
  ggplot(aes(y=GVIF, x=var)) +
  coord_flip() + 
  geom_hline(yintercept=5, linetype="dashed", color = "red") +
  geom_bar(stat = 'identity', width=0.3 ,position=position_dodge()) 


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# filter the target data
t_df <- linear_train_data 

# build X matrix and Y vector
X <- model.matrix(TARGET_AMT ~ ., data=t_df)[,-1]
Y <- t_df[,"TARGET_AMT"] 


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
lasso.lm.model <- cv.glmnet(
  x=X,y=Y, # Y already logged in prep
  family = "gaussian",
  type.measure="mse",
  standardize = TRUE, # standardize
  nfold = 10,
  alpha=1) # alpha=1 is lasso

lasso.lm.model

l.min <- lasso.lm.model$lambda.min
l.1se <- lasso.lm.model$lambda.1se


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))

plot(lasso.lm.model)
plot(lasso.lm.model$glmnet.fit, xvar="lambda", label=TRUE)
plot(lasso.lm.model$glmnet.fit, xvar='dev', label=TRUE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
assess.glmnet(lasso.lm.model,           
              newx = X,              
              newy = Y,
              s = 'lambda.min',
              family = 'gaussian')    

print(glmnet_cv_aicc(lasso.lm.model, 'lambda.min'))
print(glmnet_cv_aicc(lasso.lm.model, 'lambda.1se'))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
as.data.frame(as.matrix(coef(lasso.lm.model, s = "lambda.min"))) %>%
  arrange(desc(s1)) %>%
  nice_table(cap='Model Coefficients', cols='Est')


## ----fig.height=3---------------------------------------------------------------------------------------------------------------------------------------------
vip(lasso.lm.model, num_features=20 ,geom = "col", include_type=TRUE, lambda = "lambda.min")

coeffs.table <- coeff2dt(fitobject = lasso.lm.model, s = "lambda.min")

coeffs.table %>% mutate(name = fct_reorder(name, desc(coefficient))) %>%
ggplot() +
  geom_col(aes(y = name, x = coefficient, fill = {coefficient > 0})) +
  xlab(label = "") +
  ggtitle(expression(paste("Lasso Coefficients with ", lambda, " = 0.0275"))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),legend.position = "none")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
t0_df <- linear_valid_data 
X_test <- model.matrix(TARGET_AMT ~ ., data=t0_df)[,-1]
Y_test <- t0_df[,"TARGET_AMT"] 

lassoPred <- predict(
  lasso.lm.model, 
  newx = X_test,
  type = "response",
  s = 'lambda.min')

pred_ln_df <- linear_valid_data
pred_ln_df$TARGET_AMT_PRED_RAW <- lassoPred[,1]
pred_ln_df$TARGET_AMT_PRED <- ifelse(lassoPred > 100, lassoPred, 0)[,1]


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
pred_ln_df %>% multi_metric(truth=TARGET_AMT, estimate=TARGET_AMT_PRED)


## ----fig.height=3---------------------------------------------------------------------------------------------------------------------------------------------
n <- nrow(t0_df)
x <- t0_df$TARGET_AMT
e <- lassoPred - t0_df$TARGET_AMT

plot(x, e,  
     xlab = "cost", 
     ylab = "residuals", 
     bg = "steelblue", 
     col = "darkgray", cex = 1.5, pch = 21, frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n) 
  lines(c(x[i], x[i]), c(e[i], 0), col = "red" , lwd = 1)

# comparative histogram
t0_df <- pred_ln_df %>% dplyr::select(TARGET_AMT,TARGET_AMT_PRED) %>% pivot_longer(c(TARGET_AMT,TARGET_AMT_PRED),
                                 names_to='variable' , values_to = 'value')

t0_df %>% dplyr::filter(value <= 1000) %>% 
  ggplot(aes(x=value, fill=variable)) +geom_histogram(bins=5) 

t0_df %>% dplyr::filter(value > 1000) %>% 
  ggplot(aes(x=value, fill=variable)) +geom_histogram(bins=100) 


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# build X matrix and Y vector
t0_df <- linear_valid_data 
X_test <- model.matrix(TARGET_AMT ~ ., data=t0_df)[,-1]
Y_test <- t0_df[,"TARGET_AMT"] 

lasso.r1 <- assess.glmnet(lasso.lm.model,
                                newx = X_test,
                                newy = Y_test )
lasso.r2 <- glmnet_cv_aicc(lasso.lm.model, 'lambda.min')


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
m_df <- pred_ln_df %>% multi_metric(truth=TARGET_AMT, estimate=TARGET_AMT_PRED)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
results_lm_tbl <- results_lm_tbl %>% add_row(tibble_row(
                      Model = "Model 3: Lasso",
                      mape = m_df[[1,3]],
                      smape = m_df[[2,3]],
                      mase = m_df[[3,3]],
                      mpe = m_df[[4,3]],
                      "RMSE" = m_df[[5,3]],
                      AIC = lasso.r2$AICc,
                      "Adjusted R2" = NA,
                      "F-statistic" = NA
                     ))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
pred_ln_df$PER_ERR <- (pred_ln_df$TARGET_AMT_PRED - pred_ln_df$TARGET_AMT)


## ----fig.height=4---------------------------------------------------------------------------------------------------------------------------------------------
pred_ln_df %>% ggplot(aes(x=TARGET_AMT, colour=TARGET_AMT_PRED, y=PER_ERR)) +
  geom_point()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
results_lm_tbl %>% 
  nice_table(cap='Linear Model Comparison') 


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# build X matrix and Y vector
eval_df$TARGET_FLAG <- 0
X_eval <- model.matrix(TARGET_FLAG ~ ., data=eval_df)[,-1]


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# predict using coefficients at lambda.min
lassoPred <- predict(lasso.log.model, newx = X_eval, type = "response", s = 'lambda.min')


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
eval_pred_df <- eval_df
eval_pred_df$TARGET_FLAG_PROB <- lassoPred[,1]
eval_pred_df$TARGET_FLAG_PRED <- ifelse(lassoPred > 0.5, 1, 0)[,1]
eval_pred_df <- subset(eval_pred_df, select = -c(TARGET_FLAG))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
eval_transformed$TARGET_FLAG_PRED <- eval_pred_df$TARGET_FLAG_PRED
eval_transformed$TARGET_FLAG_PROB <- round(eval_pred_df$TARGET_FLAG_PROB, digits=4)

# from predictions by model_2_aic we get ln(TARGET_AMT), to get TARGET_AMT, we take exp() of the predicted values
eval_transformed <- eval_transformed %>% 
  mutate(TARGET_AMT_PRED = case_when(TARGET_FLAG_PRED==1 ~ exp(predict(object = model_2_aic, newdata=eval_transformed)), TARGET_FLAG_PRED==0 ~ 0))

eval_transformed$TARGET_AMT_PRED <- round(eval_transformed$TARGET_AMT_PRED, digits=4)

# reorder predictions to front
eval_transformed <- eval_transformed %>% 
  relocate(TARGET_FLAG_PRED, TARGET_AMT_PRED, TARGET_FLAG_PROB)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
predictions <- eval_transformed %>% 
  dplyr::select(c(TARGET_FLAG_PRED, TARGET_AMT_PRED, TARGET_FLAG_PROB))


## ----eval=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------
## write.csv(eval_transformed, 'eval_predictions.csv', row.names=F)
## write.csv(predictions, 'predictions_only.csv', row.names=F)


## ----results_table--------------------------------------------------------------------------------------------------------------------------------------------
DT::datatable(
      eval_transformed,
      extensions = c('Scroller'),
      options = list(scrollY = 350,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     dom = 'lBfrtip',
                     fixedColumns = TRUE, 
                     searching = FALSE), 
      rownames = FALSE) 

