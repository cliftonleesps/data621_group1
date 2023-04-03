## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggplot2)
library(glmnet)
library(psych)
library(caret)
library(pROC)
library(skimr)
library(lemon)
library(DT)
library(kableExtra)
library(forecast)
library(corrplot)
library(ggpubr)
library(Hmisc)
library(caTools)
library(performance)
library(jtools)
library(regclass)
library(MASS)
library(PerformanceAnalytics)
library(leaps)
library(broom)
library(vip)

source("logistic_functions.R")

knitr::opts_chunk$set(echo = FALSE)


## ------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1233)


## ----data----------------------------------------------------------------------------------------------------------------------------------
# read train and evaluation datasets
train_df <- read.csv("https://raw.githubusercontent.com/cliftonleesps/data621_group1/main/hw3/crime-training-data_modified.csv")
eval_df <- read.csv("https://raw.githubusercontent.com/cliftonleesps/data621_group1/main/hw3/crime-evaluation-data_modified.csv")



## ----summary1------------------------------------------------------------------------------------------------------------------------------
DT::datatable(
      train_df,
      extensions = c('Scroller'),
      options = list(scrollY = 350,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     dom = 'lBfrtip',
                     fixedColumns = TRUE, 
                     searching = FALSE), 
      rownames = FALSE) 


## ----summary-------------------------------------------------------------------------------------------------------------------------------
skim_without_charts(train_df)


## ----density_plot--------------------------------------------------------------------------------------------------------------------------

m_df <- train_df %>% pivot_longer(!target, names_to='variable' , values_to = 'value')
m_df %>% ggplot(aes(x=value, group=target, fill=target)) + 
geom_density(color='#023020') + facet_wrap(~variable, scales = 'free',  ncol = 4) + theme_bw()

m_df %>% ggplot(aes(x= value)) + 
geom_density(color='#023020', fill='gray') + facet_wrap(~variable, scales = 'free',  ncol = 4) + theme_bw()



## ----box_plot------------------------------------------------------------------------------------------------------------------------------

m_df %>% ggplot(aes(x=target, y=value, group=target)) + 
geom_boxplot(color='#023020', fill='gray') + facet_wrap(~variable, scales = 'free',  ncol = 4) +
  stat_summary(fun = "mean",  geom = "point", shape = 8, size = 2, color = "steelblue") + 
  stat_summary(fun = "median",  geom = "point", shape = 8, size = 2, color = "red") + theme_bw()



## ----balance-------------------------------------------------------------------------------------------------------------------------------
proportion <- colMeans(train_df['target'])
proportion


## ----corr----------------------------------------------------------------------------------------------------------------------------------

rcore <- rcorr(as.matrix(train_df %>% dplyr::select(where(is.numeric))))
coeff <- rcore$r
corrplot(coeff, tl.cex = .7, tl.col="black", method = 'color', addCoef.col = "black",
         type="upper", order="hclust",
         diag=FALSE)



## ----corr_numbers--------------------------------------------------------------------------------------------------------------------------
tst <- train_df
tst <- tst[,13]
kable(cor(drop_na(train_df))[,13], "html", escape = F, col.names = c('Coefficient')) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(1, bold = T)


## ----factor--------------------------------------------------------------------------------------------------------------------------------
transformed_train_df <- train_df
transformed_train_df$chas <- as.factor(transformed_train_df$chas)
transformed_train_df$target <- as.factor(transformed_train_df$target)
transformed_train_df$rad <- as.factor(transformed_train_df$rad)


## ----count_zn------------------------------------------------------------------------------------------------------------------------------
count(train_df,zn)

transformed_train_df <- subset(transformed_train_df, select = -c(zn))


## ----split_data_1--------------------------------------------------------------------------------------------------------------------------
#split data for training and testing
sample <- sample.split(transformed_train_df$target, SplitRatio = 0.8)
transformed_training  <- subset(transformed_train_df, sample == TRUE)
transformed_testing   <- subset(transformed_train_df, sample == FALSE)


## ----boxcox, fig.keep="none"---------------------------------------------------------------------------------------------------------------
# Create new dataframe to work with
box_train_df <- transformed_train_df


age_boxcox <- boxcox(lm(box_train_df$age ~ 1))
age_lambda <- age_boxcox$x[which.max(age_boxcox$y)]
box_train_df$age <- BoxCox(box_train_df$age, age_lambda)

dis_boxcox <- boxcox(lm(box_train_df$dis ~ 1))
dis_lambda <- dis_boxcox$x[which.max(dis_boxcox$y)]
box_train_df$dis <- BoxCox(box_train_df$dis, dis_lambda)

lstat_boxcox <- boxcox(lm(box_train_df$lstat ~ 1))
lstat_lambda <- lstat_boxcox$x[which.max(lstat_boxcox$y)]
box_train_df$lstat <- BoxCox(box_train_df$lstat, lstat_lambda)

nox_boxcox <- boxcox(lm(box_train_df$nox ~ 1))
nox_lambda <- lstat_boxcox$x[which.max(nox_boxcox$y)]
box_train_df$nox <- BoxCox(box_train_df$nox, nox_lambda)

ptratio_boxcox <- boxcox(lm(box_train_df$ptratio ~ 1))
ptratio_lambda <- lstat_boxcox$x[which.max(ptratio_boxcox$y)]
box_train_df$ptratio <- BoxCox(box_train_df$ptratio, ptratio_lambda)

tax_boxcox <- boxcox(lm(box_train_df$tax ~ 1))
tax_lambda <- tax_boxcox$x[which.max(tax_boxcox$y)]
box_train_df$tax <- BoxCox(box_train_df$tax, tax_lambda)

indus_boxcox <- boxcox(lm(box_train_df$indus ~ 1))
indus_lambda <- indus_boxcox$x[which.max(indus_boxcox$y)]
box_train_df$indus <- BoxCox(box_train_df$indus, indus_lambda)

medv_boxcox <- boxcox(lm(box_train_df$medv ~ 1))
medv_lambda <- medv_boxcox$x[which.max(medv_boxcox$y)]
box_train_df$medv <- BoxCox(box_train_df$medv, medv_lambda)



## ----box_hists, warning=FALSE, message=FALSE-----------------------------------------------------------------------------------------------
m_df_box <- box_train_df %>% dplyr::select(c(age, dis, lstat, nox, ptratio, tax, indus, medv)) %>% melt() 

a <- m_df_box %>% ggplot(aes(x= value)) + 
geom_density(color='#023020', fill='gray') + facet_wrap(~variable, scales = 'free',  ncol = 1) + theme_bw()

m_df <- transformed_train_df %>% dplyr::select(c(age, dis, lstat, nox, ptratio, tax, indus, medv)) %>% melt() 

b <- m_df %>% ggplot(aes(x= value)) + 
geom_density(color='#023020', fill='gray') + facet_wrap(~variable, scales = 'free',  ncol = 1) + theme_bw()

ggarrange(a, b + rremove("x.text"), 
          labels = c("Transformed", "Original"))


## ----split_data_2--------------------------------------------------------------------------------------------------------------------------
# Split data for training and testing
sample <- sample.split(box_train_df$target, SplitRatio = 0.8)
box_training  <- subset(box_train_df, sample == TRUE)
box_testing   <- subset(box_train_df, sample == FALSE)


## ------------------------------------------------------------------------------------------------------------------------------------------
# set seed for consistancy 
set.seed(1233)

# 80/20 split of the training data set
sample <- sample.split(train_df$target, SplitRatio = 0.8)
train_data  <- subset(train_df, sample == TRUE)
test_data   <- subset(train_df, sample == FALSE)


# build X matrix and Y vector
X <- model.matrix(target ~ ., data=train_data)[,-1]
Y <- train_data[,"target"] 



## ----general, warning=FALSE, message=FALSE-------------------------------------------------------------------------------------------------
model_1 <- glm(transformed_training, formula = target ~., family = binomial(link = "logit"))
summ(model_1)


## ----model1_aic, warning=FALSE, message=FALSE----------------------------------------------------------------------------------------------
model_1_aic <- model_1 %>% stepAIC(trace = FALSE)
summary(model_1_aic)


## ----chi_model_1---------------------------------------------------------------------------------------------------------------------------
1-pchisq(model_1_aic$null.deviance-model_1_aic$deviance, model_1_aic$df.null- model_1_aic$df.residual)


## ----check_lm1-----------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(model_1_aic)



## ----model_1_linearity---------------------------------------------------------------------------------------------------------------------
probabilities <- predict(model_1_aic, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

#Only numeric predictors
data <- transformed_training %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(data)

# Bind the logit and tidying the data for plot
data <- data %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

#Scatter plot
ggplot(data, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


## ----model_1_residuals---------------------------------------------------------------------------------------------------------------------
model_1_aic.data <- augment(model_1_aic) %>% 
  mutate(index = 1:n())

ggplot(model_1_aic.data, aes(index, .std.resid)) + 
  geom_point(aes(color = target), alpha = .5) +
  theme_bw()


## ----warning=FALSE, message=FALSE----------------------------------------------------------------------------------------------------------
car::mmps(model_1_aic, span = 3/4, layout = c(2, 2))


## ----model_1_vif---------------------------------------------------------------------------------------------------------------------------
car::vif(model_1_aic)


## ----general_box, warning=FALSE, message=FALSE---------------------------------------------------------------------------------------------
model_2 <- glm(box_training, formula = target ~ ., family = binomial(link = "logit"))
summary(model_2)


## ----model2_aic, warning=FALSE, message=FALSE----------------------------------------------------------------------------------------------
model_2_aic <- model_2 %>% stepAIC(trace = FALSE)
summary(model_2_aic)


## ----chi_model_2---------------------------------------------------------------------------------------------------------------------------
1-pchisq(model_2_aic$null.deviance-model_2_aic$deviance, model_2_aic$df.null- model_2_aic$df.residual)


## ----check_lm2-----------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(model_2_aic)


## ----model_2_linearity---------------------------------------------------------------------------------------------------------------------
probabilities <- predict(model_2_aic, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

#Only numeric predictors
data <- box_training %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(data)

# Bind the logit and tidying the data for plot
data <- data %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

#Scatter plot
ggplot(data, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


## ----model_2_residuals---------------------------------------------------------------------------------------------------------------------
model_2_aic.data <- augment(model_2_aic) %>% 
  mutate(index = 1:n())

ggplot(model_2_aic.data, aes(index, .std.resid)) + 
  geom_point(aes(color = target), alpha = .5) +
  theme_bw()


## ----warning=FALSE, message=FALSE----------------------------------------------------------------------------------------------------------
car::mmps(model_2_aic, span = 3/4, layout = c(2, 2))


## ----model_2_vif---------------------------------------------------------------------------------------------------------------------------
car::vif(model_2_aic)


## ------------------------------------------------------------------------------------------------------------------------------------------

lasso.model<- cv.glmnet(x=X,y=Y,
                       family = "binomial", 
                       link = "logit",
                       standardize = TRUE,                       #standardize  
                       nfold = 5,
                       alpha=1)                                  #alpha=1 is lasso

l.min <- lasso.model$lambda.min
l.1se <- lasso.model$lambda.1se
coef(lasso.model, s = "lambda.min" )
coef(lasso.model, s = "lambda.1se" )
lasso.model



## ------------------------------------------------------------------------------------------------------------------------------------------

par(mfrow=c(2,2))

plot(lasso.model)
plot(lasso.model$glmnet.fit, xvar="lambda", label=TRUE)
plot(lasso.model$glmnet.fit, xvar='dev', label=TRUE)

rocs <- roc.glmnet(lasso.model, newx = X, newy = Y )
plot(rocs,type="l")  



## ------------------------------------------------------------------------------------------------------------------------------------------
assess.glmnet(lasso.model,           
              newx = X,              
              newy = Y )    

print(glmnet_cv_aicc(lasso.model, 'lambda.min'))
print(glmnet_cv_aicc(lasso.model, 'lambda.1se'))



## ------------------------------------------------------------------------------------------------------------------------------------------
coef(lasso.model, s = "lambda.1se" )
vip(lasso.model, num_features=20 ,geom = "col", include_type=TRUE, lambda = "lambda.1se")


## ------------------------------------------------------------------------------------------------------------------------------------------
# Create matrix new data
X_test <- model.matrix(target ~ ., data=test_data)[,-1]
Y_test <- test_data[,"target"] 


# predict using coefficients at lambda.min
lassoPred <- predict(lasso.model, newx = X_test, type = "response", s = 'lambda.1se')

pred_df <- test_data
pred_df$target_prob <- lassoPred[,1]
pred_df$target_pred <- ifelse(lassoPred > 0.5, 1, 0)[,1]



## ------------------------------------------------------------------------------------------------------------------------------------------

confusion.glmnet(lasso.model, newx = X_test, newy = Y_test, s = 'lambda.1se')



## ------------------------------------------------------------------------------------------------------------------------------------------

pred_df <- pred_df %>%
  mutate(logit = log(target_prob/(1-target_prob))) 

m_df <- pred_df %>% pivot_longer(!c(target,target_prob,target_pred,logit), 
                                 names_to='variable' , values_to = 'value')

#Scatter plot
ggplot(m_df, aes(logit, value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~variable, scales = "free_y")



## ------------------------------------------------------------------------------------------------------------------------------------------
pred_df$index <- as.numeric(rownames(pred_df))
pred_df$resid <- pred_df$target_prob - pred_df$target


ggplot(pred_df, aes(index, resid)) + 
  geom_point(aes(color = target), alpha = .5) +
  theme_bw()



## ----model_1_confusionMatrix---------------------------------------------------------------------------------------------------------------

transformed_testing$model_1 <- ifelse(predict.glm(model_1_aic, transformed_testing, "response") >= 0.5, 1, 0)
conf_matrix_1 <- confusionMatrix(factor(transformed_testing$model_1), factor(transformed_testing$target), "1")


results <- tibble(Model = "Model #1", Accuracy=conf_matrix_1$byClass[11], 
                  "Classification error rate" = 1 - conf_matrix_1$byClass[11],
                  F1 = conf_matrix_1$byClass[7],
                  Deviance= model_1_aic$deviance, 
                  R2 = 1 - model_1_aic$deviance / model_1_aic$null.deviance,
                  Sensitivity = conf_matrix_1$byClass["Sensitivity"],
                  Specificity = conf_matrix_1$byClass["Specificity"],
                  Precision = precision(factor(transformed_testing$model_1), factor(transformed_testing$target), "1"),
                  AIC= model_1_aic$aic)

conf_matrix_1


## ----model_2_confusionMatrix---------------------------------------------------------------------------------------------------------------

box_testing$model_2 <- ifelse(predict.glm(model_2_aic, box_testing, "response") >= 0.5, 1, 0)
conf_matrix_2 <- confusionMatrix(factor(box_testing$model_2), factor(box_testing$target), "1")


results2 <- rbind(results,tibble(Model = "Model #2", Accuracy=conf_matrix_2$byClass[11], 
                  "Classification error rate" = 1 - conf_matrix_2$byClass[11],
                  F1 = conf_matrix_2$byClass[7],
                  Deviance=model_2_aic$deviance, 
                  R2 = 1 - model_2_aic$deviance / model_2_aic$null.deviance,
                  Sensitivity = conf_matrix_2$byClass["Sensitivity"],
                  Specificity = conf_matrix_2$byClass["Specificity"],
                  Precision = precision(factor(box_testing$model_2), factor(box_testing$target), "1"),
                  AIC= model_2_aic$aic))

conf_matrix_2


## ----show_confusion_matrix_3---------------------------------------------------------------------------------------------------------------
conf_matrix_3 = confusionMatrix(factor(pred_df$target_pred),factor(pred_df$target), "1")
conf_matrix_3$byClass


lasso.r1 <- assess.glmnet(lasso.model,           
                                newx = X_test,              
                                newy = Y_test )   
lasso.r2 <- glmnet_cv_aicc(lasso.model, 'lambda.1se')


results2 <- rbind(results2,tibble(Model = "Model #3", Accuracy=lasso.r1$auc[1], 
                  "Classification error rate" = 1 - lasso.r1$auc[1],
                  F1 = conf_matrix_3$byClass[7],
                  Deviance=lasso.r1$deviance[[1]], 
                  R2 = NA,
                  Sensitivity = conf_matrix_3$byClass["Sensitivity"],
                  Specificity = conf_matrix_3$byClass["Specificity"],
                  Precision = precision(factor(pred_df$target_pred),factor(pred_df$target), "1"),
                  AIC= lasso.r2$AICc))



## ------------------------------------------------------------------------------------------------------------------------------------------
kable(results2, digits=4) %>% 
  kable_styling(bootstrap_options = "basic", position = "center")


## ----roc_model_2, warning=FALSE, message=FALSE---------------------------------------------------------------------------------------------
par(mfrow=c(2,2))

# Model 1 curve
plot(roc(transformed_testing$target, transformed_testing$model_1, plot = FALSE,  print.auc = TRUE), quiet=TRUE, main="Model 1 - ROC Curve")


# Model 2 curve
plot(roc(box_testing$target, box_testing$model_2, plot = FALSE,  print.auc = TRUE, quiet=TRUE), main="Model 2 - ROC Curve")


# Model 3 curve
plot(roc(pred_df$target, pred_df$target_pred, plot = FALSE,  print.auc = TRUE, quiet=TRUE), main="Model 3 - ROC Curve")


## ------------------------------------------------------------------------------------------------------------------------------------------

# build X matrix and Y vector
eval_df$target <- 0
X_eval <- model.matrix(target ~ ., data=eval_df)[,-1]

# predict using coefficients at lambda.min
lassoEval <- predict(lasso.model, newx = X_eval, type = "response", s = 'lambda.1se')

eval_pred_df <- eval_df
eval_pred_df$target_prob <- lassoEval[,1]
eval_pred_df$target_pred <- ifelse(lassoEval > 0.5, 1, 0)[,1]

write.csv(eval_pred_df, 'eval_predictions_lasso.csv', row.names=F)


## ----results_table-------------------------------------------------------------------------------------------------------------------------
DT::datatable(
      eval_pred_df,
      extensions = c('Scroller'),
      options = list(scrollY = 350,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     dom = 'lBfrtip',
                     fixedColumns = TRUE, 
                     searching = FALSE), 
      rownames = FALSE) 

