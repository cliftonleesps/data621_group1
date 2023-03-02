library(tidyverse)
library(dplyr)


classify_predictions <- function(df) {

    true_negative <- df %>% select(class, scored.class) %>% filter(scored.class == 0 & class == 0)
    true_positive <- df %>% select(class, scored.class) %>% filter(scored.class == 1 & class == 1)

    false_negative <- df %>% select(class, scored.class) %>% filter(scored.class == 0 & class == 1)
    false_positive <- df %>% select(class, scored.class) %>% filter(scored.class == 1 & class == 0)

    matrix <- list("tn" = nrow(true_negative),
                   "tp" = nrow(true_positive),
                   "fn" = nrow(false_negative),
                   "fp" = nrow(false_positive))
    return (matrix)
}


## 3. Write a function that takes the data set as a dataframe, with actual and predicted
## classifications identified, and returns the accuracy of the predictions.
test_accuracy <- function(df) {
    m <- classify_predictions(df)
    accuracy <- (m$tp + m$tn) / (m$tp + m$fp + m$tn + m$fn)
    return (accuracy)
}

## 4. Write a function that takes the data set as a dataframe, with actual and
## predicted classifications identified, and returns the classification error rate of the predictions.
test_classification_error_rate <- function(df) {
    m <- classify_predictions(df)
    cer <- (m$fp + m$fn) / (m$tp + m$fp + m$tn + m$fn)
    return (cer)
}

## 5. Write a function that takes the data set as a dataframe, with actual and
## predicted classifications identified, and returns the precision of the predictions.
test_precision <- function(df) {
    m <- classify_predictions(df)
    p <- m$tp / (m$tp + m$fp)
    return (p)
}

## 6. Write a function that takes the data set as a dataframe, with actual and
## predicted classifications identified, and returns the sensitivity of the predictions.
## Sensitivity is also known as recall.
test_sensitivity <- function(df) {
    m <- classify_predictions(df)
    s <- (m$tp) / (m$tp + m$fn)
    return (s)
}

## 7. Write a function that takes the data set as a dataframe, with actual and predicted
## classifications identified, and returns the specificity of the predictions.
test_specificity <- function(df) {
    m <- classify_predictions(df)
    s <- (m$tn) / (m$tn + m$fp)
    return (s)
}

## 8. Write a function that takes the data set as a dataframe, with actual and
## predicted classifications identified, and returns the F1 score of the predictions.
test_f1_score <- function(df) {
    precision <- precision(df)
    sensitivity <- sensitivity(df)

    score <- (2 * precision * sensitivity) / (precision + sensitivity)
    return(score)
}
