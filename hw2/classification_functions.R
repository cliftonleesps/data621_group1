library(tidyverse)
library(dplyr)


classify_predictions <- function(df) {

    true_negative <- data %>% select(class, scored.class) %>% filter(scored.class == 0 & scored.class == class)
    true_positive <- data %>% select(class, scored.class) %>% filter(scored.class == 1 & scored.class == class)

    false_negative <- data %>% select(class, scored.class) %>% filter(scored.class == 0 & scored.class != class)
    false_positive <- data %>% select(class, scored.class) %>% filter(scored.class == 1 & scored.class != class)

    matrix <- list("tn" = nrow(true_negative),
                   "tp" = nrow(true_positive),
                   "fn" = nrow(false_negative),
                   "fp" = nrow(false_positive))
    return (matrix)
}

accuracy <- function(df) {
    m <- classify_predictions(df)
    accuracy <- (m$tp + m$tn) / (m$tp + m$fp + m$tn + m$fn)
    return (accuracy)
}


classification_error_rate <- function(df) {
    m <- classify_predictions(df)
    cer <- (m$fp + m$fn) / (m$tp + m$fp + m$tn + m$fn)
    return (cer)
}


precision <- function(df) {
    m <- classify_predictions(df)
    p <- m$tp / (m$tp + m$fp)
    return (p)
}

sensitivity <- function(df) {
    m <- classify_predictions(df)
    s <- (m$tp) / (m$tp + m$fn)
    return (s)
}

specificity <- function(df) {
    m <- classify_predictions(df)
    s <- (m$tn) / (m$tn + m$fp)
    return (s)
}

f1_score <- function(df) {
    precision <- precision(df)
    sensitivity <- sensitivity(df)

    score <- (2 * precision * sensitivity) / (precision + sensitivity)
    return(score)
}
