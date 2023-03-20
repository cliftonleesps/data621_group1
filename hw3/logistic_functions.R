library(tidyverse)
library(dplyr)



#' Title
#'
#' @param fit
#' @param lambda
#'
#' @return
#' @export
#'
#' @examples
glmnet_cv_aicc <- function(fit, lambda = 'lambda.1se'){
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


