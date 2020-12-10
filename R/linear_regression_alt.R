#' Implements simple linear regression by gradient descent
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
slr_gd <- function(dat, response, explanatory){
  
  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `simple_linear_regression`
  X <- dat %>% 
    pull({{explanatory}}) %>% 
    as.matrix()
  Y <- dat %>% pull({{response}})
  
  ex_name <- dat %>%
    select({{explanatory}}) %>%
    names()
  
  m = 0 # initializing slope
  c = 0 # initializing intercept
  
  L = 0.00001 # learning rate
  
  n = length(X)
  
  # performing Gradient Descent
  for (i in 1:1000000) { # number of iterations for gradient descent
    Y_Pred = m*X + c # creating current predicted value of Y

    D_m = (-2/n) * sum(X * (Y - Y_Pred)) # derivative with respect to m
    D_c = (-2/n) * sum(Y - Y_Pred) # derivative with respect to c
    
    m = m - L*D_m # updating value of slope
    c = c - L*D_c # updating value of intercept
  }
  
  results <- tibble::tibble(
    Intercept = c,
    Slope = m
  )
  
  names(results)[2] <- ex_name
  
  return(results)
  
  
}


#' Implements linear regression with many predictors by gradient descent
#'
#' This function computes coefficients for multiple regression by gradient descent
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
mlr_gd <- function(dat, response) {
  
  x <- dat %>% 
    select(-{{response}}) %>%
    as.matrix()
  
  X <- cbind(1, x)
  
  Y <- dat %>% 
    pull({{response}}) %>% 
    as.matrix()
  
  m <- rep(0, ncol(X)) %>% 
    as.matrix()
  
  L <- 0.00001 # learning rate
  n <- nrow(X)
  
  for (i in 1:4000000) { # number of iterations for gradient descent
    D_m = (-2/n) * t(X) %*%(Y - (X%*%m)) #derivative with respect to m
    m = m - L*D_m # updating value of slope and intercept
    
  }
  results <- m %>% t() %>% as.data.frame()
  names(results)[1] <- "Intercept"
  return(results)
  
  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `multiple_linear_regression`
}

#' Implements linear regression with many predictors by matrix decomposition
#'
#' This function computes coefficients for multiple regression by QR matrix decomposition
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
mlr_qr <- function(dat, response) {
  
  x <- dat %>% 
    select(-{{response}})  
  
  y <- dat %>% 
    pull({{response}})
  X <- cbind(Intercept = 1, x)
  
  QR <- qr(X)
  results <- solve.qr(QR, y) %>% t() %>% as.data.frame() 
  
  ### Compute coefficients by QR decomposition
  ### Return a data frame of the same form as in the `multiple_linear_regression`
  
  return(results)
  
}
