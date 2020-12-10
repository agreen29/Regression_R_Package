#' Computes predicted values given coefficients
#'
#' This function takes a data frame of coefficients in the form outputted by
#' functions like \code{multiple_linear_regression} or \code{ridge_regression}.
#'
#' It calculates the predicted values of a response variable from these coefficients.
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param coefs A data frame of coefficient estimates
#'
#' @return A data frame of true and predicted values
#'
#' @import dplyr
#'
#' @export
predict_from_coefs <- function(dat, response, coefs){
  
    data_matrix <- dat %>% select(- {{response}}) %>% as.matrix()
    intercept <- coefs$Intercept
    coefs_matrix <- coefs %>% select(- Intercept) %>% as.matrix() %>% t()
    Predicted <- intercept + (data_matrix %*% coefs_matrix)
    
    observed_response <- dat %>% select({{response}}) 
    
    return(as.data.frame(cbind(observed_response, Predicted)))
    
  }



