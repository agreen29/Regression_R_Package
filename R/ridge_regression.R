#' Implements ridge regression with many predictors
#'
#' This function computes coefficients for ridge regression
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @importFrom purrr pmap
#' @importFrom tidyr pivot_longer pivot_wider
#' @export
ridge_regression <- function(dat, response, lambda) {
  
  y <- dat %>% 
    pull({{response}})
  x_matrix <- dat %>% 
    select(-{{response}}) %>% 
    scale() %>% 
    as.matrix()
  Intercept <- y %>% 
    mean()
  
  lambda_diag_matrix = pmap(list(lambda), .f = function(lambda) {lambda * diag(dim(x_matrix)[2])
    
  })
  betas <- pmap(list(lambda_diag_matrix), .f = function(lambda_diag_matrix) {
    solve(t(x_matrix) %*% x_matrix + lambda_diag_matrix) %*% t(x_matrix) %*% y
  }) %>% 
    as.data.frame(col.names = lambda) 
  
  names(betas) <- sub("^X", "", names(betas))
  
  betas <- tibble::rownames_to_column(betas, var="Explanatory") %>% 
    pivot_longer(-Explanatory, names_to = "lambda", values_to = "betas") %>% 
    pivot_wider(names_from = Explanatory, values_from = betas) %>%
    mutate(
      lambda = as.numeric(lambda)
    )
  
  betas <- betas[, c(2,3,1)]
  results <- cbind(Intercept, betas)
  ### This should be a data frame, with columns named
  ### "Intercept" and the same variable names as dat, and also a column
  ### called "lambda".
  
  return(results)
}

#' Determines the best penalty term from a set of options
#'
#' This function uses a randomly chosen test and training set
#'
#' No interaction terms are included.
#'
#'
#' @param train_dat A data frame to construct the model from
#' @param test_dat A data frame to test the model on
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of penalty terms and resulting errors
#'
#' @import dplyr
#'
#' @export
find_best_lambda <- function(train_dat, test_dat, response, lambdas) {
  
  
  ### lambda_errors should be a data frame with two columns: "lambda" and "error"
  ### For each lambda, you should record the resulting Sum of Squared error
  ### (i.e., the predicted value minus the real value squared) from prediction
  ### on the test dataset.
  result_coefs <- ridge_regression(train_dat, {{response}}, lambdas) %>%
    select(-lambda)
  
  predicted <- predict_from_coefs(test_dat, {{response}}, result_coefs)
  
  error_data <- apply(predicted, 2, function(x){sum((predicted[1] - x)**2)})
  error_data <- error_data[-1]
  
  lambda_errors <- cbind(lambdas, error_data) %>% as.data.frame()
  names(lambda_errors) <- c("lambda", "error") 
  
  return(lambda_errors)
}
