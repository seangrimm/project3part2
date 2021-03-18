#' random forest with k-fold cross validation
#'
#' Builds a classification model for the \code{my_penguins} data set to predict
#' \code{body_mass_g} based on \code{bill_length_mm}, \code{bill_depth_mm}, and
#' \code{flipper_length_mm} and applies k-fold cross-validation with
#' k = \code{k}.
#'
#' @param k Integer indicating how many folds \code{train} will be divided
#'   into for k-fold cross-validation. Must have \code{k} >= 2.
#'
#' @keywords prediction
#'
#' @return numeric representing the cross-validation error from the k-fold
#'   cross-validation.
#'
#' @examples
#' my_rf_cv(2)
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  input_data <- na.omit(my_penguins)[,3:6]
  
  # Randomly split into k groups
  fold <- sample(rep(1:k, length = nrow(input_data)))
  input_data$fold <- fold
  
  errors <- rep(0, k)
  for (i in 1:k) {
    # separate into training and test
    data_train <- input_data[which(input_data$fold != i), ]
    data_train <- subset.data.frame(data_train, select = -ncol(data_train))
    
    data_test <- input_data[which(input_data$fold == i), ]
    data_test <- subset.data.frame(data_test, select = -ncol(data_test))
    
    # make model and prediction
    model <- randomForest::randomForest(body_mass_g ~ bill_length_mm +
                                          bill_depth_mm + flipper_length_mm,
                                        data = data_train, ntree = 100)
    
    prediction <- predict(model, data_test[, 1:3])
    
    # determine error
    errors[i] <- mean((data_test[[4]] - prediction)^2)
  }
  
  return(mean(errors))
}
