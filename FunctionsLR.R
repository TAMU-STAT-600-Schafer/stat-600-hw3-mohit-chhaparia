# Function that implements multi-class logistic regression.
#############################################################
# Description of supplied parameters:
# X - n x p training data, 1st column should be 1s to account for intercept
# y - a vector of size n of class labels, from 0 to K-1
# Xt - ntest x p testing data, 1st column should be 1s to account for intercept
# yt - a vector of size ntest of test class labels, from 0 to K-1
# numIter - number of FIXED iterations of the algorithm, default value is 50
# eta - learning rate, default value is 0.1
# lambda - ridge parameter, default value is 1
# beta_init - (optional) initial starting values of beta for the algorithm, should be p x K matrix 

## Return output
##########################################################################
# beta - p x K matrix of estimated beta values after numIter iterations
# error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
# error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
# objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
LRMultiClass <- function(X, y, Xt, yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  ## Check the supplied parameters as described. You can assume that X, Xt are matrices; y, yt are vectors; and numIter, eta, lambda are scalars. You can assume that beta_init is either NULL (default) or a matrix.
  ###################################
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if(any(X[ , 1] != 1)) stop("First column of X must all be 1s to account for intercept.")
  if(any(Xt[ , 1] != 1)) stop("First column of Xt must all be 1s to account for intercept.")
  
  # Check for compatibility of dimensions between X and Y
  if(length(y) != nrow(X)) stop("Length of y and number of rows of X should be equal.")
  
  # Check for compatibility of dimensions between Xt and Yt
  if(length(yt) != nrow(Xt)) stop("Length of yt and number of rows of Xt should be equal.")
  
  # Check for compatibility of dimensions between X and Xt
  if(ncol(X) != ncol(Xt)) stop("Number of columns of X and Xt should be equal.")
  
  # Check eta is positive
  if(eta <= 0) stop("Learning rate (eta) should strictly be positive.")
  
  # Check lambda is non-negative
  if(lambda < 0) stop("Ridge parameter (lambda) should strictly be non-negative.")
  
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
  p <- ncol(X)
  K <- length(unique(y))
  if(is.null(beta_init)){
    beta <- matrix(rep(0, p * K), p, K)
  } else{
    if(!all(dim(beta_init) == c(p, K))) stop("Number of rows of beta_init should be equal to the number of columns of X and number of columns of beta_init should be equal to the number of classes.")
    beta <- beta_init
  }
  
  ## Calculate corresponding pk, objective value f(beta_init), training error and testing error given the starting point beta_init
  ##########################################################################
  objective <- vector(mode = 'numeric', length = (numIter + 1))
  error_train <- vector(mode = 'numeric', length = (numIter + 1))
  error_test <- vector(mode = 'numeric', length = (numIter + 1))
  objective[1] <- cal_obj(X, y, beta, lambda)
  error_train[1] <- cal_err(X, y, beta)
  error_test[1] <- cal_err(Xt, yt, beta)
  
  ## Newton's method cycle - implement the update EXACTLY numIter iterations
  ##########################################################################
  for(i in 1:numIter){
    pk <- cal_pk(X, beta)
    
    # Indicator function and Gradient Calculations
    gradient <- matrix(rep(0, p * K), p, K)
    for(j in 1:K){
      indicator <- (y == (j - 1))
      gradient[ , j] <- - t(X) %*% (indicator - pk[ , j]) + lambda * beta[ , j]
    }
    
    # Hessian and beta Calculations
    for(j in 1:K){
      wt <- diag(as.vector(pk[ , j] * (1 - pk[ , j])))
      hessian <- t(X) %*% wt %*% X + lambda * diag(p)
      beta[ , j] <- beta[ , j] - eta * solve(hessian) %*% gradient[ , j]
    }
  
    # Within one iteration: perform the update, calculate updated objective function and training/testing errors in %
    error_train[i + 1] <- cal_err(X, y, beta)
    error_test[i + 1] <- cal_err(Xt, yt, beta)
    objective[i + 1] <- cal_obj(X, y, beta, lambda)
  
  }
  ## Return output
  ##########################################################################
  # beta - p x K matrix of estimated beta values after numIter iterations
  # error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
  # error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
  # objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
  return(list(beta = beta, error_train = error_train, error_test = error_test, objective =  objective))
}

cal_pk <- function(X, beta){
  pk <- exp(X %*% beta)
  return(pk / rowSums(pk))
}

cal_obj <- function(X, Y, beta, lambda){
  pk <- cal_pk(X, beta)
  return( - (sum(log(pk[cbind(1:nrow(X), Y + 1)]))) + ((lambda / 2) * sum(beta ^ 2)))
}

cal_err <- function(X, Y, beta){
  pk <- cal_pk(X, beta)
  pred <- max.col(pk) - 1
  return(mean(pred != Y) * 100)
}