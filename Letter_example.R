# Application of multi-class logistic to letters data

# Load the letter data
#########################
# Training data
letter_train <- read.table("Data/letter-train.txt", header = F, colClasses = "numeric")
Y <- letter_train[ , 1]
X <- as.matrix(letter_train[ , -1])

# Testing data
letter_test <- read.table("Data/letter-test.txt", header = F, colClasses = "numeric")
Yt <- letter_test[ , 1]
Xt <- as.matrix(letter_test[ , -1])

# [ToDo] Make sure to add column for an intercept to X and Xt
X <- cbind(rep(1, nrow(X)), X)
Xt <- cbind(rep(1, nrow(Xt)), Xt)

# Source the LR function
source("FunctionsLR.R")

# [ToDo] Try the algorithm LRMultiClass with lambda = 1 and 50 iterations. Call the resulting object out, i.e. out <- LRMultiClass(...)
out <- LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL)

# The code below will draw pictures of objective function, as well as train/test error over the iterations
plot(out$objective, type = 'o')
plot(out$error_train, type = 'o')
plot(out$error_test, type = 'o')

# Feel free to modify the code above for different lambda/eta/numIter values to see how it affects the convergence as well as train/test errors
out_modified <- LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.5, lambda = 1, beta_init = NULL)
out_modified <- LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 7, beta_init = NULL)
out_modified <- LRMultiClass(X, Y, Xt, Yt, numIter = 100, eta = 0.1, lambda = 1, beta_init = NULL)
out_modified <- LRMultiClass(X, Y, Xt, Yt, numIter = 100, eta = 0.5, lambda = 7, beta_init = NULL)
out_modified <- LRMultiClass(X, Y, Xt, Yt, numIter = 200, eta = 0.99, lambda = 370, beta_init = NULL)
out_modified <- LRMultiClass(X, Y, Xt, Yt, numIter = 200, eta = 0.05, lambda = 37, beta_init = NULL)

# [ToDo] Use microbenchmark to time your code with lambda=1 and 50 iterations. To save time, only apply microbenchmark 5 times.
library(microbenchmark)
microbenchmark(
  LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL),
  times = 5
)

# [ToDo] Report the median time of your code from microbenchmark above in the comments below
print("Median Time from microbenchmark on my M1 Mac = 1.128087 seconds.")
# Median time:  (in sec)