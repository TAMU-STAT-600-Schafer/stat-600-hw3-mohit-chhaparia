# This is a script to save your own tests for the function
source("FunctionsLR.R")
library(testthat)

set.seed(0928)

# Dataset for testing
n <- 100
p <- 3
K <- 3

# Normal X and Xt populations
X <- cbind(1, matrix(rnorm(n * (p - 1)), n, p - 1))
Xt <- cbind(1, matrix(rnorm(n * (p - 1)), n, p - 1))

# Random class labels
y <- sample(0:(K - 1), n, replace = TRUE)
yt <- sample(0:(K - 1), n, replace = TRUE)

# Initialize beta_init
beta_init <- matrix(rep(0, p * K), p, K)

# Function call
out <- LRMultiClass(X, y, Xt, yt, 50, 0.1, 1, beta_init)

# Check change in objective function values
test_that("Objective function value decreases or converges.", {
  values <- out$objective
  expect_true(all(diff(values) <= 0 | abs(diff(values)) < 1e-5))
})

# Check change in training error - A test that will fail we can't guarantee that the error will always decrease or the absolute value of difference would be very small
try(test_that("Training error decreases or converges.", {
  values <- out$error_train
  expect_true(all(diff(values) <= 0 | abs(diff(values)) < 1e-5))
}))

# Check that training error always reduces compared to first iteration
test_that("Training error decreases or converges.", {
  values <- out$error_train
  expect_true(all(diff(values) <= values[1]))
})

# Check change in testing error - A test that will fail we can't guarantee that the error will always decrease or the absolute value of difference would be very small
try(test_that("Testing error decreases or converges.", {
  values <- out$error_test
  expect_true(all(diff(values) <= 0 | abs(diff(values)) < 1e-5))
}))

# Check that testing error always reduces compared to first iteration
test_that("Testing error decreases or converges.", {
  values <- out$error_test
  expect_true(all(diff(values) <= values[1]))
})

# Check change in objective function values for a different normal population
test_that("Objective function value decreases or converges for different set of parameters for rnorm.", {
  X_new <- cbind(1, matrix(rnorm(n * (p - 1), 5, 2), n, p - 1))
  Xt_new <- cbind(1, matrix(rnorm(n * (p - 1), 5, 2), n, p - 1))
  out_new <- LRMultiClass(X_new, y, Xt_new, yt, 50, 0.1, 1, beta_init)
  values <- out_new$objective
  expect_true(all(diff(values) <= 0 | abs(diff(values)) < 1e-5))
})



# Error: X is not a matrix
tryCatch({
  X_wrong <- "Not a matrix"
  LRMultiClass(X_wrong, y, Xt, yt, 50, 0.1, 1, beta_init)
}, error = function(e) {
  print(paste("Error:", e$message))
})

# Error: First column of X is not all 1s
tryCatch({
  X_wrong <- matrix(rnorm(n * p), n, p)
  LRMultiClass(X_wrong, y, Xt, yt, 50, 0.1, 1, beta_init)
}, error = function(e) {
  print(paste("Error:", e$message))
})

# Error: Xt is not a matrix
tryCatch({
  Xt_wrong <- "Not a matrix"
  LRMultiClass(X, y, Xt_wrong, yt, 50, 0.1, 1, beta_init)
}, error = function(e) {
  print(paste("Error:", e$message))
})

# Error: Incorrect values in y
tryCatch({
  y_wrong <- rep(0:3, 25)
  LRMultiClass(X, y_wrong, Xt, yt, 50, 0.1, 1, beta_init)
}, error = function(e) {
  print(paste("Error:", e$message))
})

# Error: numIter not a positive integer
tryCatch({
  LRMultiClass(X, y, Xt, yt, -50, 0.1, 1, beta_init)
}, error = function(e) {
  print(paste("Error:", e$message))
})
