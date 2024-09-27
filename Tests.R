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
test_that("Objective function value decreases or converges.",{
  values <- out$objective
  expect_true(all(diff(values) <= 0 | abs(diff(values)) < 1e-5))
})

