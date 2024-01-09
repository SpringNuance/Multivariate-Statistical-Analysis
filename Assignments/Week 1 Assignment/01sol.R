# Demo Problem 1
# a)
# setwd("...")
help(c)
help(matrix)


# b)
# We create vectors x and b in two different ways
a <- matrix(c(2, 1, 5, -2, 7, 0, 5, -8, -1), nrow = 3, byrow = TRUE)
x1 <- c(8, -4, 2)
b1 <- c(3, 10, -19)

x2 <- matrix(x1, nrow = 1, byrow = TRUE)
b2 <- matrix(b1, nrow = 1, byrow = TRUE)

y1 <- x1 %*% solve(a) + b1
y2 <- x2 %*% solve(a) + b2

all(y1 == y2)
y1

x3 <- matrix(x1, ncol = 1, byrow = FALSE)
b3 <- matrix(b1, ncol = 1, byrow = FALSE)

x3 %*% solve(a) + b3 # Note that this line of code gives an error


# c)
# install.packages("mvtnorm") # Run this only once
library(mvtnorm)

n <- 100
mu <- c(3, 1)
sigma <- matrix(c(4, 1, 1, 2), byrow = TRUE, ncol = 2)

set.seed(123)
x <- rmvnorm(n, mu, sigma)
head(x)
plot(x)


# d)
# There are a couple of ways to compute the sample mean vector
x_mean1 <- apply(x, 2, mean) # Apply function "mean" to every column
x_mean2 <- colMeans(x)
all(x_mean1 == x_mean2)
x_mean1

x_cov <- cov(x)
x_cov

eig <- eigen(x_cov)
eigval <- eig$values
eigvec <- eig$vectors
eigval
eigvec

# Remember that trace of a square matrix is defined as the sum of the
# diagonal elements.
sum(diag(x_cov)) - sum(eigval)
det(x_cov) - prod(eigval)


# e)
# Create vector b and matrix A
b <- c(3, 1)
a <- matrix(c(1, 2, 3, 1), byrow = TRUE, ncol = 2)

# Two different ways to compute affine transformations
# First way
y1 <- sweep(x %*% t(a), 2, b, "+")

# Second way
ones <- rep(1, n)
y2 <- x %*% t(a) + ones %*% t(b)

all(y1 == y2)

# Check that sample mean and sample covariance are affine equivariant
norm(colMeans(y1) - (a %*% colMeans(x) + b), type = "F")
norm(cov(y1) - (a %*% cov(x) %*% t(a)), type = "F")

# What does affine equivariance mean in practice?

# Affine equivariant location and scatter functionals behave as expected under
# coordinate transformations and changes of units. Notice that, for example,
# componentwise median is **not** an affine equivariant location functional.


# f)

# First check your working directory
getwd()

# Read the data
data <- read.table("data/data.txt", sep = "\t", header = FALSE)
head(data)

center <- function(x) {
  pairs(x, pch = 19, col = "midnightblue", gap = 0, upper.panel = NULL,
        cex.labels = 1)
  sweep(x, 2, colMeans(x), "-")
}

data_center <- center(data)
colMeans(data_center) # Indeed, the data is centered now

center_cov <- cov(data_center)
center_cor <- cor(data_center)
eigen(center_cov)$values
eigen(center_cov)$vectors
eigen(center_cor)$values
eigen(center_cor)$vectors
