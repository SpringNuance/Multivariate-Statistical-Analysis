# Demo Problem 1

library(MASS) # For Fisher's linear discriminant analysis

# Read the data, note that column species is a factor
data(iris)

iris <- droplevels(iris[-(1:50), ])
str(iris)

# Visualize
pairs(iris[, 1:4], pch = c(16, 17)[iris$Species], gap = 0, upper.panel = NULL,
      col = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5))[iris$Species])
par(xpd = NA)
legend(0.75, 0.75, legend = levels(iris$Species), pch = c(16, 17),
       col = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)), cex = 1)

# a)

# Perform Fisher's linear discriminant analysis with the function lda
# Species ~ . is a shorthand for
# Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_lda <- lda(Species ~ ., data = iris)
a_lda <- iris_lda$scaling
a_lda

# Perform Fisher's linear discriminant analysis manually
# cov(X_1)
s1 <- cov(iris[1:50, 1:4])

# cov(X_2)
s2 <- cov(iris[51:100, 1:4])

d1 <- colMeans(iris[1:50, 1:4])
d2 <- colMeans(iris[51:100, 1:4])
d <- as.matrix(d1 - d2, ncol = 1)

b <- (50 * 50) / 100 * d %*% t(d)
w <- 49 * (s1 + s2)
l <- solve(w) %*% b

# a is the eigenvector corresponding to the largest eigenvalue of W^(-1)B
a_manual <- eigen(l)$vectors[, 1]

# When g = 2, a = W^(-1)d
a_manual2 <- solve(w) %*% d

# a_lda, a_manual and a_manual2 are equal up to scale
c(norm(a_lda, type = "2"), norm(a_manual, type = "2"),
  norm(a_manual2, type = "2"))

a_lda <- a_lda / norm(a_lda, type = "2")
a_manual2 <- a_manual2 / norm(a_manual2, type = "2")

data.frame(a_lda = a_lda, a_manual = a_manual, a_manual2 = a_manual2)


# b)
# New observation is classified as versicolor
newobs <- data.frame(Sepal.Length = 6.08, Sepal.Width = 2.76,
                     Petal.Length = 4.6, Petal.Width = 1.44)
predict(iris_lda, newdata = newobs)$class

# Classification can be also done manually
abs(t(a_lda) %*% (as.numeric(newobs) - d1)) <
  abs(t(a_lda) %*% (as.numeric(newobs) - d2))


# c)
# Leave-out-one cross-validation
d_cv <- lda(Species ~ ., data = iris, CV = TRUE)
result <- data.frame(est = d_cv$class, truth = iris[, 5])
table(result)

# Misclassification rate is 0.03

# Leave-out-one cross-validation manually
predicted <- rep(NA, 100)
for (i in 1:100) {
  train <- iris[-i, ]
  test <- iris[i, ]
  predicted[i] <- predict(lda(Species ~ ., data = train),
                          newdata = test)$class
}
predicted <- factor(predicted, levels = c(1, 2),
                    labels = c("versicolor", "virginica"))
sum(predicted != iris$Species) / nrow(iris)


# Demo Problem 2

library(ddalpha) # For depth based classification

# a)
# Compute sample halfspace depth for each point with respect to its own species
depths_versicolor <- depth.halfspace(iris[1:50, 1:4], iris[1:50, 1:4],
                                     exact = TRUE)
depths_virginica <- depth.halfspace(iris[51:100, 1:4], iris[51:100, 1:4],
                                    exact = TRUE)

# Visualize, points are colored according to the value of depth

# Versicolor
pal <- colorRampPalette(c("blue", "red"))
n_col <- length(unique(depths_versicolor))
ranks <- as.numeric(as.factor(depths_versicolor))

pairs(iris[1:50, 1:4], pch = 16, gap = 0, upper.panel = NULL,
      col = pal(n_col)[ranks])
par(xpd = NA)
legend(0.75, 0.75, legend = range(depths_versicolor), pch = 16,
       col = pal(2), cex = 1)

# Virginica
n_col <- length(unique(depths_virginica))
ranks <- as.numeric(as.factor(depths_virginica))

pairs(iris[51:100, 1:4], pch = 16, gap = 0, upper.panel = NULL,
      col = pal(n_col)[ranks])
par(xpd = NA)
legend(0.75, 0.75, legend = range(depths_virginica), pch = 16,
       col = pal(2), cex = 1)


# b)
# New observation is classified as versicolor
depth.halfspace(newobs, iris[1:50, 1:4], exact = TRUE) >
  depth.halfspace(newobs, iris[51:100, 1:4], exact = TRUE)

# c)
# Misclassification rate is 0.26, that is much higher than in Problem 1
predicted <- rep(NA, 100)
for (i in 1:100) {
  train <- iris[-i, ]
  test <- iris[i, 1:4]
  train_versicolor <- train[train$Species == "versicolor", 1:4]
  train_virginica <- train[train$Species == "virginica", 1:4]

  cond <- (depth.halfspace(test, train_versicolor, exact = TRUE) >
             depth.halfspace(test, train_virginica, exact = TRUE))

  predicted[i] <- ifelse(cond, 1, 2)
}
predicted <- factor(predicted, levels = c(1, 2),
                    labels = c("versicolor", "virginica"))
sum(predicted != iris$Species) / nrow(iris)
