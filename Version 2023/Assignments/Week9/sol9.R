library(MASS)

#(a)

data(iris)
# View(iris)
head(iris)
help(iris)



D <- iris[51:150,]

cols <- rep(NA,100)

cols[D$Species == "versicolor"] <- "blue"
cols[D$Species == "virginica"] <- "red"
pairs(D[,1:4],col=cols,pch=16)

# You can similarly check that setosa is quite far from the other two in all variables


class(D[,5]) #now the fifth column contains factors
              # note that sometimes you have to manually set one of the columns to factors

help(lda)

# This gives an error since the variable "remembers" that there is a factor level
# called setosa. However, the error does not effect the results. 
D.lda <- lda(Species~. ,data=D) 

# You can remove the error by modifying the data set as follows
D[,5] <- droplevels(D[,5]) #This drops the empty factor level

D.lda <- lda(Species~. ,data=D) #used similarly as the function lm()
# Species~. is equivalent to Species~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

names(D.lda)

# The important object in D.lda:
D.lda$prior # prior probabilities that you can manually assing (we do not use them here)
D.lda$N  # Total number of observations
m <- D.lda$means # colMeans for versicolor and virginica
a <- D.lda$scaling #the vector a
# note that if $scaling contains more than one column, the vector a that corresponds to 
# Fisher's linear discriminant analysis is always the first column. 

# Verify that it is equal to the eigenvector of W^(-1)B

S1 <- cov(D[1:50,1:4])
S2 <- cov(D[51:100,1:4])
d1 <- colMeans(D[1:50,1:4])
d2 <- colMeans(D[51:100,1:4])

d <- d1 - d2
d = as.matrix(d,ncol=1)

B <- (50*50)/100 * d %*% t(d)
W <- 49*(S1 + S2)
L <- solve(W) %*% B
a2 <- eigen(L)$vectors[,1]

e <- eigen(L)$values[1]

# Note that eigenvectors are not unique and
norm(a,type="2") # not 1
norm(a2,type="2") # equal to 1

# Scale a to have length 1:
a.scaled <- a/norm(a,type="2")

a.scaled
a2
as.numeric(a2)
# Thus vector a is equal to the eigenvector corresponding 
# to the largest (only nonzero here) eigenvalue


#(b)

# new observation = (6, 3, 4, 1)

newdat <- data.frame(Sepal.Length=6,Sepal.Width=3,Petal.Length=4,
                     Petal.Width=1)

predict(D.lda,newdata=newdat)$class # New flower classified as versicolor

#(c)

D.cv <- lda(Species~. ,data=D,CV=T) #Results for leave-one out cross-validation

result <- data.frame(Est=D.cv$class,Truth=D[,5])

tab <- table(result) #truth table, here 2 versicolor are classified as virginica and
# 1 virginica is classified as versicolor
      
# Hereby, the missclassification rate is 3/100
