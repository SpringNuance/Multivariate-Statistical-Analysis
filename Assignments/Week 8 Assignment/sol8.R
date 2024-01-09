car <- read.table("data/car.txt",header=T,sep="\t")

# Convert X and Y to matrices for future operations
# (note that e.g. matrix multiplication not possible for data.frame objects)

X <- as.matrix(car[,c(6,5)]) # X = (Price,Value)
Y <- as.matrix(car[,c(3,4,7:10)]) # Y = (Economy,Service, Desing,Sport,Safety,Easy.h)



XY <- cbind(X,Y)

# (a)
# Compute sample canonical vectors with corrected scaling

R <- cov(XY) 
R11 <- R[1:2,1:2]  # cov(X)
R22 <- R[3:8,3:8]  # cov(Y)
R21 <- R[3:8,1:2]  # cov(Y,X)
R12 <- R[1:2,3:8]  # cov(X,Y)
R11.inv <- solve(R11)
R22.inv <- solve(R22)

#Non-zero eigenvalues of M1 and M2 are the same
M1 <- R11.inv %*% R12 %*% R22.inv %*% R21
M2 <- R22.inv %*% R21 %*% R11.inv %*% R12


va1 <- eigen(M1)$vectors[,1]
va2 <- eigen(M1)$vectors[,2]

vb1 <- eigen(M2)$vectors[,1]
vb2 <- eigen(M2)$vectors[,2]

#Remove the "ghost" imaginary parts
vb1=as.numeric(vb1)
vb2=as.numeric(vb2)

# Correct scaling. We scale with the standard deviation of the scores
a1 <- va1/sqrt(va1%*%R11%*%va1)
a2 <- va2/sqrt(va2%*%R11%*%va2)
b1 <- vb1/sqrt(vb1%*%R22%*%vb1)
b2 <- vb2/sqrt(vb2%*%R22%*%vb2)

# Another approach (maybe more intuitive) to get the scaling right
eta1 = X %*% va1  #Scores on the first canonical correlation variable related to X
a1 = va1/sqrt(var(eta1))

var(eta1)-va1%*%R11%*%va1

# The condition for correct scaling is that the following are 1
a1 %*% R11 %*% a1 #Note that here R automatically transposes the first vector
a2 %*% R11 %*% a2
b1 %*% R22 %*% b1
b2 %*% R22 %*% b2


# Note that the following are 0
a1 %*% R11 %*% a2 
b1 %*% R22 %*% b2

# (b)
# Score vectors:
eta1 <- X%*%a1
eta2 <- X%*%a2
fii1 <- Y%*%b1
fii2 <- Y%*%b2

# sample canonical vectors scaled such that the score vectors have variance 1,
# this scaling makes them unique up to sign

var(eta1)
var(eta2)
var(fii1)
var(fii2)


# Matrix of the canonical correlations
round(cor(cbind(eta1,eta2,fii1,fii2)),2)
#canonical correlations
# r1 = 0.98
# r2 = |-0.91| = 0.91
# The relationship between both pairs seems to be quite strong.

# Note that the canonical correlations are also the square roots of the non-zero
# eigenvalues of M1,M2

round(sqrt(eigen(M1)$values),2)


# (c)

# Value = "Loss of value" = "How fast the value goes down" 

# Note that u1,v1 are unique up to sign!

# a1 <- 0.32Price - 0.62Value 
# b1 <- 0.43Economy  - 0.21 Service +0Desing - 0.47Sport - 0.22Safety 
#             -0.4 Easy.h

# Recall: 1 = very good, 6 = very bad

# Here the interpretations are a bit tedious, since high values correspond to very bad and 
# low values correspond to very good. 

# Variables Price and Value have opposite signs.
# --> Variables Economy, Service, Desing, Sport, Safety and Easy Handling have the 
 #opposite relation to them




plot(eta1,fii1,xlab="'Value index' of the car",
       ylab="'Quality' of the car",pch="")
text(eta1,fii1,labels=paste(car$Type,car$Model))

# In the plot, x-axis:
# The most left point would be a car with Value=6 (very bad) and Price = 1 (very good)
# --> On the left we have cheap cars that lose value fast (e.g. Wartburg, Trabant)

# The most right point would be a car with Value=1 (very good) and Price = 6 (very bad)
# --> On the right we have cars that are expensive but lose value slowly (e.g. BMW, Jaguar, Ferrari)

# We interpet u1 as the value index of the car, such that high scores on the x-axis indicate that 
# the car is a "worthy" investment. Note also, that Value has almost twice as much weight in the scores
# here (when compared to Price).  

# 6 = very bad, 1 = very good
# v1 <- 0.43Economy - 0.21 Service +0Desing - 0.47Sport - 0.22Safety - 0.4 Easy.h

#y-axis:
# Here, Economy, Sport and Easy handling have the largest weight in this component,
# desing has negligible effect on the scores. 

# The uppermost points have very bad Economy and very good Service, Sport, Safety and Easy.h
# --> The uppermost cars use a lot of fuel but the other qualities are good
# (e.g. BMW, Ferrari)

# The bottommost points have very good economy but the other qualities are very bad
# --> The bottommost cars are economical but other qualities are bad. 

# v1 can be interpreted as a quality index of the car, such that you get a sporty car that is easy 
# to drive in the expense of economy. 





# a2 <- - 1.41Price - 1.42 Value 
# b2 <-  0.46Economy +0.70Service  - 0.06Desing  + 0.00Sport -  0.30Safety  +1.01Easy.h

# Interpret the second component similarly, might be harder (but not impossible)
# since e.g. Ferrari and Wartburg have
# similar profiles. 

plot(eta2,fii2,xlab="u2",
     ylab="v2",pch="")
text(eta2,fii2,labels=car$Type)
