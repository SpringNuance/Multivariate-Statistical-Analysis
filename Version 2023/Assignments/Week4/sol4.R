# install.packages(c("MASS","rrcov"))

library(MASS)
library(rrcov)

# (a), (b)

D <- read.table("data/wood.txt",header=TRUE,sep="\t")
dim(D)
plot(D)
# Again, you might see some outliers,
# Note that when data is truly high dimensional,
# you cannot easily detect outliers visually

# (c)

# Classical unbiased covariance
scov <- cov(D)
scov

# MCD covariance
# try different values of alpha

# previously there was bug with alpha=1, seem to be fixed now (2017)
rcov <- CovMcd(D,alpha=1/2)
# Note that the above is a S4 class object
# Most of the object we have in this course are of class S3
# We will not discuss the differences of the objects (between S3 and S4) in this
# course. 

# The difference relevant to this course is that 
# to access slots of an S4 object you use @, not $:
rcov@cov

# The S3 alternative for CovMcd is covmcd:

# covMcd(D,alpha=1/2)$cov

# however, since some of the later functions in these exercises do not have S3 alternatives,
# we use the CovMcd function here

# (d)

## Using sample covariance
# Note that the next command returns mahalanobis^2

s.maha <- mahalanobis(D,center=colMeans(D),cov=scov)

# If the original data is normally distributed
# --> Squared Mahalanobis distance follows chi^2 distribution with 
# p degrees of freedom (p=5, since 5 variables)

#qqplot(qchisq(ppoints(20),df=5),s.maha,main="Using sample covariance matrix")
#qqline(s.maha,distribution = function(p) qchisq(p,df=5))




# Using Robust

r.maha <- mahalanobis(D,center=rcov@center, cov=rcov@cov)

#qqplot(qchisq(ppoints(20),df=5),r.maha,main="Using MCD")
#qqline(r.maha,distribution = function(p) qchisq(p,df=5), probs=c(0.075,0.875))


plot(c(1,nrow(D)),range(sqrt(c(s.maha,r.maha))),type="n",
     xlab="Observation",ylab="Mahalanobis-Distance")

points(1:nrow(D),sqrt(s.maha),col="blue")

points(1:nrow(D),sqrt(r.maha),col="red",pch=16)


legend("topleft",col=c("blue","red"),cex=0.8,legend=c("Classical","MCD"),
       pch=c(1,16))


# Clearly, when using MCD we see some potential outliers,
# however, using classical covariance the outliers are not visible 

# (e)

apply(D,2,range)
# It seems that the scales of the variables are different
# Also, we do not know if the variables are of the same units
# Hereby, we use the correlation based PCA here

# Correlation based PCA
PCA1 <- princomp(D,cor=T)
PCA1$loadings


# MCD based PCA
PCA2 <- PcaCov(D,scale=TRUE,cov.control = CovControlMcd(alpha=1/2)) 
PCA2@loadings
# see help(PcaCov)
# You can verify that PCA1 and PCA2 are equal if alpha=1
# The scale=TRUE in PcaCov() means that we perform the correlation based PCA


# You can see the objects inside an S3 object with names():
names(PCA1)

# Likewise, for S4 objects use slotNames():
slotNames(PCA2)


# Summary and plotting works for both types
summary(PCA1)
summary(PCA2)

# From Exercises 3, check what happens to regular PCA,
# when outliers are present
par(mfrow=c(1,2))

plot(PCA1)
screeplot(PCA2)

dev.off()

PCA1$loadings
PCA2@loadings


#completely different loadings
# --> different interpretations for the principal components

pairs(PCA1$scores)
pairs(PCA2@scores)

####
#2
#####
set.seed(123)
n <- 200
library(mvtnorm)

D1 <- rmvnorm(n,mean=c(0,0),sigma=diag(2))
plot(D1)

D2 <- rmvt(n,df=5,sigma=diag(2))
plot(D2)

x1 <- rweibull(n,shape=1,scale=2)
x2 <- rgamma(n,shape=2,scale=1)

D3 <- cbind(x1,x2)
plot(D3)

cov1 <- cov(D1)
rcov1 <- CovMcd(D1,alpha=1/2)@cov

cov2 <- cov(D2)
rcov2 <- CovMcd(D2,alpha=1/2)@cov

cov3 <- cov(D3)
rcov3 <- CovMcd(D3,alpha=1/2)@cov
