

#1.1

#a

DATA <- read.table("data/decathlon.txt",header=TRUE,sep="\t",row.names=1)

# Nice way to check that the upload was succesful
head(DATA)
View(DATA)

# Check the size of the data matrix
dim(DATA)
colnames(DATA)

# Remove total points, height and weight from the analysis
DEC <- DATA[,-c(1,12,13)]

help("princomp")

# Visualize the data
plot(DEC) # Note that since DEC is of the type data.frame R automatically uses the function pairs to plot the variables
# Use plot(as.matrix(DEC)) the see the difference
# More related to different data types in the course Introduction to R-programming
pairs(DEC)
# The above is not very informative here but sometimes reveals if theres is something weird in the data

# A way to plot two specific variables with the names of the athletes:
plot(DEC$R100m,DEC$R400m,xlab="Running 100m",ylab="Running 400m",type="n")
text(DEC$R100m,DEC$R400m,labels=rownames(DEC))

DEC.PCA <- princomp(DEC,cor=FALSE)

names(DEC.PCA)

DEC.PCA$call # input of the function

DEC.PCA$scores # Y from lecture slides

DEC.PCA$n.obs # number of observations

DEC.PCA$scale # Relevant when cor=TRUE

DEC.PCA$center # The sample mean 
colMeans(DEC) # Same as above

DEC.PCA$loadings #matrix of eigenvectors (G-matrix) (columns are eigenvectors)

DEC.PCA$loadings[1,1] #How to access a single value from G

DEC.PCA$sdev # The standard deviation of the principal components


plot(DEC.PCA) # Plots the variances of the principal components

(DEC.PCA$sdev)^2

# Note that the variances of the principal components are equal to the eigenvalues
# of the covariance matrix of the original data matrix

n <- nrow(DEC)
DEC_cov <- (n-1)/n*cov(DEC)

# Note that the princomp package uses the maximum likelihood estimator
# of the covariance matrix (1/n divisor instead of 1/(n-1))

DEC_cov_eval <- eigen(DEC_cov)$values
(DEC.PCA$sdev)^2 #Same values as above

# DEC_cov_eval -(DEC.PCA$sdev)^2 


#b

summary(DEC.PCA)
sum(DEC_cov_eval[1:4])/sum(DEC_cov_eval) 
# Approx 70% of the variation explained with 4 principal components


#c 

# We mainly use the loadings to interpret the principal components
DEC.PCA$loadings #Note that values close to 0 are not visible here
DEC.PCA$loadings[,1:4]


#We choose the first 4 components by looking at the values of loadings and the cumulative proportional variance

# Check what the sports are from wikipedia if you are not familiar with them

#1st component:Strength
# High negative loadings with shot puck and discus throw. Furthermore, a negative loading wiht high
# jump. 
# High positive loading with Running 1500m and a positive loading with r400m

# The high jump is a bit mysterious here. Maybe it is the jumping power?
# Interpretation: Strength (weakness), here a large negative value means that the sport in question
# requires strength. However, athletes with a high body muscle mass are bad in "long" running
# distances, when comparing to other decathletes.
# The fist component explains particularly well the behaviour of variables Shot puck, R1500m and Discuss throw
# ( it explains the variation of the variable.)

# 2nd component: Speed
# High negative loading with R100 and a negative loading with hurdles. A negative loading with R400. 
# Positive loading with high jump, javelin and R1500m
# Interpretation: Speed (slowness). R100m, 110m hurdles and r400m require speed from the athlete.
# However, R1500 is more about stamina and not about top speed.
# Here javelin and high jump a bit mysterious.
# The second component explains particularly well the behaviour of variable R100

#Note that the scale of strength and speed reversed, see above
plot(DEC.PCA$scores[,1],DEC.PCA$scores[,2],type="n",xlab="Strength",ylab="Speed") 
text(DEC.PCA$scores[,1:2],labels=rownames(DEC))


# 3rd and 4th component: Technique 1 and Technique 2
# These components explain sports that require a special technique to perform well.
# Look the loadings yourself and determine which sports are related to these components.
# You should note that from summary(DEC.PCA), we see that the 3rd and 4th component explain 
# together less variation than the 1st component alone. Hereby, they are not as important as the
# first two.

# NOTE: Often the best possible interpretations require the help of an expert related to the
# phenomenon at hand. 

# d
cov(DEC.PCA$scores) # Diagonal, as expected
(n-1)/n*diag(cov(DEC.PCA$scores)) # The diagonal elements are equal to the variances of the principal components
colMeans(DEC.PCA$scores) # Zero as expected
