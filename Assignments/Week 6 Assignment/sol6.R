library(ca)
library(psych)
data <- read.table("data/sciencedoctorates.txt",header=T,sep="\t",row.names=1)
# View(data)

dim(data)
# Again, remove the total col/row
SD <- data[-dim(data)[1],-dim(data)[2]]
dim(SD)


# To interpret correspondence analysis, the first step is to evaluate whether there is a significant
# dependency between the rows and columns.

n <- sum(SD)

v1 <- matrix(colSums(SD),nrow=1)
v2 <- matrix(rowSums(SD),ncol=1)

#theoretical frequencies under independence
E <- v2 %*% v1/n

I <- dim(SD)[1]
J <- dim(SD)[2]

s <- 0
#chi-square statistic
for(i in 1:I){
  for(j in 1:J){
    s <- s + ( SD[i,j]-E[i,j] )^2/(E[i,j])
  }
}

pchisq(s,df=((I-1)*(J-1)),lower.tail=F)

# H0: Discipline and Year are independent
# H1: Discipline and Year are not independent

chisq.test(SD)
# there is evicende that there is statistically
# significant association between the number
# of doctors graduated and the year (in USA)


#ca-function
help(ca)
SD.ca = ca(SD) #set nd=8 to make all columns visible in summary
names(SD.ca)
SD.ca$colnames
SD.ca$rownames
SD.ca$N
SD.ca$sv #The square roots of singular values related to the PCA transformation for rows/cols
# (how much variation explained by the principal components)
# for symmetric matrices, singular values = |eigenvalues|
# (here sv's are used since the package uses svd instead of eigen)

########################
# Inertia
########################

# How much of the total "variation" the specific variable explains
# i.e. how much it contributes to the chi-squared statistic

# Inertia is the chi squared statistic divided by n

SD.ca$rowinertia 
SD.ca$colinertia

# You can get the single row-inertia values by fixing i (the row index)
s2 <- 0

#For physics, i =3

i <- 3
for(j in 1:J){
  s2 <- s2 + ( SD[i,j]-E[i,j] )^2/(E[i,j])
}

s2/n 

#Note that
sum(SD.ca$rowinertia)
sum(SD.ca$colinertia)
#is the same as
sum(SD.ca$sv^2)
s/n 

SD.ca$rowinertia/sum(SD.ca$sv^2) #these proportional values are the ones seen in summary(SD.ca)
SD.ca$colinertia/(s/n)
###############################################


#ca manually ###################################

#theoretical relative frequencies under independence
Ef <- E/n
#observed relative frequencies
SDf = SD/n

#the matrix Z
Z = (SDf - Ef)/sqrt(Ef)
class(Z)
Z = as.matrix(Z)

#matrices V and W
V = t(Z)%*%Z
W = Z%*%t(Z)

#chi-squared test statistic
n*tr(V)

variances = eigen(V)$values
SD.ca$sv^2
components = eigen(V)$vectors

#V and W has the same non-zero eigenvalues
eigen(W)$values
components2 = eigen(W)$vectors

#forming the matrix R
f1 = v1/n #SD.ca$colmass
f2 = v2/n #SD.ca$rowmass
one = matrix(rep(1,nrow(SD)), ncol=1)
sifting = one%*%sqrt(f1)
scaling = f2 %*% sqrt(f1)
R = SDf/scaling - sifting

#rowcoordinates
rowcoord = as.matrix(R)%*%components
#omit the dimension corresponding to zero eigenvalue
rowcoord = rowcoord[,-8]
summary(SD.ca)

#standardized rowcoordinates
stand = matrix(rep(1,nrow(SD), ncol=1))%*%matrix(variances[-8], nrow=1)
standrowcoord = rowcoord/sqrt(stand)
SD.ca$rowcoord

#forming the matrix C
one2 = matrix(rep(1,ncol(SD)), nrow=1)
sifting2 = sqrt(f2)%*%one2
scaling2 = sqrt(f2) %*% f1
C = SDf/scaling2 - sifting2

#colcoordinates
colcoord = t(C)%*%components2
#omit dimensions corresponding to zero eigenvalues
colcoord = colcoord[,-c(8:ncol(colcoord))]
summary(SD.ca)

#standardized colcoordinates
stand2 = matrix(rep(1,ncol(SD), ncol=1))%*%matrix(variances[-8], nrow=1)
standcolcoord = colcoord/sqrt(stand2)
SD.ca$colcoord

## The chi-squared distances from the "center", where variables close to center do not deviate from the
# independence assumption.
sqrt(rowSums(rowcoord^2))
SD.ca$rowdist

#coldistances
sqrt(rowSums(colcoord^2))
SD.ca$coldist
####################################################

SD.ca
summary(SD.ca)
# Note that the quantities are multiplied by 1000
# Quality of representation = as in the lecture slides, but here we consider the angle between profiles and 
# the plane spanned by the two first principal components
# Squared correlations = quality of representation from lecture slides
# also, the sum of the squared correlations is the quality of representation.

# ctr = contribution in forming that ca-component (contributions sum to 1)
# important variables related to forming the specific component have a high ctr 

# k=1 and k=2 are the coordinates on the plot 

names(summary(SD.ca)$rows)

# Contribution of engineering to the second axis
f2[1]*SD.ca$rowcoord[1,2]^2 #recall that these coordinates are already scaled
# If the rows and columns were independent, ctr would be same for every variable

# Squared correlation of biology with the second component
d2 = rowcoord[6,]^2
d2[2]/sum(d2)

#Note that the following plots unscaled coordinates (principal coordinates) and hence, deduction based on the plot is questionable
plot(SD.ca,arrows=c(T,T),map="symmetric") 
#plot(SD.ca,arrows=c(T,T),map="symmetric",dim=c(2,4))

#Instead try e.g. following commands
plot(SD.ca, arrows=c(T,T), map="rowprincipal") #standard column coordinates
plot(SD.ca, arrows=c(T,T), map="rowgreen") #standard column coordinates scaled with square root of the column masses
plot(SD.ca, arrows=c(T,T), map="colgab", dim=c(3,4)) #standard row coordinates scaled with the row masses
plot(SD.ca, arrows=c(T,T), map="rowgab") 

# If two row-variables are close on the picture, they have a similar profile,
# the same is true for column-variables

# Distant row/column-variables have different profiles

# Variables distant from the origin represent variables different from the average profile
# these are usually the most interesting ones

# Now you can again try to interpret the dimensions.
# 1st dim splits the sciences into soft/hard
# 2nd dim splits the sciences into more formula heavy(math,physics,engineering) vs
# the more experimental ones (chemistry,agriculture,earthsciences)

# same for different years

# You can also try this:
plot3d.ca(SD.ca)
