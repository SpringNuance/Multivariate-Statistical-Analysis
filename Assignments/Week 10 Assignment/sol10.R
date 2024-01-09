
# We will use the MASS-library
library(MASS)

# First, we read the data

poll <- read.table("data/polls.txt",header=T,sep="\t",row.names=1)
View(poll)

# Create labels for each region
label <- c("UM","VS","KH","PiM","PH","KL","PS","PK","PoM","K","L")


# a) Scatter plot the variables. Can you spot the different clusters?
plot(poll, panel = function(x,y) {text(x,y,labels=label,xpd=T)}) 


# b) Calculate the euclidean distances between the countries.
poll.dist <- dist(poll,method="euclidean")
rounded <- round(poll.dist,1)
View(as.matrix(rounded))

# c) Perform the “bottom up” hierarchical clustering by hand. 
#    Aggregate two clusters using the minimum distance (single linkage).
min(poll.dist)
sort(round(poll.dist,1))

#(Paijat-Hame, Kymenlaakso) 3.6
#(Kanta-Hame, Paijat-Hame, Kymenlaakso) 4.8
#(Kanta-Hame, Paijat-Hame, Kymenlaakso), (Varsinais-Suomi, Pirkanmaa) 7.1
#(Kanta-Hame, Paijat-Hame, Kymenlaakso, Varsinais-Suomi, Pirkanmaa) 7.7
#(Kanta-Hame, Paijat-Hame, Kymenlaakso, Varsinais-Suomi, Pirkanmaa), (Lappi, Kainuu) 7.8
#(Kanta-Hame, Paijat-Hame, Kymenlaakso, Varsinais-Suomi, Pirkanmaa), (Lappi, Kainuu), (Pohjois-Savo, Pohjois-Karjala) 9.0 
#(Kanta-Hame, Paijat-Hame, Kymenlaakso, Varsinais-Suomi, Pirkanmaa, Uusimaa), (Lappi, Kainuu), (Pohjois-Savo, Pohjois-Karjala) 12.3
#(Kanta-Hame, Paijat-Hame, Kymenlaakso, Varsinais-Suomi, Pirkanmaa, Uusimaa), (Lappi, Kainuu, Pohjois-Savo, Pohjois-Karjala) 12.8 
#(Kanta-Hame, Paijat-Hame, Kymenlaakso, Varsinais-Suomi, Pirkanmaa, Uusimaa, Lappi, Kainuu, Pohjois-Savo, Pohjois-Karjala) 16.6
#(Kanta-Hame, Paijat-Hame, Kymenlaakso, Varsinais-Suomi, Pirkanmaa, Uusimaa, Lappi, Kainuu, Pohjois-Savo, Pohjois-Karjala, Pohjanmaa) 47.1



# d) Repeat (c) using the function hclust().
# Simply call on the hclust-function
poll.min <- hclust(poll.dist,method="single")


# e) Plot the classification tree (dendrogram).
# This can be done with the plot-function:

plot(poll.min, main="Single")


# f) Repeat the steps by aggregating the clusters using the average 
#    (average link- age) and the maximum (complete linkage). 
#    Compare the results.

# First, the maximum linkage
poll.max <- hclust(poll.dist,method="complete")
plot(poll.max, main="Complete")

# Second the average linkage
poll.ave <- hclust(poll.dist,method="average")
plot(poll.ave, main="Average")

# g) Where would you cut the tree?

# Here is some code which can be used to test the clusters with


Minp <- cutree(poll.min,k=5)
Minp

Maxp <- cutree(poll.max,k=2)
Maxp

Avep <- cutree(poll.ave,k=4)
Avep

# 


####
#2
####
library(cluster)

BANK <- read.table("data/bank.txt",header=T,sep="\t")
n <- nrow(BANK)

cols <- rep(NA,n)
cols[BANK$CODE == 0] <- "blue"
cols[BANK$CODE == 1] <- "red"
plot(BANK[,-1],col=cols,pch=16)

plot(BANK[,-1],type='n')
text(BANK[,-1],label=1:n)

set.seed(100) #500 yields nicely differing results for 3 centers
k.mean <- kmeans(BANK[,-1],centers=2)
table(k.mean$cluster,BANK[,1])
# zero wrong ones in the 1st category
# 95 right ones in the 2nd category

clusplot(BANK[,-1],k.mean$cluster,color=T,shade=T)
plot(BANK[,-1], col=k.mean$cluster, pch=16)  # Perhaps just using the standard plot() is simpler

# With 3 centers
k.mean3 <- kmeans(BANK[,-1],centers=3)
table(k.mean3$cluster)  # With a simple table of the number of elements in each cluster we can see the difference.


clusplot(BANK[,-1],k.mean3$cluster,color=T,shade=TRUE)
plot(BANK[,-1], col=k.mean3$cluster, pch=16)

# If you want to manually set the colors for plot(), you have to specify a vector of colors. For example:
colvec <- c("blue", "green", "magenta")
cols <- rep(NA, nrow(BANK))
for(i in 1:length(colvec)){
  cols[k.mean3$cluster==i] <- colvec[i]
}
plot(BANK[,-1], col=cols, pch=16)

# The seed does not seem to affect the results when we have 2 clusters
# However, it does have an effect for 3 clusters

# If there is a problem with clusplot, installing the package fpc might help
# library(fpc)
