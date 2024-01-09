
# setwd()

data <- read.table("data/sciencedoctorates.txt",header=T,row.names=1)

dim(data)

S <- data[-13,-9] #remove the total col/row

View(S)

class(S)

D <- S

D <- as.matrix(S)


# prob table wants a matrix
# A <- as.matrix(A)

prop.table(D) # table of relative frequencies

prop.table(D,1) #row profiles
prop.table(D,2) #col profiles

rowSums(prop.table(D,1))
colSums(prop.table(D,2))

margin.table(D,1)
data[,9]

margin.table(D,2)
data[13,]

margin.table(D)
sum(D)

v1 <- margin.table(D,1)
v2 <- margin.table(D,2)

V1 <- matrix(v1,ncol=1) 
V2 <- matrix(v2,nrow=1) 

E <- V1 %*% V2 /sum(D)
AR.matrix <- D/E  #D = original data (number of observations), 
                  #E = expected number of observations under independence

# Values near 1: The year and science are independent
# Values < 1: The science is less frequent in that specific year
# Values > 1: The science is more frequent in that specific year
View(round(AR.matrix,2))

# A nice way to represent the AR-matrix:

library(ggplot2)
library(reshape)

melted <- melt(AR.matrix)
View(melted)
range(melted$value)
ggplot(melted, aes(x=X1, y=X2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = median(melted$value), limit = c(0.5, 1.55),
                       name="AR value")
