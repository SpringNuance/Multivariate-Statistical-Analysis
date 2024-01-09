
data <- read.table("data/decathlon.txt",header=T,row.names = 1)

#View(DEC)

# install.packages("corrgram")

DEC <- data[,-c(1,12,13)]

library(corrgram)

corrgram(DEC)
cor(DEC)[,7]

#Try tuning
corrgram(DEC, panel=panel.pie, diag.panel = panel.minmax, col.regions = colorRampPalette(c("blue4", "blue3", "blue2", "blue1", "blue", "red", "red1", "red2", "red3", "red4")))

DEC.PCA <- princomp(DEC,cor=TRUE)

#(a)
summary(DEC.PCA)
#Like last week

#(b) Choose 4 components since hard to find interpretations for more.
# --> explains 69% variation which is usually very good for real data. 
round(DEC.PCA$loadings[,1:4],2)

#note that the signs can always be reversed

#Comp 1:
# high negative loadings with shot put, discuss throw and high jump

# high positive loadings with R1500 and R400m

# --> Strength again

# Comp 2:
# high negative: R100, Hurdles, Long Jump

# high positive: R1500m 

# --> "Explosive" Speed
# such that "endurance" is on the other side
# of the scale

#Comp 3: Special techniques. Something to do with the supporting leg?
 
#Comp 4: Acrobatics?

plot(DEC.PCA$scores[,1], DEC.PCA$scores[,2])


s <- 3

DEC[49,] <- c(rep(1200,s),rep(800,10-s))

rownames(DEC)[49] <- "outlier"

plot(DEC)

DEC.PCA <- princomp(DEC,cor=T)



scores <- DEC.PCA$scores


# Since the largest variation on the first direction,
# the outlier can easily be detected here
plot(scores[,1],scores[,2],pch=16)

plot(scores[,3], scores[,4], pch=16)
