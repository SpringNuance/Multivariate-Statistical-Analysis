# Demo Problem 1: Multiple Correspondence Analysis

# Read data
house <- read.csv("data/household.csv", header = TRUE)
dim(house)
head(house)

# Perform MCA
house_mca <- ca::mjca(house, lambda = "indicator", reti = TRUE)
names(house_mca)

# MCA is just BCA performed to the indicator matrix
house_ca_ind <- ca::ca(house_mca$indmat)

abs(round(house_mca$colcoord[, 1:2], 2)) ==
  abs(round(house_ca_ind$colcoord[, 1:2], 2))

# Summary
house_summary <- summary(house_mca)
house_summary

# Scree plot
barplot(house_summary$scree[, 3], ylim = c(0, 20),
        names.arg = paste("PC", 1:12), las = 2, xlab = "Component",
        ylab = "% of variation explained", col = "skyblue")

# Column principal coordinates for the first two components
plot(house_mca, arrows = c(TRUE, TRUE))

# Similar plot manually
# Function for scaling values from 0 to 1 (this is for visualization purposes):
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Generate the scatter plot. Point size is now scaled according to qlt:
qlt <- house_summary$columns[, 3]
house_covariates <- house_mca$colpcoord[, 1:2]
plot(house_covariates, pch = 21, asp = 1, bg = "red", cex = normalize(qlt) + 1,
     xlab = paste0("Dimension 1", " (", house_summary$scree[1, 3], "%", ")"),
     ylab = paste0("Dimension 2", " (", house_summary$scree[2, 3], "%", ")"))

# Add arrows. Slight transparency is added to increase visibility.
arrows(rep(0, 17), rep(0, 17), house_covariates[, 1], house_covariates[, 2],
       length = 0, col = rgb(1, 0, 0, 0.25))

# "Cross-hair" is added, i.e., dotted lines crossing x and y axis at 0.
abline(h = 0, v = 0, lty = 3)

# Add variable:category names to the plot.
text(house_covariates, house_mca$levelnames, pos = 2, cex = 0.5)

# Row principal coordinates for the first two components
plot(house_mca, arrows = c(FALSE, FALSE), what = c("all", "none"),
     labels = c(0, 0))

# Column principal coordinates and row standard coordinates for the first two
# components
plot(house_mca, arrows = c(FALSE, TRUE), what = c("all", "all"),
     map = "colprincipal", labels = c(0, 2), cex = 0.5)

# Similar plot manually
# Row standard coordinates
plot(house_mca$rowcoord[, 1:2], cex = 0.5, pch = 4, asp = 1,
     col = rgb(0, 0, 1, 0.1),
     xlab = paste0("Dimension 1", " (", house_summary$scree[1, 3], "%", ")"),
     ylab = paste0("Dimension 2", " (", house_summary$scree[2, 3], "%", ")"))

# Generate the scatter plot. Point size is now scaled according to qlt.
points(house_covariates, pch = 21, bg = "red", cex = normalize(qlt) + 1)

# Add arrows. Slight transparency is added to increase visibility.
arrows(rep(0, 17), rep(0, 17), house_covariates[, 1], house_covariates[, 2],
       length = 0, col = rgb(1, 0, 0, 0.25))

# "Cross-hair" is added, i.e., dotted lines crossing x and y axis at 0.
abline(h = 0, v = 0, lty = 3)

# Add variable:category names to the plot. Now we only use first letters of the
# variables so that the plot does not become too stuffed.
first <- substr(house_mca$levelnames, 1, 1)
last <- substr(house_mca$levelnames, nchar(house_mca$levelnames),
               nchar(house_mca$levelnames))
labels <- paste0(first, last)
text(house_covariates, labels, pos = 3, cex = 0.5)
