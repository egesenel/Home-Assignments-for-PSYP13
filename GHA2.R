##################################################################
###### PSYP13 Sub-course 1 - GP Assignment 2 - by Ege Şenel ######
##################################################################
# Load data
Nations <- read.delim("~/Desktop/Uni Stuff/Lund/PSYP13 Advanced Scientific Methods in Psychology/More excercises/Nations.txt")
Nations.original <- Nations

# Loading packages:
library(psych)    # for describe function
require(smacof)   # needed for the sim2diss function
require(MASS) 
library(rgl)

# Check the data
describe(Nations)
summary(Nations)

# Dissimilarities
Nations.d <- sim2diss(Nations, method = 7, to.dist = TRUE) 
print(Nations)
print(Nations.d)

# Non-metric multidimensional scaling
Nations.mds <- isoMDS(Nations.d, maxit = 12)
Nations.mds$points
Nations.mds$stress # Cite (Kruskal, 1964) when describing the cutoff points for stress values.

print(Nations.mds)

# Getting coordinates
x <- Nations.mds$points[,1] # 1 refers to 1st coordinate
y <- Nations.mds$points[,2] # 2 refers to 2nd coordinate

print(x)
print(y)

# Plot the coordinates
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
           xlim = range(Nations.mds$points[,1])*1.2, type = "n")
text(x, y, labels = colnames(Nations), cex = 0.6)
abline(h=0, v=0, col = "gray60", lty = 2)       # putting in vertical and horizontal lines
 
# Shepard plot
Nation_shep <- Shepard(Nations.d, Nations.mds$points)
 plot(Nation_shep, pch = "°", xlab = "Dissimilarity",
      ylab = "Distance", col= "darkgray", main = "Shephard's Diagram")
 lines(Nation_shep$x, Nation_shep$yf, type = "S")

# Reducing to three dimensions instead of two 
 Nations.mds_3 <- isoMDS(Nations.d, k = 3)
 
 x1 <- Nations.mds_3$points[,1] # 1 refers to 1st coordinate
 y1 <- Nations.mds_3$points[,2] # 2 refers to 2nd coordinate
 z1 <- Nations.mds_3$points[,3] # 3 refers to 3rd coordinate
 
 plot(x1, z1, xlab = "Coordinate 1", ylab = "Coordinate 3",
      xlim = range(Nations.mds$points[,1])*1.2, type = "n")
 text(x1, z1, labels = colnames(Nations), cex = 0.6)
 abline(h=0, v=0, col = "gray60", lty = 2)
 
 Nation_shep_3d <- Shepard(Nations.d, Nations.mds_3$points)
 plot(Nation_shep_3d, pch = "°", xlab = "Dissimilarity",
      ylab = "Distance", col= "darkgray", main = "Shephard's Diagram")
 lines(Nation_shep$x, Nation_shep$yf, type = "S")
 
 # 3D Plot 
 plot3d(x1, y1, z1, 
        xlab = "Coordinate 1", ylab = "Coordinate 2", zlab = 'Coordinate 3',)
 text3d(x1, y1, z1, texts = colnames(Nations), cex = 0.6)
 
 #Animate by spinning on Y & Z axes 
 play3d(spin3d(axis=c(0,1,1), rpm=5), duration=15)
 
 