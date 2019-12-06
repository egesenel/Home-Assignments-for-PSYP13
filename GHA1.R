##################################################################
###### PSYP13 Sub-course 1 - GP Assignment 1 - by Ege Şenel ######
##################################################################

# Load data
PAQ_Ege <- read.csv("~/Desktop/Uni Stuff/Lund/PSYP13 Advanced Scientific Methods in Psychology/More excercises/PAQ_Ege.txt", sep="")

# Load packages
library(psych)
library(tidyr)
library(corrplot)
library(REdaS)
source('cor.matrix.scr.R')
source("http://www.sthda.com/upload/rquery_cormat.r")

# Reformat the data
PAQ_Ege1 <- spread(PAQ_Ege, 
                   key = "var",
                   value = "value")
PAQ_Ege1

summary(PAQ_Ege1)
describe(PAQ_Ege1$sex)
str(PAQ_Ege1)

# Getting rid of missing values
PAQ_Ege2 <- na.omit(PAQ_Ege1)

# Identifying leverages and multivariate outliers 
rownames(PAQ_Ege2) <- 1:nrow(PAQ_Ege2)

regr01 <- lm(formula = id ~ age + Q1_cry + Q2_help + Q3_breathe + Q4_freeze + Q5_alien + 
                Q6_inferior + Q7_weep + Q8_Support + Q9_Nerd + sex, data = PAQ_Ege2)

lev <- hat(model.matrix(regr01))
PAQ_Ege2[lev > .045,]  
plot(lev)

N <- nrow(PAQ_Ege2) # N is the number of rows (pps) in our data set
mahad <- (N-1)*(lev-1/N) # mahad = (df)*(leverage model-1)/number of pps
tail(sort(mahad),11)                        
order(mahad,decreasing=T)[c(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)]   

# Removing the unnecessary variables for the  PCA analysis
PAQ_Ege3 <- PAQ_Ege2[c(-1,-2, -12)]

# Check the reducibility with KMO and Bartlett's sphericity
cor.matrix <- rquery.cormat(PAQ_Ege3, type="full")

KMO(cor.matrix$r)

bart_spher(cor.matrix$r)

# Correlation matrix
data_pcacor=princomp(PAQ_Ege3,cor=TRUE)
summary(data_pcacor, loadings=TRUE)
# Covariance matrix
data_pcacov=princomp(PAQ_Ege3,cor=FALSE)
summary(data_pcacov, loadings=TRUE)

# Bar plot for explained variance
p = princomp(PAQ_Ege3)
p.variance.explained = p$sdev^2 / sum(p$sdev^2)
loadings = p$loadings[]
p.variance.explained = p$sdev^2 / sum(p$sdev^2)
barplot(100*p.variance.explained, las=2, xlab='', ylab='% Variance Explained', col ="gray")

# eigenvalues
eig <- (data_pcacor$sdev)^2
eig
mean(eig)

# Plots
x11()
par(mfrow=c(2,1))
plot(data_pcacor$sdev^2, xlab = "Component number",
     ylab = "Component variance", type = "l", 
     main = "Scree diagram")
plot(log(data_pcacor$sdev^2), xlab = "Component number",
     ylab = "log(Component variance)", type="l",
     main = "Log(eigenvalue) diagram")

# Biplot of the first two principal components
biplot(data_pcacor, col = c("gray90", "black"))



