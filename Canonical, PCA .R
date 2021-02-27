# ST 558: Multivariate Analytics
# Module 6 R Activity Example Script File

###########
# Step 2  #
###########

# Set working directory 

setwd('/Users/scemerson/Documents/Old Computer Documents/ST 558 2017/Datasets')

# Install and load the 'CCA' R library

install.packages('CCA')
library(CCA)

# Read in 'NYSEData.csv' dataset 

nyse <- read.csv('NYSEData.csv')
exam <- read.csv('TestScoreData.csv')

###########
# Step 3  #
###########

x1 <- as.matrix(nyse[,1:2])
x2 <- as.matrix(nyse[,3:5])

sig11 <- cov(x1)
sig22 <- cov(x2)
sig12 <- cov(x1, x2)

# Compute canonical variates:

# Square root matrices:

sig11.eig <- eigen(sig11)
sig11.5 <- sig11.eig$vec %*% diag(sqrt(sig11.eig$val)) %*% 
	t(sig11.eig$vec)
	
sig11
sig11.5 %*% sig11.5
	
sig22.eig <- eigen(sig22)
sig22.5 <- sig22.eig$vec %*% diag(sqrt(sig22.eig$val)) %*% 
	t(sig22.eig$vec)
	
sig22
sig22.5 %*% sig22.5


# Calculate matrices A1 and A2

A1 <- solve(sig11.5) %*% sig12 %*% solve(sig22) %*% t(sig12) %*% solve(sig11.5)
A2 <- solve(sig22.5) %*% t(sig12) %*% solve(sig11) %*% sig12 %*% solve(sig22.5)

A1.eig <- eigen(A1)
A2.eig <- eigen(A2)

# First canonical variates loadings:

e1 <- A1.eig$vec[,1]
f1 <- A2.eig$vec[,1]

a1 <- e1 %*% solve(sig11.5)
b1 <- f1 %*% solve(sig22.5)

a1
b1

# First canonical variables/scores

u1 <- x1 %*% t(a1)
v1 <- x2 %*% t(b1)

# First canonical correlation

sqrt(A1.eig$val[1])
sqrt(A2.eig$val[1])
cor(u1, v1)

# Second canonical variates: loadings

e2 <- A1.eig$vec[,2]
f2 <- A2.eig$vec[,2]

a2 <- e2 %*% solve(sig11.5)
b2 <- f2 %*% solve(sig22.5)

a2
b2

# Second canonical variables/scores

u2 <- x1 %*% t(a2)
v2 <- x2 %*% t(b2)

# Second canonical correlation

sqrt(A1.eig$val[2])
sqrt(A2.eig$val[2])
cor(u2, v2)

a2 <- -a2
a2
u2 <- x1 %*% t(a2)
cor(u2, v2)

###########
# Step 4  #
###########

# R function to perform CCA

nyse.cc <- cc(nyse[,1:2], nyse[,3:5])

names(nyse.cc)

# Canonical variate loadings

nyse.cc$xcoef

a1
a2

nyse.cc$ycoef

b1
b2

# Canonical variate scores 

plot(nyse.cc$scores$xscores[,1], nyse.cc$scores$yscores[,1], xlab="First Canonical Variate U1 Scores", ylab="First Canonical Variate V1 Scores")

plot(nyse.cc$scores$xscores[,2], nyse.cc$scores$yscores[,2], xlab="Second Canonical Variate U2 Scores", ylab="Second Canonical Variate V2 Scores")

# Note that the cc results center the scores!!!

u1[1:10]
nyse.cc$scores$xscores[1:10,1]

u1.sc <- u1 - mean(u1)
u1.sc[1:10]

nyse.cc$scores$yscores[1:10,1]
v1.sc <- v1 - mean(v1)
v1.sc[1:10]

###########
# Step 5  #
###########

# PCA

# Use the seed data for PCA exercises

exam.cov <- cov(exam)
exam.cor <- cor(exam)

# Example linear combination:
aVec1 <- c(2, 2, 1, 1, 1, 1, 1, 1)

exam.linComb1 <- as.matrix(exam) %*% aVec1
exam.linComb1[1:10,]
var(exam.linComb1)

t(aVec1) %*% exam.cov %*% aVec1


aVec2 <- 10*aVec1

exam.linComb2 <- as.matrix(exam) %*% aVec2
exam.linComb2[1:10,]
var(exam.linComb2)

aVec3 <- 1:8
exam.linComb3 <- as.matrix(exam) %*% aVec3

cov(exam.linComb1, exam.linComb3)
t(aVec1) %*% exam.cov %*% aVec3
t(aVec3) %*% exam.cov %*% aVec1

# Eigendecompositions of Cov and Cor matrices

exam.cov.eig <- eigen(exam.cov)
exam.cor.eig <- eigen(exam.cor)

exam.cov.eig

exam.cor.eig

# Loadings plots with unstandardized data

par(mfrow=c(3,3), oma=c(0,0,2,0))
for(i in 1:8){
	plot(1:8, exam.cov.eig$vec[,i], xlab="Variable", ylab="Loading", main=paste("Principal Component ", i, sep=""), ylim=c(-1, 1))
	abline(h=0)
}
mtext("Raw Principal Components", outer=T)

# Loadings plots with standardized data

par(mfrow=c(3,3), oma=c(0,0,2,0))
for(i in 1:8){
	plot(1:8, exam.cor.eig$vec[,i], xlab="Variable", ylab="Loading", main=paste("Principal Component ", i, sep=""), ylim=c(-1, 1))
	abline(h=0)
}
mtext("Standardized Principal Components", outer=T)

# Scores plots for unstandardized data

par(mfrow=c(1,1), oma=c(0,0,0,0))

exam.pcscores.raw12 <- as.matrix(exam) %*% exam.cov.eig$vec[,1:2]
plot(exam.pcscores.raw12, xlab="First Principal Component Scores", ylab="Second Principal Component Scores", main="(Raw) Score Plot")

exam.pcscores.cent12 <- scale(exam, center=T, scale=F) %*% exam.cov.eig$vec[,1:2]
plot(exam.pcscores.cent12, xlab="First Principal Component Scores", ylab="Second Principal Component Scores", main="(Centered) Score Plot")

# Scree plot for unstandardized data

par(mfrow=c(1,1), oma=c(0,0,0,0))
plot(1:8, exam.cov.eig$val, type="b", xlab="Component", ylab="Variance", main="Scree Plot for NYSE Data")

# Cumulative variance explained plot for unstandardized data

exam.cov.eig$val
cumsum(exam.cov.eig$val)
cumsum(exam.cov.eig$val)/sum(exam.cov.eig$val)

plot(1:8, cumsum(exam.cov.eig$val)/sum(exam.cov.eig$val), type="b", xlab="# Components", ylab="Cumulative Variance Explained")

###########
# Step 6  #
###########

# prcomp function

help(prcomp)

exam.pc1 <- prcomp(exam)

names(exam.pc1)

# The 'sdev' component is the square roots of the variances of
# the principal components.

exam.pc1$sdev
sqrt(exam.cov.eig$val)

exam.pc1$rotation
exam.cov.eig$vec

# Note that some vectors may have signs flipped

exam.pc1$x[1:8,1:2]
exam.pcscores.cent12[1:8,]

# Scores will also be flipped if principal components are flipped

exam.pc1.sc <- prcomp(exam, scale=T)

exam.pc1.sc$sdev
sqrt(exam.cor.eig$val)

exam.pc1.sc$rotation
exam.cor.eig$vec

# Note that some vectors may have signs flipped

