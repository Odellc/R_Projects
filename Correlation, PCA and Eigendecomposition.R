# ST 558: Multivariate Analytics
# Module 7 R Activity 
#Odell, Christopher


###########
# Question 1  #
###########

#setwd("C:/Users/codel/Documents/Masters Program/ST_558_400/week 7")


seeds <- read.csv('Seeds3Data.csv')
head(seeds) # First 6 rows of the data


###########
# Question 2  #
###########

#Create the pairs plot to look for distribution
pairs(seeds)


#Sample correlation matrix and eigendecomposition

seeds.cor <-  cor(seeds)
seeds.eig <-  eigen(seeds.cor)

###########
# Question 3  #
###########

#Principal components factor analysis, through diagional sqroots
load.pcfa <- seeds.eig$vectors %*% diag(sqrt(seeds.eig$values))

load.pcfa


###########
# Question 5  #
###########

#Three factor model, the loading matrix L 
m <- 3

unique.multi.pcfa <- diag(seeds.cor - load.pcfa[,1:m] %*% t(load.pcfa[,1:m]))
unique.multi.pcfa

#Compute the fitted model
fit.pcfa <- load.pcfa[,1:m] %*% t(load.pcfa[,1:m]) + diag(unique.multi.pcfa)
fit.pcfa


#Calculate the residual plot

res.pcfa <- round(seeds.cor - fit.pcfa,4)
res.pcfa


###########
# Question 6  #
###########

# Fit a factoral model without rotaiton or regression by observation 
seeds.mlfa <-  factanal(x = seeds, factors = 3, rotation = "none")

seeds.mlfa

###########
# Question 8  #
###########

seeds.mlfa$loadings

seeds.mlfa$loadings[,1:3]

fit.mle <-  seeds.mlfa$loadings %*% t(seeds.mlfa$loadings) + diag(seeds.mlfa$uniquenesses)

fit.mle

###########
# Question 9  #
###########

#Calculate the performance vs the residuals
res.mle <-  round(seeds.cor - fit.mle,4)
res.mle


###########
# Question 11  #
###########

# Fit a factoral model with rotation
seeds.mlfa2 <-  factanal(x = seeds, factors = 3, rotation = "varimax")

seeds.mlfa2

#get the loadings for the 3 factors
round(seeds.mlfa2$loadings[,1:3],4)

apply(seeds.mlfa$load[,1:2]^2, 2, var)
apply(seeds.mlfa2$load[,1:2]^2, 2, var)


###########
# Question 12  #
###########

# fit model with rotation and scores per observation as regression
seeds.mlfa3 <-  factanal(x = seeds, factors = 3, rotation = "varimax", scores = "regression")

seeds.mlfa3


seeds.mlfa3$scores[10,]

scale(seeds, center=T, scale=F)[10,]
