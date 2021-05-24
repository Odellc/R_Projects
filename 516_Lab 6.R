
#Creates same data set as the lab to work from
set.seed(1964)

#assigns a vector of random numbers for both shoe types

shoeA <- rnorm(25, mean = 50, sd = 20)
shoeB <- rnorm(30, mean = 60, sd = 15)

#We want to know if the shoes have the same average consumer rating.

#Conduct a two sided t-test, since the samples are not paired

t.test(shoeA,shoeB, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = .95)

#Create data for a paired test

#we use set 1809, that is when Gauss helped establish the normal distribution
set.seed(1809)

#Since we have a paired data, we need to create distinct means for the pairs
means <- rnorm(32, 50, 25)

#We want to make the means the same for both wife and husband
wife <- rnorm(32, mean = means, sd = 15)

husband <- rnorm(32, mean = means, sd = 15)

#We want to create a paired t-test this time. 

t.test(wife, husband, alternative = "two.sided", mu = 0, paired = TRUE, var.equal = FALSE)

#I am noticing that we are reversing the signs in the summary for the confidence intervals.
#This is from us comparing the second to the first, and having to change their orders.
