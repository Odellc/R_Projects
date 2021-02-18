library(tidyverse)
library(ggplot2)

#---- Load original train data ----#
data <- read.csv("train.csv", header=T, quote="\"")

# Convert to Tibble
data <- as_tibble(data)

## subset data: trainning and test set
train_size <- 250000
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = train_size)
train <- data[train_ind, ] # Train

test_size <- 50000
data_tem <- data[-train_ind, ]
set.seed(123)
test_ind <- sample(seq_len(nrow(data_tem)),size = test_size) 
test <- data_tem[test_ind, ] # Test

all_id <- as.data.frame(table(c(train$id, test$id)))
all_id[all_id$Freq > 1,] # make sure no overlappind records

train_fix <- train

#----  Finding features from the training dataset ----#

# Method: logistic 
# Factoring catigorical and binary features
lapply(train, class)
cat <- colnames(dplyr::select(train,ends_with('cat')))
bin <- colnames(dplyr::select(train,ends_with('bin')))
catbin <- c(cat,bin)
train[,catbin] <- lapply(train[,catbin], factor)

# drop id
name <- "id"
train <- dplyr::select(train,-one_of(name))

# issue: highly unbalanced target
ggplot(data=train, aes(factor(target))) + geom_histogram(stat = 'count')

# solution 1: subset train set to balance the positive/negtive classes.

set.seed(123)
train_balance <- train %>%
  group_by(target) %>%
  sample_n(5000)
ggplot(data=train_balance, aes(factor(target))) + geom_histogram(stat = 'count')


# Stepwise Logistic Regression
# http://www.utstat.toronto.edu/~brunner/oldclass/appliedf11/handouts/2101f11StepwiseLogisticR.pdf

#fullmod <- glm(factor(target) ~ .,family=binomial, data=train_balance)
fullmod <- glm(target ~ .,family=binomial, data=train_balance)
#nullmod <- glm(factor(target) ~ 1,family=binomial, data=train_balance)

mod <- step(fullmod, direction = "backward",trace=0) 

# solution 2: subset a small size of samples
# train_small <- sample(nrow(train),10000)
# fulmod_2 <- glm(target ~ .,family=binomial, data=train_balance)
# mod_s <- step(fulmod_2, direction = "backward",trace=0) 


#---- Build a classifier using new train set and to validate on dev set ----#

#separate out a part of your training dataset as validation set
dev_size <- 50000
set.seed(123)
dev_ind <- sample(seq_len(nrow(train_fix)),size = dev_size) 
dev <- train_fix[dev_ind, ] # dev/validate set
dev_fix <- dev
train_new <- train_fix[-dev_ind , ] # new training
train_new_fix <- train_new

name <- "id"
train_new <- dplyr::select(train_new,-one_of(name))
dev <- dplyr::select(dev,-one_of(name))

train_new[,catbin] <- lapply(train_new[,catbin], factor)
dev[,catbin] <- lapply(dev[,catbin], factor)

set.seed(123)
train_balance_new <- train_new %>%
  group_by(target) %>%
  sample_n(5000)
ggplot(data=train_balance_new, aes(factor(target))) + geom_histogram(stat = 'count')

# mod_fit <- glm(mod$formula,family=binomial, data=train_balance_new)
mod_fit <- glm(mod$formula,family=binomial, data=train_balance_new)

# predict 
#res <- predict(mod_fit, newdata = dev,type = 'response')
# find thredhold
for (i in 1:9){
  res <- predict(mod_fit, newdata = dev,type = 'response')
  res[res > (i/10)] = 1
  res[res <= (i/10)] = 0
  print(i/10)
  print(table(res, dev$target))}

for (j in 41:69){
  res <- predict(mod_fit, newdata = dev,type = 'response')
  res[res > (j/100)] = 1
  res[res <= (j/100)] = 0
  print(j/100)
  print(table(res, dev$target))}


