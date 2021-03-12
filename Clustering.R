library(tidyverse)
library(ggplot2)
library(caret)
library(tree)
library(ISLR)
#install.packages("corrplot")
library(corrplot)
library(rpart)
library( randomForest)


#---- Load original train data ----#
data <- read.csv("train.csv", header=T, quote="\"")

# Convert to Tibble
data <- as_tibble(data)

## subset data: trainning and test set
set.seed(123)
train_size <- 250000
train_ind <- sample(seq_len(nrow(data)), size = train_size)
train <- data[train_ind, ] # Train

test_size <- 50000
data_tem <- data[-train_ind, ]
test_ind <- sample(seq_len(nrow(data_tem)),size = test_size) 
test <- data_tem[test_ind, ] # Test

all_id <- as.data.frame(table(c(train$id, test$id)))
all_id[all_id$Freq > 1,] # make sure no overlappind records

#----  Finding features from the training dataset ----#



#---- Build a classifier use new train set and validate on dev set ----#

#separate out a part of your training dataset as validation set
dev_size <- 50000
set.seed(123)
dev_ind <- sample(seq_len(nrow(train)),size = dev_size) 
dev <- train[dev_ind, ] # dev

train_new <- train[-dev_ind , ] # new training

#all_id <- as.data.frame(table(c(dev$id, test$id,train_new$id)))
#all_id[all_id$Freq > 1,] 
#train_id <- as.data.frame(table(c(train$id,train_new$id)))
#nrow(train_id[train_id$Freq > 1,] )
#train_id <- as.data.frame(table(c(train$id,dev$id)))
#nrow(train_id[train_id$Freq > 1,] )
#train_id <- as.data.frame(table(c(train_new$id,dev$id)))
#nrow(train_id[train_id$Freq > 1,] )



# Method: Hierarchical Clustering

# install.packages('e1071') # for calculate hamming distance
#library(e1071)
library(cluster)
library(MASS)


name <- "id"
train_new <- dplyr::select(train_new,-one_of(name))


# Important variables selected from XGB
val_imp <- c("ps_car_13","ps_reg_03","ps_ind_05_cat","ps_car_07_cat",
             "ps_ind_03","ps_car_14","ps_calc_14","ps_ind_17_bin",
             "ps_ind_16_bin","ps_car_09_cat","ps_ind_02_cat","ps_car_11_cat",
             "ps_calc_03","ps_car_11_cat","ps_ind_05_cat","ps_calc_07",
             "ps_calc_04","ps_car_11_cat","ps_car_06_cat","ps_car_04_cat")
length(unique(val_imp))

# keep only quantative variables 
train_newImp <- dplyr::select(train_new, c('target',val_imp))

# missing values
for (i in 2:18){
  train_newImp <-train_newImp[(train_newImp[,i]!=-1),]
}

cat <- colnames(dplyr::select(train_newImp,ends_with('cat')))
bin <- colnames(dplyr::select(train_newImp,ends_with('bin')))
catbin <- c(cat,bin)
train_newNum <- dplyr::select(train_newImp, -catbin)
# issue: due to limited data description 
# can not further seperate ordinal varaibles from quantative

#distances_num<- dist(train_newNum[-1])
# Error: cannot allocate vector of size 82.8 Gb

# Due to error, further sampling 
size <- 10000 # tried 50000, 40000,30000.. cannot work
set.seed(123)
ind <- sample(seq_len(nrow(train_newNum)),size = size) 
train_newNum_n <- train_newNum[ind, ]
distances_num<- dist(train_newNum_n[-1])


# hc_num <- hclust(distances_num, method = 'single')
hc_num = hclust(distances_num, method = 'complete') # best
# hc_num = hclust(distances_num, method = 'average')
# hc_num = hclust(distances_num, method = 'centroid')

hc_numClusters = cutree(hc_num , k = 2)
table(factor(hc_numClusters), factor(train_newNum_n$target))

### catbin
train_newCB <- dplyr::select(train_newImp, catbin)
train_newCB_n <- train_newCB[ind, ]

# https://stackoverflow.com/questions/8997198/calculating-hamming-distance-for-two-vectors-in-r
# matrix_hamm <- hamming.distance(as.matrix(train_newCB_n[-1])) 
# cannot work for this case

n <- nrow(train_newCB_n)
m <- matrix(nrow=n, ncol=n)
for(i in seq_len(n - 1))
  for(j in seq(i, n))
    m[j, i] <- m[i, j] <- sum(train_newCB_n[i,] != train_newCB_n[j,]) # too slow...


#Factoring
cor.x <- cor(train_newImp[,1:18])
fa.pc <- principal(cor.x, nfactors=2)

fa.cm <- fa(cor.x, nfactors=2, rotate="varimax", scores="regression", fm="ml")

load <- fa.pc$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(train_newImp),cex=.7)
