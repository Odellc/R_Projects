#Titanic Survival Prediction

library(ggplot2)
library(stringr)

train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)


#add a "survived" variable to the test set to allow for combing data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

#combine data sets
data.combined <- rbind(train, test.survived)

#colnames(test.survived)
#colnames(train)

# A bit about r data types (e.g. factors)
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)


#Take a look at the gross survival rates
table(data.combined$Survived)

#Take a look of distribution of classes
table(data.combined$Pclass)

train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x=Pclass, fill = factor(Survived))) +
  geom_bar(width=0.5)+
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill="Survived")

#Exame the first few names in the training set
head(as.character(train$Name))

#How many unique names are there across both train and test?
length(unique(as.character(data.combined$Name)))

#Two duplicate names, look closer
#First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

# Next, take a look at the recorders in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]

#Any correlation with other variables (e.g. sibsp?)
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

#Hypothesis - Name titles correlates with age
mres <- data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mres[1:5,]

#Check out males to see if pattern continues
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]


#Expand upon relationships between 'Survived' and 'Pclass' by adding the new 'Title' variable to the 
#data set and then explore a potential 3-dimensional relationship.

extractTitle <-  function(name){
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) >0) {
    return("Miss.")
  } else if (length(grep("Master.", name))>0){
    return("Master.")
  } else if (length(grep("Mrs.", name)) >0){
    return("Mrs.")
  } else if (length(grep("Mr.", name)) >0){
    return("Mr.")
  } else{
    return ("Other")
  }
}
titles <- NULL
for ( i in 1:nrow(data.combined)){
    titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$Title <- as.factor(titles)

ggplot(data.combined[1:891,], aes(x=Title, fill=Survived)) +
  geom_bar(binwidth = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill = "Survived")


#What's the distribution of females to males across train & test
table(data.combined$Sex)
