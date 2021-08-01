library(ggplot2)
library(randomForest)
library(dplyr)
data <- read.csv("C:/Users/Marcus Hong/Downloads/heart.csv")
View(data)
str(data)

#we need to change sex to M/F
#We need to change cp (chest pain), thal, ca, thal and target to factors

#sets asll "?" data to NA
data[data == "?"] <- NA

#converts sex to M/F in 2 ways
data[data$sex == 0,]$sex <- "F"
data = mutate(data, sex = replace(sex, sex == 1, "M"))
data$sex <- as.factor(data$sex)

#converts a bunch of columns to factors
data$cp = as.factor(data$cp)
data$fbs = as.factor(data$fbs)
data$restecg = as.factor(data$restecg)
data$exang = as.factor(data$exang)
data$slope = as.factor(as.factor)
data$ca = as.factor(data$ca)
data$thal = as.factor(data$thal)
data$target = as.factor(data$target)
#set the RNG, so that our work is repeatable
set.seed(42)


#runs the model
random.forest = randomForest(target ~., data = data, proximity=TRUE)

#The following comand will show us the output of our random forest
#Type of Random Forest: Will state either regression, classification or unsupervised depending on what we used as inputs
#Number of trees: is the number of trees in the model
#Varialbes tried at each split: is the number of variables used to make each tree
#OOB error: Basically the error rate, so our model was incorrect 16.5% of the time
#Confusion matrix: the correct/incorrect classification

random.forest

#lets now try and figure out how many trees in a forest is "optimal"
#the value err.rate in our random forest, tells us the error rate generated when we create each tree!
#let us take all of the OOB (general error rate), and the corrisponding number of trees and put it into a data frame
View(random.forest$err.rate)


error.rate = data.frame(numTrees = rep(1:500, times = 3), Error = random.forest$err.rate[,"OOB"])
View(error.rate)
ggplot(error.rate, aes(numTrees, Error)) + geom_line()


#As we can see, as the num trees increase the error rate decreases, but it stabilizes after 300 trees, 
#lets run it again with 300 trees
random.forest.300 = randomForest(target ~., data = data, proximity=TRUE, ntree =300)
random.forest.300
random.forest
#random.forest.300 might not be more accurate because of the random nature of randomForests

#now lets find the optimal number of variables to try
#this code will run a for loop for values 1-10
#then, it will run the randomForest model for each of the values between 1 and 10
#finally, it will store the error rate of all 300 trees into the vector error.values
error.values = list(length = 10)
for(i in 1:10){
  model = randomForest(target ~., data = data, proximity=TRUE, mtry = i)
  error.values[i] = model$err.rate[nrow(model$err.rate),1]
}
error.values
#it looks like the default value of 3 is optimal, now let us construct our optimal forest
random.forest.optimal = randomForest(target ~., data = data, proximity=TRUE, ntree =300, mtry = 3)
random.forest.optimal
random.forest
