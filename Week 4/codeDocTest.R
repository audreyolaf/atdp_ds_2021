library(ggplot2)
library(dplyr)
library(randomForest)
library(party)

View(balloons)
str(balloons)

# balloons decision tree
balloons$InflatedF = factor(balloons$Inflated)
balloons = select(balloons, -Inflated)
str(balloons)
tree = ctree(balloons$Inflated~., data = balloons)
plot(tree)
test = predict(tree, newdata = balloons)
View(test)
table(predict(tree), balloons$Inflated)

# balloons random forest
balloons$Color = as.factor(balloons$Color)
balloons$Size = as.factor(balloons$Size)
balloons$Act = as.factor(balloons$Act)
balloons$Age = as.factor(balloons$Age)
balloons$Inflated = as.factor(balloons$Inflated)

set.seed(10)

random.forest = randomForest(Inflated ~., data = balloons, proximity = TRUE)
random.forest

View(random.forest$err.rate)

error.rate = data.frame(numTrees = rep(1:100, times = 3, Error = random.forest$err.rate[,"OOB"]))
View(error.rate)
ggplot(error.rate, aes(numTrees, Error)) + geom_line()


# iris svm demo
data = (iris)
View(data)
library(ggplot2)
install.packages(e1071)
library(e1071)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

model = svm(Species~., data = data)
summary(model)
plot(model, data = iris, 
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))
prediction = predict(model, iris)
table(Predicted = prediction, Actual = iris$Species)


# anthrokids svm
data = (OA.9.27...anthrokids)
View(data)
library(ggplot2)
library(e1071)
ggplot(OA.9.27...anthrokids, aes(Height, Age, color = Race)) + geom_point()
OA.9.27...anthrokids$Sex = factor(OA.9.27...anthrokids$Sex, labels = c("Male", "Female"))
OA.9.27...anthrokids$Race = factor(OA.9.27...anthrokids$Race, labels = c("White", "Other"))
anthrokids <- svm(Race ~., data = OA.9.27...anthrokids)
table(Actual = OA.9.27...anthrokids$Race, Predicted = predict(anthrokids))
plot(anthrokids, OA.9.27...anthrokids, Age ~ Sex)

# model = svm(Race~., data = OA.9.27...anthrokids)
# summary(model)
# plot(model, data = OA.9.27...anthrokids, type=Classification,
    # Height~Weight,
   #  slice = list(Weight = 3, Age = 4))
prediction = predict(model, OA.9.27...anthrokids)
table(Predicted = prediction, Actual = OA.9.27...anthrokids$Race)
