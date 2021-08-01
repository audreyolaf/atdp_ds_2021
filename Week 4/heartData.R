# heart decision tree
library(party)

str(heart)

# heart trees
cat_heart$target = factor(cat_heart$target)
heart = select(cat_heart, -target)
str(cat_heart)

heart.tree = ctree(cat_heart$target ~ ., data = cat_heart)
plot(heart.tree)
heart.test = predict(heart.tree, newdata = cat_heart)
View(heart.test)
table(predict(heart.tree), cat_heart$target)

# heart forests
library(randomForest)
library(ggplot2)
cat_heart <- na.omit(cat_heart)
cat_heart$sex = as.factor(cat_heart$sex)
cat_heart$cp = as.factor(cat_heart$cp)
cat_heart$fbs = as.factor(cat_heart$fbs)
cat_heart$restecg = as.factor(cat_heart$restecg)
cat_heart$exang = as.factor(cat_heart$exang)

set.seed(10)

heart.forest = randomForest(target ~., data = cat_heart, proximity = TRUE)
heart.forest

View(heart.forest$err.rate)

heart.error = data.frame(numTrees = rep(1:500, times = 3), Error = heart.forest$err.rate[,"OOB"])
View(heart.error)
ggplot(heart.error, aes(numTrees, Error)) + geom_line()

# heart svm
library(ggplot2)
library(e1071)
ggplot(cat_heart, aes(thalach, chol, color = target)) + geom_point()

str(cat_heart)
cat_heart$cp = factor(cat_heart$cp)
cat_heart$thalach = factor(cat_heart$thalach)
hearts <- svm(target ~., data = cat_heart)
table(Actual = heart$target, Predicted = predict(hearts))

heart.model = svm(target ~., data = cat_heart)
summary(heart.model)
plot(heart.model, data = cat_heart, cp~trestbps, 
     slice=list(age=30, sex=1, chol=0, fbs=0, restecg=0, thalach=0, exang=0, oldpeak=9, slope=0, ca=0, thal=23))
heart.prediction = predict(heart.model, cat_heart)
plot(heart.model, data = cat_heart, chol~thalach)
heart.table = table(Predicted = heart.prediction, Actual = cat_heart$target)
View(heart.table)
