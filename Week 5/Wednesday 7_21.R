library(tidyverse)
library(randomForest)
library(party)
library(e1071)
library(class)

Data = list()
for (file in 2015:2020) {
  name = paste("Y", file, sep = "")
  Data[[name]] <- as_tibble(read_csv(paste("Data/Happiness/", file, ".csv", sep = "")))
  #Data[[name]]$Region <- as.factor(Data[[name]]$Region)
}

attach(Data)

### DATA CLEANING

Data$Y2019$`Happy_Level` = cut(Data$Y2019$`Overall rank`,
                               breaks=c(0,25,102,140,Inf),
                               include.lowest=TRUE,
                               labels=c("Ecstatic","Happy","Neutral","Sad"))

nineteenHap  = select(Data$Y2019, -c("Overall rank", "Country or region"))
nineteenHap  = rename(nineteenHap, GDP = `GDP per capita`, 
                      Social_support = `Social support`, 
                      life_expectancy = `Healthy life expectancy`, 
                      freedom = `Freedom to make life choices`, 
                      corruption = `Perceptions of corruption`)



# to determine how accurate random forest regression model is
correctThreshold = 0.5

# run random forest algorithm
nineteenForest = randomForest(Score ~ ., data = nineteenHap, proximity = TRUE)
dataFrame = tibble(Actual = nineteenHap$Score, Predicted = predict(nineteenForest))
dataFrame$Correct = abs(dataFrame$Actual - dataFrame$Predicted) < correctThreshold
accuracy = round((count(filter(dataFrame, Correct == T)) / count(dataFrame))[[1]] * 100)
ggplot(data = dataFrame, aes(x = Actual, y = Predicted)) + geom_point(aes(color = Correct)) + geom_abline(slope = 1, intercept = 0) + ggtitle("Random Forest Accuracy", subtitle = paste(accuracy, "% of countries were predicted within ", correctThreshold, " of their happiness score.", sep = "")) #+ geom_smooth(method = "lm")

# run svm algorithm
sv = svm(Score ~ ., data = nineteenHap)
dataFrame = tibble(Actual = nineteenHap$Score, Predicted = predict(sv))
dataFrame$Correct = abs(dataFrame$Actual - dataFrame$Predicted) < correctThreshold
accuracy = round((count(filter(dataFrame, Correct == T)) / count(dataFrame))[[1]] * 100)
ggplot(data = dataFrame, aes(x = Actual, y = Predicted)) + geom_point(aes(color = Correct)) + geom_abline(slope = 1, intercept = 0) + ggtitle("SVM Accuracy", subtitle = paste(accuracy, "% of countries were predicted within ", correctThreshold, " of their happiness score.", sep = "")) #+ geom_smooth(method = "lm")

tree = ctree(Score ~ ., data = nineteenHap)
dataFrame = tibble(Actual = nineteenHap$Score, Predicted = predict(tree))
dataFrame$Correct = abs(dataFrame$Actual - dataFrame$Predicted) < correctThreshold
accuracy = round((count(filter(dataFrame, Correct == T)) / count(dataFrame))[[1]] * 100)
ggplot(data = dataFrame, aes(x = Actual, y = Predicted)) + geom_point(aes(color = Correct)) + geom_abline(slope = 1, intercept = 0) + ggtitle("Decision Tree Accuracy", subtitle = paste(accuracy, "% of countries were predicted within ", correctThreshold, " of their happiness score.", sep = "")) #+ geom_smooth(method = "lm")

total.rows = nrow(nineteenHap)
training.i = sample.int(total.rows, total.rows * 0.8)

training = slice(nineteenHap, training.i)
test = slice(nineteenHap, -training.i)

knnModel = knn(training, test, training$Score, k = 5)
dataFrame = tibble(Actual = test$Score, Predicted = as.integer(knnModel))
dataFrame$Correct = abs(dataFrame$Actual - dataFrame$Predicted) < correctThreshold
accuracy = round((count(filter(dataFrame, Correct == T)) / count(dataFrame))[[1]] * 100)
ggplot(data = dataFrame, aes(x = Actual, y = Predicted)) + geom_point(aes(color = Correct)) + geom_abline(slope = 1, intercept = 0) + ggtitle("kNN Accuracy", subtitle = paste(accuracy, "% of countries were predicted within ", correctThreshold, " of their happiness score.", sep = "")) #+ geom_smooth(method = "lm")

tree = ctree(Happy_Level ~ ., data = nineteenHap)
happy.svm = svm(Happy_Level ~ ., data = nineteenHap)
plot(tree)
table(Actual = nineteenHap$Happy_Level, Predicted = predict(tree))
summary(happy.svm)

# Little new section
nineteenHap = select(Data$Y2019, -c("Overall rank", "Country or region", "Happy_Level"))
nineteenHap = rename(nineteenHap, GDP = `GDP per capita`, Social_support = `Social support`, life_expectancy = `Healthy life expectancy`, freedom = `Freedom to make life choices`, corruption = `Perceptions of corruption`)

sliceData = select(nineteenHap, -c("Score", "life_expectancy","Social_support"))
sl = list()

for (n in 1:ncol(sliceData)) {
  col = sliceData[,n]
  name = names(col)[1]
  sl[[name]] = mean(col[[1]])
}

happy.svm = svm(Score ~ ., data = nineteenHap)
plot(happy.svm, data = nineteenHap, life_expectancy ~ Social_support, slice = sl)


fit = list()
nineteenHap = select(Data$Y2019, -c("Country or region"))
for (column in variable.names(nineteenHap)) {
  d = select(nineteenHap, c(column,"Score"))
  df = tibble(x = d[[column]], Score = d$Score)
  regression = lm(Score ~ ., data = d)
  #ggplot(df, aes(x, Score)) + geom_point() + geom_smooth(method = "lm") + xlab(column)
  plot(d)
  lines(regression$model)
  fit[[column]] = summary(regression)$r.squared
}

fit.table = tibble(Column = names(fit), `Linear R-Squared` = unlist(fit, use.names = F))


hist(Data$Y2019$Score, breaks = seq(2,8, 0.2), xlab = "Score")
