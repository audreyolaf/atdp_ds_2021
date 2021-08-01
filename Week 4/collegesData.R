# trees
library(party)
library(dplyr)
colleges = (ForbesAmericasTopColleges2019)
colleges$Rank = factor(colleges$Rank)
colleges = select(colleges, -Rank)
str(colleges)
colleges <- select(colleges, Rank, Undergraduate.Population, Student.Population, Net.Price, Average.Grant.Aid, Total.Annual.Cost, Alumni.Salary, Acceptance.Rate, SAT.Lower, SAT.Upper, ACT.Lower, ACT.Upper)
colleges.tree = ctree(colleges$Rank ~ ., data = colleges)
plot(colleges.tree)
colleges.test = predict(colleges.tree, newdata = colleges)
View(colleges.test)
table(predict(colleges.tree), colleges$Rank)

# colleges forest DOES NOT WORK
# ForbesAmericasTopColleges2019 <- select(Name, Public.Private, Undergraduate.Population, Student.Population, Net.Price, Average.Grant.Aid, Total.Annual.Cost, Alumni.Salary, Acceptance.Rate, SAT.Lower, SAT.Upper, ACT.Lower, ACT.Upper)
colleges <- na.omit(colleges)
colleges$Alumni.Salary = as.factor(colleges$Alumni.Salary)
colleges$Acceptance.Rate = as.factor(colleges$Acceptance.Rate)
colleges$SAT.Upper = as.factor(colleges$SAT.Upper)
# ForbesAmericasTopColleges2019$Public.Private = as.factor(ForbesAmericasTopColleges2019$Public.Private)

set.seed(300)
colleges.forest = randomForest(ACT.Upper~., data = ForbesAmericasTopColleges2019, proximity = TRUE)
colleges.forest

View(colleges.forest$err.rate)
colleges.error = data.frame(numTrees = rep(1:500, times = 3), Error = colleges.forest$err.rate[,"OOB"])
View(colleges.error)
ggplot(colleges.error, aes(numTrees, Error)) + geom_line()

# svm
data = (ForbesAmericasTopColleges2019)
ForbesAmericasTopColleges2019[]
ForbesAmericasTopColleges2019 <- na.omit(ForbesAmericasTopColleges2019)
View(data)
library(ggplot2)
library(e1071)
View(ForbesAmericasTopColleges2019)
ForbesAmericasTopColleges2019 = mutate(ForbesAmericasTopColleges2019, Rank = replace(Rank, Rank < 101, 1))
ForbesAmericasTopColleges2019 = mutate(ForbesAmericasTopColleges2019, Rank = replace(Rank, Rank > 100, 0))
ggplot(ForbesAmericasTopColleges2019, aes(Alumni.Salary, Acceptance.Rate, color = Rank)) + geom_point()
type()

# ForbesAmericasTopColleges2019$Rank = factor(ForbesAmericasTopColleges2019$Rank)
colleges.model = svm(Rank~Alumni.Salary+Acceptance.Rate, data = ForbesAmericasTopColleges2019, type="C-classification", kernel = "linear")
summary(colleges.model)
View(colleges.model)
plot(colleges.model, ForbesAmericasTopColleges2019, Alumni.Salary~Acceptance.Rate)
colleges.prediction = predict(colleges.model, ForbesAmericasTopColleges2019)
View(colleges.prediction)
colleges.prediction
table(Predicted = colleges.prediction, Actual = ForbesAmericasTopColleges2019$Rank)
View(colleges.table)

