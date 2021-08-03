library(tidyverse)
library(randomForest)
library(party)
library(e1071)
library(class)

## FUNCTIONS
# Removes spaces in column names. Useful for random foresets - randomForest()
#  breaks if you try to use columns with spaces.
remove_col_spaces <- function(.data) {
  return(rename_with(.data, ~ gsub(" ", ".", .x)))
}

# Visualizes models for regression. You can only use a confusion matrix for
#  classification, this is a good alternative.
# The `correct threshold` is how close the prediction needs to be to the actual 
#  value for it to be considered correct.
visualize_model <- function(model, actualData, modelName = "Model", correctThreshold = 0.5) {
  dataFrame = tibble(Actual = actualData, Predicted = predict(model))
  dataFrame$Correct = abs(dataFrame$Actual - dataFrame$Predicted) < correctThreshold
  accuracy = round((count(filter(dataFrame, Correct == T)) / count(dataFrame))[[1]] * 100)
  p <- ggplot(data = dataFrame, aes(x = Actual, y = Predicted)) + geom_point(aes(color = Correct)) + geom_abline(slope = 1, intercept = 0) + ggtitle(paste(modelName, "Accuracy"), subtitle = paste(accuracy, "% of countries were predicted within ", correctThreshold, " of their happiness score.", sep = "")) #+ geom_smooth(method = "lm")
  return(p)
} 


## IMPORTING DATA

# A list containing all the data frames. Data$Y2015 accesses the 2015 data frame, etc.
Data = list()
for (file in 2015:2020) {
  # The filenames all have the same format: 2015.csv, etc...
  # Iterate through 2015-2020 to import all of them.
  name = paste("Y", file, sep = "") # Add 'Y' to the front of the name, you can't
                                    #  have columns that are purely numbers.
  
  # Sets that year's data as new data frame in the list.
  Data[[name]] <- as_tibble(read_csv(paste("Data/Happiness/", file, ".csv", sep = "")))
  
  # The Region column should be a factor, because there is a limited number of them.
  Data[[name]]$Region <- as.factor(Data[[name]]$Region)
}
remove(file, name) # Remove variables used in file importing that we don't need anymore.


# Sets the theme for all ggplots.
theme_set(theme_light())


### DATA CLEANING

#attach(Data)

# Select only the columns we want to use. - The 2020 data has a lot of other columns.
Data$Y2020 = Data$Y2020 %>% select(c( "Country", "Region", "Score", "Logged GDP", "Social Support", "Health (Life Expectancy)", "Freedom", "Generosity", "Corruption"))


# Data$Y2019$`Happy_Level` = cut(Data$Y2019$`Overall rank`,
#                                breaks=c(0,25,102,140,Inf),
#                                include.lowest=TRUE,
#                                labels=c("Ecstatic","Happy","Neutral","Sad"))

#dat = select(Data$Y2019, -c("Overall rank", "Country or region"))


## EXPLORATORY ANALYSIS
# Get a feel for the data.
hist(Data$Y2020$Score, breaks = seq(2,8, 0.2), xlab = "Score", main = "Happiness Score Distribution")
#ggplot(Data$Y2020, aes(x=Score)) + geom_histogram(color = "darkblue", fill = "lightblue", bins = 30)
ggplot(Data$Y2020, aes(Region, Score)) + geom_boxplot(aes(color = Region)) + ggtitle("Happiness by Region") + theme(axis.text.x=element_blank())


## ANALYSIS
# Random forest doesn't like spaces or parenthesis, so we remove them!
dat = Data$Y2020 %>% rename("Health" = "Health (Life Expectancy)") %>% remove_col_spaces()


rf = randomForest(Score ~ ., data = dat, proximity = TRUE)
visualize_model(rf, dat$Score, "Random Forest")

dat = select(Data$Y2020, -Country) # We don't want the models acciententally try
                                   #  to use a country's name to predict their 
                                   #  happiness :)

sv = svm(Score ~ ., data = dat)
visualize_model(sv, dat$Score, "SVM")

tree = ctree(Score ~ ., data = dat)
visualize_model(tree, dat$Score, "Decision Tree")

# knn
total.rows = nrow(dat)
training.i = sample.int(total.rows, total.rows * 0.8)

training = slice(dat, training.i)
test = slice(dat, -training.i)

knnModel = knn(training, test, training$Score, k = 3)
dataFrame = tibble(Actual = test$Score, Predicted = as.integer(knnModel))
dataFrame$Correct = abs(dataFrame$Actual - dataFrame$Predicted) < correctThreshold
accuracy = round((count(filter(dataFrame, Correct == T)) / count(dataFrame))[[1]] * 100)
ggplot(data = dataFrame, aes(x = Actual, y = Predicted)) + geom_point(aes(color = Correct)) + geom_abline(slope = 1, intercept = 0) + ggtitle("kNN Accuracy", subtitle = paste(accuracy, "% of countries were predicted within ", correctThreshold, " of their happiness score.", sep = "")) #+ geom_smooth(method = "lm")

visualize_model(knnModel, test$Score, "kNN")


tree = ctree(Happy_Level ~ ., data = dat)
happy.svm = svm(Happy_Level ~ ., data = dat)



plot(tree)
table(Actual = dat$Happy_Level, Predicted = predict(tree))

summary(happy.svm)

# Little new section
dat = select(Data$Y2019, -c("Overall rank", "Country or region", "Happy_Level"))
dat = rename(dat, GDP = `GDP per capita`, Social_support = `Social support`, life_expectancy = `Healthy life expectancy`, freedom = `Freedom to make life choices`, corruption = `Perceptions of corruption`)

sliceData = select(dat, -c("Score", "life_expectancy","Social_support"))
sl = list()

for (n in 1:ncol(sliceData)) {
  col = sliceData[,n]
  name = names(col)[1]
  sl[[name]] = mean(col[[1]])
}

happy.svm = svm(Score ~ ., data = dat)
plot(happy.svm, data = dat, life_expectancy ~ Social_support, slice = sl)



## Linear regression between each attribute and the happiness score

# A list with each attribute and the R-squared when it is plotted in relation
#  to the happiness score.
fit = list()

# Can't run regression on text!
dat = select(Data$Y2020, -c("Country"))

# For each attribute (column), find the R-squared of that + the score.
for (column in variable.names(dat)) {
  d = select(dat, c(column,"Score"))
  df = tibble(x = d[[column]], Score = d$Score)
  regression = lm(Score ~ ., data = d)
  rsq = summary(regression)$r.squared
  
  # Plot
  p <- ggplot(df, aes(x, Score)) + geom_point() + geom_smooth(method = "lm", aes(group=1)) + xlab(column) + ggtitle(paste(column, "vs", "Score"), subtitle = paste("R-squared: ", rsq, ".", sep = ""))
  plot(p)

  fit[[column]] = rsq
}

# Make a nice table.
fit.table = tibble(Attribute = names(fit), `Linear R-Squared` = unlist(fit, use.names = F))
fit.table = arrange(fit.table, desc(`Linear R-Squared`))
print(fit.table)