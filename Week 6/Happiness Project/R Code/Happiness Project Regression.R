# Happiness Project

## IMPORTS
library(tidyverse) # Useful tools
library(randomForest)
library(party) # Decision Trees
library(e1071) # SVMs

set.seed(3601) # For randomness

## FUNCTIONS
# Removes spaces in column names. Useful for random forests
#  breaks if you try to use columns with spaces.
remove_col_spaces <- function(.data) {
  return(rename_with(.data, ~ gsub(" ", ".", .x)))
}

# Visualizes models for regression. You can only use a confusion matrix for
#  classification, this is a good alternative.
# The `correct threshold` is how close the prediction needs to be to the actual 
#  value for it to be considered correct.
visualize_model <- function(model, actualData, modelName = "Model", correctThreshold = 0.5) {
  comparison = tibble(Actual = actualData, Predicted = predict(model))
  comparison$Difference = comparison$Actual - comparison$Predicted
  comparison$Correct = abs(comparison$Difference) < correctThreshold
  accuracy = round(((count(filter(comparison, Correct == T)) / count(comparison))[[1]] * 100), 3)

  p <- ggplot(data = comparison, aes(x = Actual, y = Predicted)) + geom_point(aes(color = Correct)) + geom_abline(slope = 1, intercept = 0) + ggtitle(paste(modelName, "Accuracy"), subtitle = paste(accuracy, "% of countries were predicted within ", correctThreshold, " of their happiness score.", sep = "")) #+ geom_smooth(method = "lm")

    return(p)
} 


## IMPORTING DATA

# A list containing all the data frames. Data$Y2015 accesses the 2015 data frame, etc.
Data = list()
for (file in 2015:2020) {
  # The filenames all have the same format: 2015.csv, etc...
  # Iterate through 2015-2020 to import all of them.
  name = paste("Y", file, sep = "") # Add 'Y' to the front of the name, you can't
                                    #  have variable names that are purely numbers.
  
  # Sets that year's data as new data frame in the list.
  Data[[name]] <- as_tibble(read_csv(paste("Data/Happiness/", file, ".csv", sep = "")))
  
  # The Region column should be a factor, because there is a limited number of them.
  Data[[name]]$Region <- as.factor(Data[[name]]$Region)
  
  # Data[[name]]$`Happy Level` <- cut(Data[[name]]$Score,
  #                                   breaks=c(0,4.75,6.5,Inf),
  #                                   include.lowest=TRUE,
  #                                   labels=c("Sad","Neutral","Happy"))
}
remove(file, name) # Remove variables used in file importing that we don't need anymore.
attach(Data)

## EXPLORATORY ANALYSIS
# Get a feel for the data.


hist(Y2020$Score, breaks = seq(2,8, 0.5), xlab = "Score", main = "Happiness Score Distribution")

boxplot(Y2020$Score)
  summary(Y2020$Score) # Tells us the values displayed in the box plot.
#ggplot(Y2020, aes(Score)) + geom_boxplot()
  
ggplot(Y2020, aes(Rank, Score))  + geom_point(color = "blue")

## ANALYSIS
# Can't run regression on text, and we don't want to feed Rank into the model, 
#  that's just giving it the answers :)
dat = select(Data$Y2020, -c("Country", "Rank", "Dystopia + residual"))

## Linear regression between each attribute and the happiness score

# A list with each attribute and the R-squared when it is plotted in relation
#  to the happiness score.
fit = list()

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

# Make a nice table for each value + R-Squared.
fit.table = tibble(Attribute = names(fit), `Linear R-Squared` = unlist(fit, use.names = F))
fit.table = arrange(fit.table, desc(`Linear R-Squared`))
print(fit.table)

# Decision Tree Model
tree = ctree(Score ~ ., data = dat)
plot(tree)
visualize_model(tree, dat$Score, "Decision Tree")

# Random forest doesn't like spaces or parenthesis, so we remove them!
rfdata = dat %>% rename("Health" = "Health (Life Expectancy)") %>% remove_col_spaces()

rf = randomForest(Score ~ ., data = rfdata, proximity = TRUE, ntree = 130)
visualize_model(rf, rfdata$Score, "Random Forest")
plot(rf) # Error model, shows best number of trees.

importance = tibble(Attribute = row.names(rf$importance), as_tibble(rf$importance))
importance <- arrange(importance, desc(IncNodePurity))

sv = svm(Score ~ ., data = dat)
visualize_model(sv, dat$Score, "SVM")
# SVM Accuracy Rates:
# Baseline SVM = 74.51%
# Without GDP: 73.856
# Without Social Support: 73.203%
# Without Generosity 72.549
# W/o Freedom: 75.163%
# W/o Corruption: 75.817%
# W/o Health: 74.51%



## Happiness by Region
ggplot(Y2020, aes(Region, Score)) + geom_boxplot(aes(color = Region)) + ggtitle("Happiness by Region") + theme(axis.text.x=element_blank())