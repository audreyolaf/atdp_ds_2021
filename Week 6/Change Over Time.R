# Change Over Time 2

library(tidyverse)
library(randomForest)

visualize_model <- function(model, actualData, modelName = "Model", correctThreshold = 0.5) {
  comparison = tibble(Actual = actualData, Predicted = predict(model))
  comparison$Difference = comparison$Actual - comparison$Predicted
  comparison$Correct = abs(comparison$Difference) < correctThreshold
  accuracy = round(((count(filter(comparison, Correct == T)) / count(comparison))[[1]] * 100), 3)
  
  p <- ggplot(data = comparison, aes(x = Actual, y = Predicted)) + geom_point(aes(color = Correct)) + geom_abline(slope = 1, intercept = 0) + ggtitle(paste(modelName, "Accuracy"), subtitle = paste(accuracy, "% of countries were predicted within ", correctThreshold, " of their happiness score.", sep = "")) #+ geom_smooth(method = "lm")
  # .\nThe average difference is about ", round(mean(dataFrame$Difference), 10), "."
  return(p)
} 

# Scores.OT <- tibble(Rank = c(),
#                     Country = c(),
#                     Region = c(),
#                     Score = c(),
#                     #`Standard Error` = col_double(),
#                     GDP = c(),
#                     Family = c(),
#                     `Health (Life Expectancy)` = c(),
#                     Freedom = c(),
#                     Corruption = c(),
#                     Generosity = c())
#                     #`Dystopia Residual` = col_double())

# Makes mean() default to not using NA values.
mean <- function(x, ..., na.rm = TRUE) {
  base::mean(x, ..., na.rm = na.rm)
}

Scores.OT = tibble()
# A list containing all the data frames. Y2015 accesses the 2015 data frame, etc.
Data = list()
for (file in 2015:2020) {
  # The filenames all have the same format: 2015.csv, etc...
  # Iterate through 2015-2020 to import all of them.
  name = paste("Y", file, sep = "") # Add 'Y' to the front of the name, you can't
  #  have columns that are purely numbers.
  dataFrame <- as_tibble(read_csv(paste("Data/Happiness/", file, ".csv", sep = "")))
  
  # The Region column should be a factor, because there is a limited number of them.
  dataFrame$Region <- as.factor(dataFrame$Region)
    
  if (is.null(dataFrame$Family)) {
    dataFrame$Family = NA
  }

  if (is.null(dataFrame$`Social Support`)) {
    dataFrame$`Social Support` = NA
  }

  # 2020 doesn't have GDP, but uncomparable logged GDP instead.
  if (is.null(dataFrame$GDP)) {
    dataFrame$GDP = NA
  }
  newRows <- select(dataFrame, c("Rank", "Region", "GDP", "Health (Life Expectancy)", "Corruption", "Country", "Score", "Family", "Freedom", "Generosity", "Social Support"))
  newRows$Year <- file
  
  Scores.OT <- bind_rows(Scores.OT, newRows)
  
  Scores.OT <- add_row(Scores.OT, newRows)
  # Sets that year's data as new data frame in the list.
  Data[[name]] <- dataFrame
  
}
remove(file, name) # Remove variables used in file importing that we don't need anymore.
#attach(Data)

Scores.OT <- Scores.OT %>% group_by(Year)

#  X cols = 2020 , y cols = 2015 
countryDiff = full_join(Data$Y2020, Data$Y2015, by = "Country")

countryDiff = countryDiff %>% select(-Region.y) %>% rename(Region = Region.x)

countryDiff$Change = countryDiff$Score.x - countryDiff$Score.y

# How much a country changed, disregarding whether the change was negative
#     or positive.
countryDiff$`Absolute Change` = abs(countryDiff$Change)

countryDiff <- countryDiff %>%
  filter(!is.na(Change))

countryDiff <- arrange(countryDiff, desc(`Absolute Change`))


ggplot(countryDiff, aes(x = Change)) + geom_boxplot() + ggtitle("Country Happiness Changes, 2015-2020")
ggplot(countryDiff, aes(y = Change, color = Region)) + geom_boxplot() + ggtitle("Happiness Change by Region, 2015-2020")

## Large changes

# largeChanges <- add_row(head(countryDiff, 2), tail(countryDiff, 2))
largeChanges <- head(countryDiff, 10)

barplot(table(largeChanges$Region))
#theme_set(theme_grey())
ggplot(largeChanges, aes(Region)) + geom_bar(stat = "count") + 
  ggtitle("Top 10 Most Changed Countries")

## Over time
#ggplot(Scores.OT, aes(Year, Score, group = Country)) + geom_line()

# Stats by year. This worked because we used group_by(), so the computer knows
#   to differentiate data points by year.
year.stats <- Scores.OT %>%
  summarize(mean = mean(Score), median = median(Score), min = min(Score), max = max(Score),
            mean.gdp = mean(GDP), mean.health = mean(`Health (Life Expectancy)`), mean.corruption = mean(Corruption), mean.family = mean(Family), mean.freedom = mean(Freedom), mean.generosity = mean(Generosity), mean.support = mean(`Social Support`))


ggplot(year.stats, aes(x = Year)) + geom_smooth(aes(y = mean.corruption)) +
  ggtitle("Mean Happiness") + ylab("World Average")

#   geom_smooth(aes(y = c(proportions(na.omit(mean.gdp)), NA)))

#geom_smooth(aes(y = mean.gdp, color = "GDP")) +

imputed = rfImpute(mean ~ ., data = select(year.stats, -c("Year", "median", "min", "max")))
rf = randomForest(mean ~ ., data = imputed, proximity = T)



visualize_model(rf, imputed$mean)
importance = as_tibble(rf$importance)
importance$Attribute = row.names(rf$importance)
importance <- arrange(importance, IncNodePurity)
