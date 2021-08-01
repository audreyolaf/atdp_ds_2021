# filter tailnum containing '4'
# summarize mean delay
# nycflights
flight_four <- function(f) {
  filter(clean_nyc_flights_delays, grepl(4, clean_nyc_flights_delays[c('tailnum')]))
}

View(filter(clean_nyc_flights_delays, grepl('4', clean_nyc_flights_delays[c('tailnum')])))

nycflights_carrier_delay %>%
  group_by(carrier) %>%
  summarise_at(vars(arr_delay), list(arr_delay = mean))
  
clean_nyc_flights_date %>%
  group_by(month, day) %>%
  summarise_at(vars(arr_delay), list(arr_delay = mean))

clean_nyc_flights_weather %>%
  group_by(month, day) %>%
  summarise_at(vars(wind_speed), list(wind_speed = mean))

# rice classification
ggplot(riceClassification, aes(Area, MajorAxisLength, color = Class)) + geom_point()
#area / roundness
riceClassification <- na.omit(riceClassification)

p = .8
total.row = nrow(riceClassification)
row.nums = sample.int(total.row, p * total.row)

training.data = slice(riceClassification, row.nums)
validating.data = slice(riceClassification, -row.nums)

View(training.data)
View(validating.data)

riceClassification = select(riceClassification, c("Area", "MajorAxisLength", "Class"))
View(riceClassification)
only.training = select(training.data, c("Area", "MajorAxisLength"))
only.validating = select(validating.data, c("Area", "MajorAxisLength"))
class = select(training.data, c("Class"))

rice.pred = knn(only.training, only.validating, class$"Class", k=7)
rice.pred = knn(training.data, validating.data, class$"Class", k=7)
data.outcome = data.frame(validating.data, rice.pred)
table(validating.data$Class, rice.pred)


#weather knn
p = .8
weather.row.total = nrow(Screenshot.2021.07.07.160705.png)
weather.row.nums = sample.int(weather.row.total, p * weather.row.total)
Screenshot.2021.07.07.160705.png = select(Screenshot.2021.07.07.160705.png, c("Humidity", "Outlook", "Play"))

weather.training = slice(Screenshot.2021.07.07.160705.png, weather.row.nums)
weather.validating = slice(Screenshot.2021.07.07.160705.png, -weather.row.nums)

View(weather.training)
View(weather.validating)
View(Screenshot.2021.07.07.160705.png)

only.weather.training = select(weather.training, c("Humidity", "Outlook"))
only.weather.validating = select(weather.validating, c("Humidity", "Outlook"))
cl = select(weather.training, c("Play"))

knn(only.weather.training, only.weather.validating, cl$Play, k=3)
knn(weather.training, weather.validating, cl$Play, k=3)
# data.outcome = data.frame(validating.data, rice.pred)
# table(validating.data$Class, rice.pred)


# fertility knn
p = .8
fertility.row.total = nrow(Screenshot.2021.07.08.183016.png)
fertility.row.nums = sample.int(fertility.row.total, p * fertility.row.total)
# Screenshot.2021.07.08.183016.png = select(Screenshot.2021.07.08.183016.png, c("Humidity", "Outlook", "Fertility"))

fertility.training = slice(Screenshot.2021.07.08.183016.png, fertility.row.nums)
fertility.validating = slice(Screenshot.2021.07.08.183016.png, -fertility.row.nums)

View(fertility.training)
View(fertility.validating)
View(Screenshot.2021.07.08.183016.png)

only.fertility.training = select(fertility.training, c("Examination", "Education"))
only.fertility.validating = select(fertility.validating, c("Examination", "Education"))
fer = select(fertility.training, c("Fertility"))

fertility.pred = knn(only.fertility.training, only.fertility.validating, fer$Fertility, k=3)
# knn(fertility.training, fertility.validating, fer$Fertility, k=3)
data.outcome = data.frame(fertility.validating, fertility.pred)
table(fertility.validating$Fertility, fertility.pred)

str(OA.9.21...overdrawn)
# convert to categorical data
# OA.9.21...overdrawn[OA.9.21...overdrawn$DaysDrink < 7,]$DaysDrink <- 0
OA.9.21...overdrawn = mutate(OA.9.21...overdrawn, DaysDrink = replace(DaysDrink, DaysDrink < 7, 0))
OA.9.21...overdrawn = mutate(OA.9.21...overdrawn, DaysDrink = replace(DaysDrink, DaysDrink = 7, 1))
OA.9.21...overdrawn = mutate(OA.9.21...overdrawn, DaysDrink = replace(DaysDrink, DaysDrink < 14 && DaysDrink >= 7, 1))
OA.9.21...overdrawn = mutate(OA.9.21...overdrawn, DaysDrink = replace(DaysDrink, DaysDrink >= 14, 2))
OA.9.21...overdrawn$DaysDrink <- as.factor(OA.9.21...overdrawn$DaysDrink)


str(OA.9.22...tipjoke)

OA.9.22...tipjoke$TipF = factor(OA.9.22...tipjoke$Tip)
OA.9.22...tipjoke = select(OA.9.22...tipjoke, -TipF)
str(OA.9.22...tipjoke)

tip_tree = ctree(OA.9.22...tipjoke$TipF ~., data = OA.9.22...tipjoke)
plot(tip_tree)
tip_test = predict(tip_tree, newdata = OA.9.22...tipjoke)
tip_table(predict(tip_tree), water1$TipF)
