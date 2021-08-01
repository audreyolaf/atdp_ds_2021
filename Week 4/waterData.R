water_potability <- na.omit(water_potability)
water_potability$ph = as.numeric(water_potability$ph)
# doesn't work
water_potability = mutate(water_potability, ph = replace(ph, ph < 5, 0))
water_potability = mutate(water_potability, ph = replace(ph, ph < 10 && ph >= 5, 1))
water_potability = mutate(water_potability, ph = replace(ph, ph >= 10, 2))


water1$PotabilityF = factor(water1$Potability)
water1 = select(water1, -PotabilityF)
str(water1)

tree = ctree(water1$PotabilityF ~., data = water1)
plot(tree)
test = predict(tree, newdata = water1)
table(predict(tree), water1$PotabilityF)


# knn
water_potability <- na.omit(water_potability)
library(class)
p = .8
water.total.row = nrow(water_potability)
water.row.nums = sample.int(water.total.row, p * water.total.row)

water.training.data = slice(water_potability, water.row.nums)
water.validating.data = slice(water_potability, -water.row.nums)

View(water.training.data)
View(water.validating.data)

water_potability = select(water_potability, c("Hardness", "Sulfate", "Potability"))
View(water_potability)
water.only.training = select(water.training.data, c("Hardness", "Sulfate"))
water.only.validating = select(water.validating.data, c("Hardness", "Sulfate"))
class = select(water.training.data, c("Potability"))

water.pred = knn(water.only.training, water.only.validating, class$"Potability", k=3)
water.pred = knn(water.training.data, water.validating.data, class$"Potability", k=3)
data.outcome = data.frame(water.validating.data, water.pred)
table(water.validating.data$Potability, water.pred)


# tree
library(party)
cat_water_potability <- na.omit(cat_water_potability)
str(cat_water_potability)
cat_water_potability$PotabilityF = factor(cat_water_potability$Potability)
cat_water_potability = select(cat_water_potability, -PotabilityF)
str(cat_water_potability)

water.tree = ctree(cat_water_potability$Potability ~., data = cat_water_potability)
plot(water.tree)
water.test = predict(water.tree, newdata = cat_water_potability)
table(predict(water.tree), cat_water_potability$Potability)


# str(water1)
water1$PotabilityF = factor(water1$Potability)
water1 = select(water1, -PotabilityF)
str(water1)

# tree = ctree(water1$PotabilityF ~ ., data = water1)
plot(tree)
test = predict(tree, newdata = water1)
table(predict(tree), water1$PotabilityF)

# forest
water_potability <- na.omit(cat_water_potability)
# cat_water_potability$ph = as.factor(cat_water_potability$ph)
cat_water_potability$hardness2 = as.factor(cat_water_potability$hardness2)
cat_water_potability$solids2 = as.factor(cat_water_potability$solids2)
cat_water_potability$chloramines2 = as.factor(cat_water_potability$chloramines2)
cat_water_potability$sulfate2 = as.factor(cat_water_potability$sulfate2)
cat_water_potability$conductivity2 = as.factor(cat_water_potability$conductivity2)
cat_water_potability$organic_carbon2 = as.factor(cat_water_potability$organic_carbon2)
cat_water_potability$trihalomethanes2 = as.factor(cat_water_potability$trihalomethanes2)
cat_water_potability$turbidity2 = as.factor(cat_water_potability$turbidity2)

set.seed(20)

water.forest = randomForest(Turbidity ~., data = water_potability, proximity = TRUE)
water.forest

View(water.forest$err.rate)

water.error = data.frame(numTrees = rep(1:100, times = 3, Error = water.forest$err.rate[,"OOB"]))
View(water.error)
ggplot(water.error, aes(numTrees, Error)) + geom_line()


# idk

install.packages("party")
library(party)

str(water_potability)
water_potability$PotabilityF = factor(water_potability$Potability)
water_potability = select(water_potability, -Potability)
str(water_potability)

tree = ctree(water_potability$PotabilityF ~., data = water1)
plot(tree)
test = predict(tree, newdata = water1)
table(predict(tree), water_potability$PotabilityF)


# str(water1)
water1$PotabilityF = factor(water1$Potability)
water1 = select(water1, -PotabilityF)
str(water1)

# tree = ctree(water1$PotabilityF ~ ., data = water1)
plot(tree)
test = predict(tree, newdata = water1)
table(predict(tree), water1$PotabilityF)

