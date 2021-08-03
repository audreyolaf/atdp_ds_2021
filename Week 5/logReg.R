mtcars = mtcars
print(mtcars)

log = glm(vs ~ wt, family = binomial, data = mtcars)
print(log)

xweight <- seq(0, 6, 0.01)

yweight <- predict(log, list(wt = xweight), type="response")

plot(mtcars$wt, mtcars$vs, pch = 16, xlab = "WEIGHT (g)", ylab = "VS")

lines(xweight, yweight)

install.packages("pROC")
library(pROC)
roc(mtcars$vs, log$fitted.values, plot = TRUE)



# crash
crash.log = glm(Survived ~ Age, family = "binomial", data = OA.9.13...crash)
print(crash.log)

crash.xweight <- seq(20, 120, 1)
crash.yweight <- predict(crash.log, list(Age = crash.xweight), type="response")

# plot(OA.9.13...crash$Age, OA.9.13...crash$Speed, pch = 16, xlab = "Age", ylab = "Speed")
plot(OA.9.13...crash$Speed, OA.9.13...crash$Survived, pch = 16, xlab = "Speed", ylab = "Survived")
plot(OA.9.13...crash$Age, OA.9.13...crash$Survived, pch = 16, xlab = "Age", ylab = "Survived")
# plot(crash.xweight, crash.yweight)

lines(crash.xweight, crash.yweight)

library(pROC)
roc(OA.9.13...crash$Survived, crash.log$fitted.values, plot = TRUE)
