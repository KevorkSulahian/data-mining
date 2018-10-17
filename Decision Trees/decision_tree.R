weather <- read.csv("Decision Trees/weather.csv")
str(weather)

library(rpart)
attach(weather)
model <- rpart(RainTomorrow~., data = weather)

library(rpart.plot)
prp(model)
prp(model, type = 2, extra = 1)

prp(model, type = 2, extra = 2)

prp(model, type = 2, extra = 4)

library(rattle)

asRules(model)
