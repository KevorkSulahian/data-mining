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


## non finsihed tho

credit <- read.csv("Logistic Regression/Credit.csv")
library(caret)

index <- createDataPartition(credit$default, p = .8, list = F)
train <- credit[index,]
test <- credit[-index,]

model_c <- rpart(default~., data = train)

prp(model_c, type = 2, extra =4 )

pred_class <- predict(model_c, test, type = "class")

confusionMatrix(pred_class, test$default, positive = "Yes")

pred_prob <- predict(model_c, test)

library(ROCR)
P_test <- prediction(pred_prob[,2], test$default)
perf <- performance(P_test, "tpr", "fpr")
plot(perf)
performance(P_test, "auc")@y.values


### EXCERCISE TIME BOIZ

exercise <- read.csv("Decision Trees/HR_data.csv")

index <- createDataPartition(exercise$left, p = .8, list = F)
train <- exercise[index,]
test <- exercise[-index,]

model_c <- rpart(left~., data = train)

prp(model_c, type = 2, extra =4 )

pred_class <- predict(model_c, test, type = "class")
# 0.9727
confusionMatrix(pred_class, test$left, positive = "Yes")

pred_prob <- predict(model_c, test)
P_test <- prediction(pred_prob[,2], test$left)
perf <- performance(P_test, "tpr", "fpr")
plot(perf)
performance(P_test, "auc")@y.values
# 0.9766683


### Logisitc
model_reg <- glm(left ~., data = train, family = "binomial") 
summary(model_reg)
  
pred_prob2 <- predict(model_reg, newdata = test, type = "response") 

pr_classes <- factor(ifelse(pred_prob2 > .5, "Yes", "No"))
# ACC = 0.7989
confusionMatrix(pr_classes, test$left, positive = "Yes")

P_test2 <- prediction(pred_prob2, test$left)
perf2 <- performance(P_test2, "tpr", "fpr")
plot(perf2)
performance(P_test2, "auc")@y.values
# 0.8199695






