library(caret)
library(e1071)
library(ROCR)

data <- read.csv("Naive Bayes/HR_balanced.csv")

set.seed(1)
index <- createDataPartition(data$left, p = .75, list = F)
train <- data[index,]
test <- data [-index,]

model <- naiveBayes(left ~., data = train, laplace = 1)

pred <- predict(model, newdata = test)
confusionMatrix(pred, test$left, positive = "Yes")
  
pred_prob <- predict(model, newdata = test, type = "raw")
head(pred_prob)

p_test <- prediction(pred_prob[,2], test$left)
perf <- performance(p_test, "tpr", "fpr")
plot(perf)

performance(p_test,"auc")@y.values
