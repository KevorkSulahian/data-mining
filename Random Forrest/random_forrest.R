pml <- read.csv("Random Forrest/pml.csv")

sapply(pml, class)

library(caret)

set.seed(1)

index <- createDataPartition(pml$classe, p = .8, list = F)

train <- pml[index,]
test <- pml [-index,]

library(randomForest)

set.seed(1)

model_f <- model <- randomForest(classe ~., data = train, ntree = 50,
                                 do.trace = T)
model_f$err.rate[45:50]

set.seed(1)

model_f <- model <- randomForest(classe ~., data = train, ntree = 50,
                                 importance = T)

varImpPlot(model_f)

pr <- predict(model_f, newdata = test, type = "prob")

pr.class <- predict(model_f, newdata = test)
pr.class[1:200]

library(e1071)
confusionMatrix(pr.class, test$classe)


# different class

bank <- read.csv("Random Forrest/bank-1.csv")
sapply(bank, class)

set.seed(1)
index <- createDataPartition(bank$y, p = .8, list = F)
train <- bank[index,]
test <- bank[-index,]

set.seed(1)
model <- randomForest(y~., data = train, ntree = 25, do.trace = T )

pr <- predict(model, test, type = "prob")
pr[1:25,]

library(ROCR)
p_test <- prediction(pr[,2], test$y)
perf <- performance(p_test, "tpr", "fpr")
plot(perf)

performance(p_test, "auc")@y.values

set.seed(1)
trc <- trainControl(method = "cv", number = 10)
mtry_grid <- expand.grid(mtry = c(4,7,9,10))

set.seed(1)
train1 <- train[sample(nrow(train),5000),]
model2 <-  train(y~., data = train1, 
                 trControl = trc,
                 method = "rf",
                 ntree = 25,
                 tuneGrid = mtry_grid)

model2$results
model2$bestTune

pr <- predict(model2, newdata = test, type = "prob")
p_test <- prediction(pr[,2], test$y)
perf <- performance(p_test, "tpr", "fpr")
plot(perf)

performance(p_test, "auc")@y.values

set.seed(1)
trc <- trainControl(method = "cv", classProbs = T,
                    summaryFunction = twoClassSummary, number = 5)

set.seed(1)
mm <- train(y~., data = train1,
            trControl = trc, method = "rf",
            ntree = 25, metric = "ROC",
            tuneGrid = mtry_grid)
plot(mm)

varImpPlot(mm$finalModel)

