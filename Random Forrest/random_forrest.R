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
