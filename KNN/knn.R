example <- read.csv("KNN/example.csv")

library(ggplot2)
ggplot(example, aes(x = X, y = Y, color = Class)) + geom_point(size = 4)

example[12,] <- c(30,130000, NA)
ggplot(example, aes(x = X, y = Y, color = Class)) + geom_point(size = 4) +
  geom_label(label = row.names(example))


d <- as.matrix(dist(example[,1:2]))
sort(d[,12])

example


sc <- scale(example[,1:2])
sc <- as.data.frame(sc)
example[,1:2] = sc

d <- as.matrix(dist(example[,1:2]))
sort(d[,2])


ggplot(example, aes(x = X, y = Y, color = Class)) + geom_point(size = 4) +
  geom_label(label = row.names(example))


# Signs

signs <- read.csv("KNN/signs.csv")
sapply(signs, class)
  
library(class)

train <- signs[signs$sample == "train",]
test <- signs[signs$sample == "test",]

train$sample <- NULL
test$sample <- NULL

sign_1 <- read.csv("KNN/sign_1.csv")

knn1 <- knn(train = train[,-1], test = test[,-1], cl = train$sign_type, k = 5)

table(knn1, test$sign_type)

mean(knn1 == test$sign_type)

knn_p <- knn(train[,-1], test[-1], cl= train$sign_type, k = 5, prob = T)
attr(knn_p, "prob")

df <- data.frame(class = knn1, probs = attr(knn_p, "prob"))
head(df)

library(caret)

diab <- read.csv("Decision Trees/Diabetes.csv")

set.seed(1)
ctrl <- trainControl(method = "cv", number = 10)


knn_c <- train(Class~., data = diab, method = "knn", 
               trControl = ctrl, preProcess = c("centern", "scale"), tunLength = 10)

#### WHAT










