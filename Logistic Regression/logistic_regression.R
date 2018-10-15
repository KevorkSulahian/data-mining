Titanic <- read.csv("Logistic Regression/Titanic_imputed.csv")

Titanic$pclass <- as.factor(Titanic$pclass)
Titanic$survived <- factor(Titanic$survived, levels = c(0,1),
                           labels = c("No", "Yes"))

model1 <- glm(survived~sex, data = Titanic, family = "binomial")
summary(model1)

exp(coef(model1))

1/0.08843935

addmargins(table(Titanic$sex, Titanic$survived))

model22 <- glm(survived ~ sex + pclass + age + sibsp, family = "binomial", data = Titanic)
summary(model22)

exp(coef(model22))
coef(model22)
exp(3.29101970 -0.30032638*2 - 0.02688245*17) /(1 + exp(3.29101970 -0.30032638*2 - 0.02688245*17))
exp(3.29101970 -2.07951031 - 2.56329858 - 0.02688245*20) / ( 1 + exp(3.29101970 -2.07951031 - 2.56329858 - 0.02688245*20))

# class 2 Football database

seria <- read.csv("Logistic Regression/seriea_games.csv")
head(seria)
                                         
fmod <- glm(Result ~ FTHG, family = "binomial", data =seria)
summary(fmod)

exp(coef(fmod))

t <- table(seria$FTHG, seria$Result)
t
Wins <- t[,2]
df <- data.frame(Goals = 0:7, Wins)
df

df$props <- prop.table(t,1)[,2]

library(ggplot2)
ggplot(df, aes(x = Goals, y = props)) + geom_point() + geom_smooth(se = F)


# testing performance

credit <- read.csv("Logistic Regression/Credit.csv")
str(credit)

prop.table(table(credit$default))

library(caret)
set.seed(1)

trainIndex <- createDataPartition(credit$default, 
                                  p = .8, list = FALSE)


train <- credit[trainIndex, ]
test <- credit[-trainIndex,]

credit1 <- glm(default ~., data = train, family = "binomial") 
summary(credit1)

predict1 <- predict(credit1, newdata = test, type = "response") 
predict1[1:50]

addmargins(table(test$default, predict1 > .5))

pr_classes <- factor(ifelse(predict1 > .5, "Yes", "No"))

addmargins(table(test$default, predict1 > .5))

library(e1071)
confusionMatrix(pr_classes, test$default, positive = "Yes")

library(ROCR)

p_test <- prediction(predict1, test$default)

perf <- performance(p_test, "tpr", "fpr")

plot(perf, colorize = T)

performance(p_test, "auc")@y.values

str(perf)

FPR <- unlist(perf@x.values)
TPR <- unlist(perf@y.values)
alpha = unlist(perf@alpha.values)

df <- data.frame(FPR, TPR, alpha)
head(df)

table(test$default, predict1 > 0.92)

ggplot(df, aes(x = FPR, y = TPR, color = alpha)) + geom_line()

diabities <- read.csv("Decision Trees/Diabetes.csv")

sample <- sample(nrow(diabities), floor(nrow(diabities) * .8))
train <- diabities[sample,]
test <- diabities[-sample,]

model <- model(Class~., data = train)
