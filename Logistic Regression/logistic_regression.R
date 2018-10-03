Titanic <- read.csv("Logistic Regression/Titanic_imputed.csv")

Titanic$pclass <- as.factor(Titanic$pclass)
Titanic$survived <- factor(Titanic$survived, levels = c(0,1),
                           labels = c("No", "Yes"))

model1 <- glm(survived~sex, data = Titanic, family = "binomial")
summary(model1)

exp(coef(model1))

1/0.08843935

addmargins(table(Titanic$sex, Titanic$survived))

model2 <- glm(survived ~ sex + pclass + age + sibsp, family = "binomial", data = Titanic)
summary(model2)

exp(coef(model2))
coef(model2)
exp(3.29101970 -0.30032638*2 - 0.02688245*17) /(1 + exp(3.29101970 -0.30032638*2 - 0.02688245*17))
exp(3.29101970 -2.07951031 - 2.56329858 - 0.02688245*20) / ( 1 + exp(3.29101970 -2.07951031 - 2.56329858 - 0.02688245*20))
