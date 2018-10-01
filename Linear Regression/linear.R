library(ggplot2)
data("mtcars")
ggplot(mtcars, aes(x = hp, y = mpg))+ geom_point() +
  geom_hline(yintercept = mean(mtcars$mpg), col = 'red', size = 1.5) +
  ggtitle("Intercept only model")

model <- lm(mpg~ 1, data = mtcars)
summary(model)
mean(mtcars$mpg)

ggplot(mtcars, aes(x = hp, y = mpg))+ geom_point() +
  geom_segment(aes(xend = hp, yend = mean(mtcars$mpg)), alpha = .2, col='red') +
  geom_hline(yintercept = mean(mtcars$mpg), col = 'blue', size = 1.5) +
  ggtitle("Intercept only model")

ggplot(mtcars, aes(x = hp, y = mpg))+ geom_point() +
  geom_abline(intercept = 29, slope = -0.06, col = "red", size = 1.5) +
  ggtitle("the best line")

mod3 <- lm(mpg~hp, data  =mtcars)
names(mod3)
summary(mod3)

ggplot(mtcars, aes(x = hp, y = mpg))+ geom_point() +
  geom_abline(intercept = 30.09886, slope = -0.06823, col = "red", size = 1.5) +
  ggtitle("the best line")

ggplot(mtcars, aes(x = hp, y = mpg)) +geom_point() +
  geom_smooth(method = 'lm')

# Class 2

## SStot
ggplot(mtcars, aes(x = hp, y = mpg))+ geom_point() +
  geom_segment(aes(xend = hp, yend = mean(mtcars$mpg)), alpha = .2, col='red') +
  geom_hline(yintercept = mean(mtcars$mpg), col = 'blue', size = 1.5) +
  ggtitle("Intercept only model")
  
## SSreg
ggplot(mtcars, aes(x = hp, y = fitted(mod3)))+ geom_point() +
  geom_segment(aes(xend = hp, yend = mean(mtcars$mpg)), alpha = .2, col='red') +
  geom_hline(yintercept = mean(mtcars$mpg), col = 'blue', size = 1.5)

##SSres
ggplot(mtcars, aes(x = hp, y = mpg))+ geom_point() +
  geom_smooth(method='lm', se = F, color = "blue") +
  geom_segment(aes(xend = hp, yend = fitted(mod3)), alpha = .2, col='red') +
  geom_point() + geom_point(aes(y=fitted(mod3)), shape  = 1)


seriea <- read.csv("Linear Regression/seriea.csv")
sapply(seriea, class)

model1 <- lm(G~POS, data = seriea)
summary(model1)

F <- mean(seriea$G[seriea$POS == "F"])
D <- mean(seriea$G[seriea$POS == "D"])
M <- mean(seriea$G[seriea$POS == "M"])

F
D
M

ggplot(seriea, aes(x = SH, y = G)) + geom_point()

model2 <- lm(G~POS+SH, data = seriea)
summary(model2)

model3 <- lm(G~POS + ST, data =seriea)
summary(model3)

model4 <- lm( G~POS+ ST+ SH, data = seriea)
summary(model4)

coef(model2)
coef(model4)

cor(seriea$SH, seriea$ST)

ggplot(seriea, aes(x = SH, y = ST))+ geom_point()

marriage <- data.frame(Happy = c(7.8, 7.6, 7.4, 7.6, 7.65, 7.4, 7.35, 7.3, 7.4, 7.5),
                       Year = 1:10)

ggplot(marriage, aes(x = Year, y = Happy)) +geom_point() + ylim(c(6,8)) + 
  scale_x_continuous(breaks = 1:10) + labs(x = "Years", y = "Happinesss") +
  geom_smooth(method = 'lm', se = FALSE)

mar1 <- lm(Happy~Year, data = marriage)
summary(mar1)

mar2 <- lm(Happy ~ poly(Year, 2), data = marriage)
summary(mar2)

ggplot(marriage, aes(x = Year, y = Happy)) +geom_point() + ylim(c(6,8)) + 
  scale_x_continuous(breaks = 1:10) + labs(x = "Years", y = "Happinesss") +
  stat_smooth(method = 'lm', se = FALSE, formula =  y~poly(x,2))

ggplot(marriage, aes(x = Year, y = Happy)) +geom_point() + ylim(c(6,8)) + 
  scale_x_continuous(breaks = 1:10) + labs(x = "Years", y = "Happinesss") +
  stat_smooth(method = 'lm', se = FALSE, formula =  y~poly(x,6))

mar6 <- lm(Happy ~ poly(Year, 6), data = marriage)
summary(mar6)

ggplot(marriage, aes(x = Year, y = Happy)) +geom_point() + ylim(c(6,8)) + 
  scale_x_continuous(breaks = 1:10) + labs(x = "Years", y = "Happinesss") +
  stat_smooth(method = 'lm', se = FALSE, formula =  y~poly(x,8))

mar8 <- lm(Happy ~ poly(Year, 8), data = marriage)
summary(mar8)

ggplot(marriage, aes(x = Year, y = Happy)) +geom_point() + ylim(c(6,8)) + 
  scale_x_continuous(breaks = 1:10) + labs(x = "Years", y = "Happinesss") +
  stat_smooth(method = 'lm', se = FALSE, formula =  y~poly(x,9))

mar9 <- lm(Happy ~ poly(Year, 9), data = marriage)
summary(mar9)

## Model selection in practice

library(MASS)
data("Boston")
set.seed(1)
sample <- sample(nrow(Boston), floor(nrow(Boston) * 0.7))
sample[1:5]

length(sample)

train <- Boston[sample,]
test <- Boston[-sample,]

model <- lm(medv~., train)


pred1 <- predict(model, newdata = test)
pred1[1:10]

sqrt(mean((test$medv- pred1)^2))


