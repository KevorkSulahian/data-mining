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

