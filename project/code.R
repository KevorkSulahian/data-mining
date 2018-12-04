data <- read.csv("project/BlackFriday.csv")

library(ISLR)

x = model.matrix(Purchase~., data)[,c(-1,-2)]
y = data$Purchase

library(glmnet)
grid = 10^seq(10,-2, length = 100)
ridge.mod = glmnet(x,y,alpha = 0, lambda = grid)

dim(coef(ridge.mod))

ridge.mod$lambda[50]

coef(ridge.mod)[,50]

predict(ridge.mod, s = 50, type = "coefficients")[1:16,]

set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]


ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid,
                   thresh = 1e-12)
ridge.pred =predict(ridge.mod, s = 4, newx = x[test,])

mean((ridge.pred - y.test)^2)

mean((mean(y[train]) - y.test)^2)



### lasso 
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])

mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:17,]
lasso.coef
lasso.coef[lasso.coef!=0]

mean((ridge.pred - y.test)^2)  - mean((lasso.pred-y.test)^2)

# lasso is better?






