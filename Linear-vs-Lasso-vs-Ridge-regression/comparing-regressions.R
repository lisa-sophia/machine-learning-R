library(glmnet)

# tecator.csv contains results of study aimed to investigate whether a near
# infrared absorbance spectrum can be used to predict the fat content of samples of
# meat. For each meat sample the data consists of a 100 channel spectrum of
# absorbance records and the levels of moisture (water), fat and protein. The
# absorbance is -log10 of the transmittance measured by the spectrometer. The
# moisture, fat and protein are determined by analytic chemistry.
data = read.csv('tecator.csv', header = TRUE)
n=dim(data)[1]

set.seed(12345) 
id=sample(1:n, floor(n*.50)) 
train=data[id,] 
test=data[-id,]

# function to calculate mean squared error, given predictions and true labels
MSE <- function(Y, Yhat) {
  se <- sum((Y - Yhat)^2)
  n <- length(Y)
  mse <- se/n
  return (mse)
}

# trying linear regression
channels <- data[,2:101]
channels.train <- train[,2:101]
channels.test  <- test[,2:101]
fit <- lm(train$Fat ~ ., channels.train)
#summary(fit)
plot(train$Fat)
#line(,fit)

# prediction
fitted.train <- predict(fit, newdata = channels.train)
summary(fitted.train)
fitted.test  <- predict(fit, newdata = channels.test )
summary(fitted.test)
plot(fitted.test)
#lines(1:length(fit$fitted.values), fit$fitted.values, col='orange')
points(fitted.train, col='red')
# quality of the predictions -> mean squared error
mse.train <- MSE(train$Fat, fitted.train)
paste('Mean Squared Error for train: ', mse.train)

mse.test <- MSE(test$Fat, fitted.test)
paste('Mean Squared Error for test: ', mse.test)

#https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html

# since we have a lot of features (100), try Lasso regression instead!
covariates <- channels.train
response  <- train$Fat

# plotting regression coefficients depending on the log of penalty factor (log𝜆) 
lasso <- glmnet(x=as.matrix(covariates) , y=response, alpha=1)
plot(lasso, xvar = 'lambda', xlab=expression(paste('Log(',lambda,')')),
     label=TRUE, main="LASSO\n")
coef(lasso, s=.8)
# since we have a lot of features, we can decrease the amount to those that matter 
# more. From the plot we can observe that as λ grows, the number of coefficients 
# that are relevant / influece the regression model decreases.
# For example we could get a model with only three features for λ=0.8, which gives 
# us the active fat channels 6, 7 and 41.

plot(lasso$lambda, lasso$df, xlab=expression(lambda), ylab='Degree of Freedom', 
     main=expression(paste('Degree of Freedom depending on ', lambda)))
# the degrees of freedom is decreasing with larger λ, which makes sense 
# (less feautres -> less degrees of freedom)

## Now trying Ridge regression instead of Lasso regression
ridge <- glmnet(x=as.matrix(covariates) , y=response, alpha=0)
plot(ridge, xvar = 'lambda', xlab=expression(paste('Log(',lambda,')')),label=TRUE, main="Ridge\n")
# -> coefficients / features never really go to zero as clearly as for Lasso

## Stick with Lasso regression instead of Ridge
# using cross-validation to compute the optimal lasso model
lasso.cv <- cv.glmnet(x=as.matrix(covariates) , y=response, alpha=1)
plot(lasso.cv, main="LASSO CV\n")
print(lasso.cv)     # <- prints important values measured from CV. Values such as optimal lambdas, Degree of Freedom (I think) etc.
lasso.cv$lambda.min # <- gives the value of log(lambda) with mininmum mean CV error
lasso.cv$lambda.1se # <- gives the value of log(lambda) for the most regularized model in which error is within one standard error of the minimum

# the cross-validation plot has two dotted lines, these lines are the upper and 
# lower lambdas which produce the best CV score. As λ increases, the CV score 
# gets worse (i.e. the mean-squared error increases). One of the optimal λ here 
# would be λ_min = 0.05744535 ; then the number of active features is 9
coef(lasso.cv, s='lambda.min')
#coef(lasso.cv, s='lambda.1se')


# scatter plot of original test versus predicted test values for optimal model
cv.pred <- predict(lasso.cv, as.matrix(channels.test), s="lambda.min") 
plot(test$Fat,ylab='Fat', main='lm() test versus cv.glmnet() test ')
points(cv.pred, col='red')
# -> not sure if this is correct...

# using the feature values from test data and the optimal lasso model to  
# generate new target values (new data) 
cv.pred.train <- predict(lasso.cv, as.matrix(channels.train), s="lambda.min")
cv.pred.new <- predict(lasso.cv, as.matrix(channels.test), s="lambda.min")

#𝜎 = standard deviation of residuals from train data predictions
residual <- cv.pred.train-train$Fat

# adding noise
cv.pred.new <- cv.pred.new + rnorm(n=length(cv.pred.new), mean = 0, sd = abs(mean(residual))) 
#test.pred <- predict(lasso.cv, as.matrix(new_data), s="lambda.min")

plot(test$Fat, cv.pred.new, ylab='Fat', 
     main='Real Fat values versus optimal model predictions with added noise')
#points(cv.pred.new, col='red')

mse.opt.lasso <- MSE(test$Fat, cv.pred)
mse.gen.data <- MSE(test$Fat, cv.pred.new)
paste('MSE for the optimal Lasso model:', mse.opt.lasso)
paste('MSE for the generated data:', mse.gen.data)

