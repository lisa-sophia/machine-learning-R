data = read.csv('parkinsons.csv', header = TRUE)
# scale and center the data (this gets rid of intercept)
data.scale = scale(data)

# scaling the data and dividing it into training and test data (60/40)
# -> all regression models will not have an intercept 
n=dim(data.scale)[1]

set.seed(12345) 
id=sample(1:n, floor(n*0.60)) 
train=data.scale[id,]
test=data.scale[-id,]

## Trying out Ridge regression by myself (not R's function)
# Assuming that motor_UPDRS is normally distributed and can be modeled by 
# Ridge regression of the voice characteristics, we can write the probabilistic 
# model as a Bayesian model (posterior ~ likelihood * prior):
# y ∼ N(y | w0 + X*w, σ^2*I)
# w ∼ N(0, σ^2/λ * I)

# P(D|w,σ)
# function that for a given parameter vector 𝒘 and
# dispersion 𝜎 computes the log-likelihood function log 𝑃(𝐷|𝑤, 𝜎) for
# the model for the training data
Loglikelihood <- function(w, sig) {
  y <- as.matrix(train[,5])
  X <- as.matrix(train[,7:22])
  n <- length(y)
  return( -n/2*log(2*pi*sig^2) - 1/(2*sig^2) * (t(y-(X%*%w)) %*% (y-(X%*%w))) )
}

# function that for given vector 𝒘, scalar 𝜎and scalar 𝜆 uses
# function above and adds up a Ridge penalty to the minus loglikelihood
Ridge <- function(w, sig, lambda) {
  return( -Loglikelihood(w, sig) + lambda*t(w)%*%w )
}

# function to go into optim()
Ridge.helper <- function(par, lambda) {
  w <- c(par[1:16])
  sig <- par[17]
  return(Ridge(w,sig,lambda))
}

# function that depends on scalar 𝜆 , uses functions above and function 
# optim() with method=”BFGS” to find the optimal 𝑤 and 𝜎 for the given 𝜆.
RidgeOpt <- function(lambda) {
  # par = [w, sigma], with initial values of 1
  x <- optim(par = replicate(17, 1), Ridge.helper, method='BFGS', lambda = lambda)
  return(x)
}

# function that for a given scalar 𝜆 computes the degrees of freedom of the 
# regression model based on the training data.
DF <- function(lambda) {
  X <- as.matrix(train[,7:22])
  return( sum(diag( X %*% solve(t(X)%*%X + lambda*diag(16)) %*% t(X)) ) ) 
}


# using function RidgeOpt to compute optimal w parameters for different lambadas 
# (𝜆=1,𝜆=100 and 𝜆=1000)
lambda.1 <- RidgeOpt(1)
lambda.1$w = c(lambda.1$par[1:16])
lambda.1$sig = lambda.1$par[17]
cat('For lambda = 1: sigma =', lambda.1$sig, ', w = [', lambda.1$w, ']')

lambda.100 <- RidgeOpt(100)
lambda.100$w = c(lambda.100$par[1:16])
lambda.100$sig = lambda.100$par[17]
cat('For lambda = 100: sigma =', lambda.100$sig, ', w = [', lambda.100$w, ']')

lambda.1000 <- RidgeOpt(1000)
lambda.1000$w = c(lambda.1000$par[1:16])
lambda.1000$sig = lambda.1000$par[17]
cat('For lambda = 1000: sigma =', lambda.1000$sig, ', w = [', lambda.1000$w, ']')

# mean squared error MSE = 1/n * sum( (y - y_predict)^2 ) = 1/n * (y - y_predict)^T*(y - y_predict)
# y_predict = X*w
MSE <- function(data, w) {
  X = data[,7:22]
  y = data[,5]
  n = length(y)
  
  y.predict = X %*% w
  dy = y.predict - y
  mse = 1/n * (t(dy) %*% dy)
}

# predicting the motor_UPDRS values for training and test data: 
# prediction = data X * weights w

# MSE for training data
lambda.1.train.mse = MSE(train, lambda.1$w)
lambda.100.train.mse = MSE(train, lambda.100$w)
lambda.1000.train.mse = MSE(train, lambda.1000$w)

paste('MSE for training data: MSE(𝜆 = 1) =', round(lambda.1.train.mse, 5), ', MSE(𝜆 = 100) =', 
      round(lambda.100.train.mse, 5), ', MSE(𝜆 = 1000) =', round(lambda.1000.train.mse, 5))

# MSE for test data
lambda.1.test.mse = MSE(test, lambda.1$w)
lambda.100.test.mse = MSE(test, lambda.100$w)
lambda.1000.test.mse = MSE(test, lambda.1000$w)

paste('MSE for test data: MSE(𝜆 = 1) =', round(lambda.1.test.mse, 5), ', MSE(𝜆 = 100) =', 
      round(lambda.100.test.mse, 5), ', MSE(𝜆 = 1000) =', round(lambda.1000.test.mse, 5))

# model evaluation: computing Akaike Information Criterion (AIC) scores for the 
# Ridge models with values𝜆=1,𝜆=100 and 𝜆=1000 & their optimal parameters 

# AIC scores (equation from wikipedia on AIC)
lambda.1.aic <- -2*Loglikelihood(lambda.1$w, lambda.1$sig) + 2*DF(1)
lambda.100.aic <- -2*Loglikelihood(lambda.100$w, lambda.100$sig) + 2*DF(100)
lambda.1000.aic <- -2*Loglikelihood(lambda.1000$w, lambda.1000$sig) + 2*DF(1000)

paste('AIC of the model: AIC(𝜆 = 1) =', round(lambda.1.aic, 2), ', AIC(𝜆 = 100) =', 
      round(lambda.100.aic, 2), ', AIC(𝜆 = 1000) =', round(lambda.1000.aic, 2))
