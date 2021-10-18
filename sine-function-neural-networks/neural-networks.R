# Training a neural network to learn the trigonometric sine function
library(neuralnet)
set.seed(12345)
# sample since function for x-values between 0 and 10
Var <- runif(500, 0, 10)
mydata <- data.frame(Var, Sin=sin(Var))

tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test

# Random initialization of the weights in the interval [-1, 1]
winit <- runif(20, -1, 1)

# training a neural net with 5 hidden layers
nn.1 <- neuralnet(Sin ~ Var, data=tr, startweights= winit, hidden = 5, rep = 100)
  
 # Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2,
     xlab='X', ylab='Sin(x)', 
     main='Prediction of Sin(x)')
points(te, col = "blue", cex=1)
points(te[,1],predict(nn.1,te), col="red", cex=1)
# -> looks pretty good!

#--------------------------------------------------------------------------

# Now sample sine function on a larger interval [0,20] instead of [0,10]
Var <- runif(500, 0, 20)
mydata2 <- data.frame(Var, Sin=sin(Var))

# trying to predict with neural net from above
plot(mydata2[,1], predict(nn.1,mydata2), col="red", cex=1,
     xlab='X', ylab='Sin(x)', 
     main='Prediction of Sin(x) given x and previously trained neuralnetwork')
points(x=mydata2[,1], y=mydata2[,2], col = "blue", cex=1)
points(tr, cex=2)
# conclusion: since neural network was only trained on values between 0-10, the 
# prediction is good for those values, but completely wrong when it comes to 
# values above 10 (no surprise though!)
# -> interpolation between training points (x <= 10) is good
# -> extrapolation beyond training points (x > 10) is bad, "magic" for nn's

#--------------------------------------------------------------------------

# Sample since function for x-values between 0 and 10 again
Var.3 <- runif(500, 0, 10)
mydata3 <- data.frame(Var.3, Sin=sin(Var.3))

# using these points as training points for learning a neural network that 
# predicts x from sin(x), so kind of "learns" the inverse sine function (unlike 
# before when the goals was to predict sin(x) from x) 
nn.3 <- neuralnet(Var.3 ~ Sin, 
                data=mydata3, 
                startweights= winit, 
                hidden = 5, 
                rep = 10,
                threshold=0.04)
plot(x=mydata3[,2], y=mydata3[,1], col="blue", cex=1, xlab='Sin', ylab='Predicted value for x', 
     main='Neural network trained to calculate x of Sin(x) ')
points(mydata3[,2],predict(nn.3,mydata3), col = "red", cex=1)

# Conclusion: Very bad results! That is no surprise though, as sine-values
# are defined for multiple x-values. E.g. the sine is 0 for x = 0; π; 2π; etc.
# Hence it's very hard to predict one exact x-value, since several x’s can be
# mapped to one y-value sin(x).
