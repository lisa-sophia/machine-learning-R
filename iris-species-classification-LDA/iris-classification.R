library(datasets)
library(ggplot2)
library(MASS)
library(mvtnorm)
library(nnet)

## linear discriminant analysis (LDA) on iris dataset
 
# function that computes prior probability
compute_prior <- function(data_set) {
  n <- nrow(data_set)
  occurances <- table(data_set$Species)  
  prior <- occurances / n
  return (prior)
}

# mean for each class
hard_coded_mean <- function(data_set) {
  specie <- as.character(unique(data_set$Species))
  df <- data.frame(Sepal.Length=numeric(),Sepal.Width=numeric(), Species=factor())
  for(thing in specie){
    idx <- which(data_set$Species == thing)
    class.data<- data_set[idx,]
    n <- length(idx)
    mean.1 <- mean(class.data$Sepal.Length)
    mean.2 <- mean(class.data$Sepal.Width)
    temp <- data.frame(mean.1, mean.2, thing)
    df <- rbind(df,temp)
  }
  names(df) <- c("Sepal.Length","Sepal.Width","Species")
  return(df)
}


discriminant_function <- function(x, mu, prior) {
  
  first <- t(x)%*%solve(cov.matrix)%*%mu
  second <- (1/2)*t(mu)%*%solve(cov.matrix)%*%mu
  third <- log(prior)
  return (first-second+third)
}

set.seed(12345)
data(iris)

# scatterplot of Sepal Width versus Sepal Length, observations are colored by Species
Sepal.Length <- iris$Sepal.Length
ggplot(data = iris, mapping = aes(x = Sepal.Width, y = Sepal.Length)) + 
  geom_point(aes(color = Species), size = 3, alpha = 0.6) +
  xlab("Sepal Width") +
  ylab("Sepal Length") 

# LDA: 4 steps
# 1) Compute mean, covariance matrices and prior probabilities per class
means <- hard_coded_mean(iris)
prior <- compute_prior(iris)

iris.cov.setosa  <- cov(iris[1:50, 1:2])
iris.cov.versicolor    <- cov(iris[51:100, 1:2])
iris.cov.virginica   <- cov(iris[101:150, 1:2])


# 2) Compute overall (pooled) covariance matrix
cov.matrix <- 1/nrow(iris) * ( 50*iris.cov.setosa + 50*iris.cov.versicolor + 50*iris.cov.virginica )
cov.matrix

# 3) Compute discriminant functions for each class
d.setosa = c()
d.versicolor = c()
d.virginica = c()
for(k in 1:150) {
  d.setosa <- c(d.setosa, discriminant_function(as.numeric(iris[k, 1:2]), 
                                    as.numeric(means[1,1:2]), 
                                    prior[1]))
  
  d.versicolor <- c(d.versicolor, discriminant_function(as.numeric(iris[k, 1:2]), 
                                        as.numeric(means[2,1:2]), 
                                        prior[2]))
  
  d.virginica <- c(d.virginica, discriminant_function(as.numeric(iris[k, 1:2]), 
                                        as.numeric(means[3,1:2]), 
                                        prior[3]))
}

# 4) Compute equations of decision boundaries between classes 
Wi <- function(mu, prior) {
  return (solve(cov.matrix)%*%mu)
}
W0 <- function(mu, prior) {
  return (-1/2*t(mu)%*%solve(cov.matrix)%*%mu+log(prior))
}

w0.setosa <- W0( as.numeric(means[1,1:2]), prior[1])
w.setosa  <- Wi( as.numeric(means[1,1:2]), prior[1])

w0.versicolor <- W0(as.numeric(means[2,1:2]), prior[2])
w.versicolor <- Wi(as.numeric(means[2,1:2]), prior[2])

w0.virginica <- W0(as.numeric(means[3,1:2]), prior[3])
w.virginica <- Wi(as.numeric(means[3,1:2]), prior[3])

w.versicolor-w.setosa
w0.versicolor-w0.setosa
w.versicolor-w.virginica
w0.versicolor-w0.virginica
w.virginica-w.setosa
w0.virginica-w0.setosa

# Using discriminant functions to predict the species from the original data 
disc_func_val <- data.frame(d.setosa,d.versicolor,d.virginica)
y_true <- data.frame(Sepal.Length=iris$Sepal.Length, 
                     Sepal.Width=iris$Sepal.Width, 
                     Species=iris$Species)

y_predict <- data.frame(Sepal.Length=iris$Sepal.Length, 
                        Sepal.Width=iris$Sepal.Width)
for (i in 1:150) {
  idx <- which(disc_func_val[i,] == max(disc_func_val[i,]))
  temp_species <- '' 
  if(idx == 1) 
    temp_species <- 'setosa'
  else if (idx  == 2)
    temp_species <- 'versicolor'
  else if (idx == 3)
    temp_species <- 'virginica'
  else 
    temp_species <- 'ERROR C:'
  
  y_predict$Species[i] <- temp_species
}

# plotting scatterplot of Sepal Length versus Sepal Width with color = predicted Species 
ggplot(data = y_predict, mapping = aes(x = Sepal.Width, y = Sepal.Length)) + 
  geom_point(aes(color = Species), size = 3, alpha = 0.6) +
  xlab("Sepal Width") +
  ylab("Sepal Length")

Missclass_rate <- function(true, pred) {
  cm <- table(true$Species, as.factor(pred))
  print(cm)
  return(1-sum(diag(cm))/sum(cm))
}

# try the LDA analysis with R's lda() function 
MR <- Missclass_rate(y_true, y_predict$Species)
LDA <- lda( Species ~ ., y_true)
lda_predict <- predict(LDA)
MR_LDA <- Missclass_rate(y_true, lda_predict$class)
# -> similar results! so what I did seems correct


# Trying to generate new data of this kind with the same total number of cases 
# as in the original data (using mean and variance from above)
generate_data <- function(n) {
  set.seed(12345)
  samples <- sample(x = c(levels(iris$Species)), size = n, prob = prior, replace = TRUE)
  
  n.set <- sum(samples == 'setosa')
  n.ver <- sum(samples == 'versicolor')
  n.vir <- sum(samples == 'virginica')
  
  set <- rmvnorm(n = n.set, mean = as.numeric(means[1,1:2]), sigma = iris.cov.setosa)
  ver <- rmvnorm(n = n.ver, mean = as.numeric(means[2,1:2]), sigma = iris.cov.versicolor)
  vir <- rmvnorm(n = n.vir, mean = as.numeric(means[2,1:2]), sigma = iris.cov.virginica)
  
  spec <- c(rep('setosa', n.set), rep('versicolor', n.ver), rep('virginica', n.vir))
  
  data <- data.frame(Sepal.Length = c(set[,1], ver[,1], vir[,1]),
                     Sepal.Width = c(set[,2], ver[,2], vir[,2]), 
                     Species = spec)
  return(data)
}

new_data <- generate_data(150)
#print(new_data)

# plotting a scatterplot of the same kind as above but for the new data 
ggplot(data = new_data, mapping = aes(x = Sepal.Width, y = Sepal.Length)) + 
  geom_point(aes(color = Species), size = 3, alpha = 0.6) +
  xlab("Sepal Width") +
  ylab("Sepal Length")
# -> generated data looks very similar to original data!


# Testing classification by logistic regression (using function
# multinom() from nnet package)
irisnew <- iris[, c("Sepal.Length", "Sepal.Width")]

irislogReg <- multinom(Species~Sepal.Length+Sepal.Width , iris )

irislogRegPredict <- predict(irislogReg,irisnew,type = "class")

iris.pred.df <- data.frame(irisnew, "pred.Species" = irislogRegPredict)
MR_logreg <- Missclass_rate(y_true, iris.pred.df$pred.Species)
#MR_logreg
## -> actually better than LDA!!

# plotting the classified data 
ggplot(data = iris.pred.df, mapping = aes(x = Sepal.Width, y = Sepal.Length)) + 
  geom_point(aes(color = pred.Species), size = 3, alpha = 0.6) +
  xlab("Sepal Width") +
  ylab("Sepal Length")
