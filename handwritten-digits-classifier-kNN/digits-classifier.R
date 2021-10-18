library(kknn)

# importing the data into R and dividing it into 
# training, validation and test sets (50%/25%/25%)
data = read.csv("optdigits.csv", header = FALSE)
n=dim(data)[1]

set.seed(12345) 
id=sample(1:n, floor(n*0.50)) 
train=data[id,] 
rest=data[-id,] 
n=dim(rest)[1]

set.seed(12345) 
id=sample(1:n, floor(n*0.50)) 
valid=rest[id,]
test=rest[-id,]

# as a base line, nearest neighbor classifier with k = 30 
# kernel=”rectangular” is the default?
data.kknn = kknn(as.factor(V65) ~ ., train, test, k=30, kernel="rectangular")

# confusion matrix
data.cm = table(data.kknn$fitted.values, test$V65 , dnn =c('p','t'))
print(data.cm)

# missclassification rate
data.mr = 1-sum(diag(data.cm))/sum(data.cm)
print(data.mr)

# also check train - train case
data.kknn.train = kknn(as.factor(V65) ~ ., train, train, k=30, kernel="rectangular")
data.cm.train = table(data.kknn.train$fitted.values, train$V65 , dnn =c('p','t'))
data.mr.train = 1-sum(diag(data.cm.train))/sum(data.cm.train)

# digit “8” seems hard to classifiy according to confusion matrix
# -> printing some examples that were easy & hard to classify 
# (i.e. having highest and lowest probabilities of the correct class)
prob = data.kknn.train$prob[,"8", drop="FALSE"]

min_idx = c()
min.v = 0
while (length(min_idx) < 3) {
  min.cond = min( prob[prob>min.v] )
  idx <- which(prob == min.cond)
  true_false <- data.kknn.train$fitted.values[idx] == 8
  if( any(true_false) )
    min_idx <- c(min_idx, idx[true_false])
  min.v = min.cond
}
paste('These are indexes with min values c:', min_idx[1:3])


max_idx = c()
max.v = 1.0
while (length(max_idx) < 2) {
  max.cond = max( prob[prob<=max.v] )
  idx <- which(prob == max.cond)
  true_false <- data.kknn.train$fitted.values[idx] == 8
  if( any(true_false) )
    max_idx <- c(max_idx, idx[true_false])
  max.v = max.cond
}

paste('These are indexes with max values c:', max_idx[1:2])

# print: reshaping features for each of these cases as matrix 8x8 and visualizing  
# the corresponding digits using heatmap() function 
train.m   <- as.matrix(train)

train.8x8.max1 <- train.m[max_idx[1],1:64]
train.8x8.max1 <- matrix(train.8x8.max1, nrow = 8, ncol=8, byrow = TRUE)
heat.max1 = heatmap(train.8x8.max1, Colv=NA, Rowv=NA)

train.8x8.max2 <- train.m[max_idx[2],1:64]
train.8x8.max2 <- matrix(train.8x8.max2, nrow = 8, ncol=8, byrow = TRUE)
heat.max2 = heatmap(train.8x8.max2, Colv=NA, Rowv=NA)

# hardly looks like an 8
train.8x8.min1 <- train.m[min_idx[1],1:64]
train.8x8.min1 <- matrix(train.8x8.min1, nrow = 8, ncol=8, byrow = TRUE)
heat.min1 = heatmap(train.8x8.min1, Colv=NA, Rowv=NA)

train.8x8.min2 <- train.m[min_idx[2],1:64]
train.8x8.min2 <- matrix(train.8x8.min2, nrow = 8, ncol=8, byrow = TRUE)
heat.min2 = heatmap(train.8x8.min2, Colv=NA, Rowv=NA)

train.8x8.min3 <- train.m[min_idx[3],1:64]
train.8x8.min3 <- matrix(train.8x8.min3, nrow = 8, ncol=8, byrow = TRUE)
heat.min3 = heatmap(train.8x8.min3, Colv=NA, Rowv=NA)

## cross-enropy(p, p_hat) = - Sum log(p_hat(p))
 # where p = true value, p_hat = probability of prediction
 # p_hat(p) = probability that kknn thinks prediction == true value
cross.entropy <- function(p, phat){
  x <- 0
  for (i in 1:length(p)){
    x <- x - log(as.numeric(phat[i,p[i]+1]+1e-15))
  }
  return(x)
}

# find optimal k (checking k from 1 to 30)
data.mr.train.vec = c()
data.mr.valid.vec = c()
cross.entropy.vec = c()
for(i in 1:30) { 
  data.kknn.train.i = kknn(as.factor(V65) ~ ., train, train, k=i, kernel="rectangular") 
  data.kknn.valid.i = kknn(as.factor(V65) ~ ., train, valid, k=i, kernel="rectangular") 
  data.cm.train.i = table(data.kknn.train.i$fitted.values, train$V65 , dnn =c('p','t'))
  data.cm.valid.i = table(data.kknn.valid.i$fitted.values, valid$V65 , dnn =c('p','t'))
  data.mr.train.vec <- c(data.mr.train.vec, 1-sum(diag(data.cm.train.i))/sum(data.cm.train.i))
  data.mr.valid.vec <- c(data.mr.valid.vec, 1-sum(diag(data.cm.valid.i))/sum(data.cm.valid.i))
  cross.entropy.vec <- c(cross.entropy.vec, cross.entropy(valid$V65, data.kknn.valid.i$prob))
}

plot(1:30, data.mr.valid.vec, xlab = 'k', ylab = 'Missclassification rate', ylim=c(0,0.06))
legend(0.2, 0.06, legend=c("Validation set", "Training set"), col=c("black", "red"), lty=1:2, cex=0.8)
points(1:30, data.mr.train.vec, col="red") 

plot(cross.entropy.vec, xlab = 'k-value', ylab = 'Cross-entropy')
paste('Optimal k-value for k =', which.min(cross.entropy.vec))

## k=3, k=4 seems to be the best, according to cross-entropy k=6 is best

test.predict.3 = kknn(as.factor(V65) ~ ., train, test, k=3, kernel="rectangular") 
data.cm.test.3 = table(test.predict.3$fitted.values, test$V65 , dnn =c('p','t'))
data.mr.test.3 = 1-sum(diag(data.cm.test.3))/sum(data.cm.test.3)

test.predict.4 = kknn(as.factor(V65) ~ ., train, test, k=4, kernel="rectangular") 
data.cm.test.4 = table(test.predict.4$fitted.values, test$V65 , dnn =c('p','t'))
data.mr.test.4 = 1-sum(diag(data.cm.test.4))/sum(data.cm.test.4)

# Missclassification rate for optimal k for training, validation and test set:
paste('Training data MR: k = 3: ', round(data.mr.train.vec[3], 4), ', k = 4:', round(data.mr.train.vec[4], 4))
paste('Validation data MR: k = 3: ', round(data.mr.valid.vec[3], 4), ', k = 4:', round(data.mr.valid.vec[4], 4))
paste('Test data MR: k = 3: ', round(data.mr.test.3, 4), ', k = 4:', round(data.mr.test.4, 4))
