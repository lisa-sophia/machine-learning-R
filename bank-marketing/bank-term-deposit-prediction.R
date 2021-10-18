library(data.table)
library(tree)
library(rpart)
library(e1071)

# Import the data into R and divide it into 
# training, validation and test sets (40%/30%/30%)

data = read.table("bank-full.csv", sep=";",header = TRUE, stringsAsFactors = TRUE)
data = subset(data, select = -c(duration) )

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.4))
train=data[id,]

id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.3))
valid=data[id2,]

id3=setdiff(id1,id2)
test=data[id3,]

# Fit decision trees to the training data, try some different default 
# settings and compare

# a. Decision Tree with default settings.
fit.a <- tree(y ~., train) 

# b. Decision Tree with smallest allowed node size equal to 7000.
fit.b = tree(y ~., train,
            control = tree.control(length(train$y),minsize = 7000) )

# c. Decision trees minimum deviance to 0.0005.
fit.c = tree(y ~., train,
            control = tree.control(length(train$y),mindev = 0.0005) )

summary(fit.a)
summary(fit.b)
summary(fit.c)

# calculate missclassification rate:
Missclass_rate <- function(true, pred) {
  cm <- table(true, as.factor(pred))
  return(1-sum(diag(cm))/sum(cm))
}

pred.valid.a <- predict(fit.a, newdata=valid, type="class") 
pred.valid.b <- predict(fit.b, newdata=valid, type="class") 
pred.valid.c <- predict(fit.c, newdata=valid, type="class") 
mr.valid.a <- Missclass_rate(valid$y, pred.valid.a)
mr.valid.b <- Missclass_rate(valid$y, pred.valid.b)
mr.valid.c <- Missclass_rate(valid$y, pred.valid.c)
# -> model c seems to be the best!

# Use training and validation sets to choose the optimal tree depth in the 
# model c: study the trees up to 50 leaves. 
train.pred = predict(fit.c, newdata = train)
summary(train.pred)

valid.pred = predict(fit.c, newdata = valid)

trainScore=c()
testScore=c()
for(i in 2:50) {
  i
  prunedTree=prune.tree(fit.2c,best=i)
  pred=predict(prunedTree, newdata=valid,type="tree")
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}

# Plot dependence of deviances for the training and the validation 
# data on the number of leaves
plot(1:50, trainScore[1:50], col="red" ,ylim=c(8000,12000), type = "l",
     xlab = 'Nr. of leaves', ylab = 'Deviance')
legend(40, 12000, legend=c("Training set", "Validation set"), 
       col=c("red", "blue"), lty=1:1, cex=0.9)
lines(1:50, testScore[1:50], col="blue")

# -> optimal nr. of leaves: look for 'elbow': 5 seems to be best
prunedTree <- prune.tree(fit.c,best=5)
prunedTree.pred <- predict(prunedTree, newdata=test, type="class")
# confusion matrix and missclassification rate
prunedTree.cm <- table(test$y, as.factor(prunedTree.pred), dnn =c('t','p'))
prunedTree.mr <- Missclass_rate(test$y, prunedTree.pred)

# Perform a decision tree classification of the test data with a loss matrix 
# that is not only ones, e.g. increase top right element to penalize
# false negatives more ("trues" that were classified as "false")
# bottom left is false positive penalization
fit <- rpart(y ~ ., data=test, method="class", 
             parms=list(loss=matrix(c(0,5,1,0), nrow=2)))

pred <- predict(fit, newdata=test, type="class") 
mr.pred <- Missclass_rate(test$y, pred.4)
cm.pred <- table(test$y, as.factor(pred.4), dnn =c('t','p'))

# use optimal (pruned) tree and a naive bayes model to classify the test data 
# by comparing the "confidence" / probability: 
# if probability of yes is > x { yes } otherwise {no} 
# and try different x-values, e.g. 0.1, 0.2, ..., 0.9

fit <- naiveBayes(y~., data=test)
pred.bayes <- predict(fit, newdata=test, type="raw") 
pred.opt <- predict(prunedTree, newdata=test) 

tpr <- c()
fpr <- c()
tpr.opt <- c()
fpr.opt <- c()
pi <- seq(0.05, 0.95, 0.05)
idx <- 1
for (i in pi) {
  result <- factor(c(), levels = c("no", "yes"))
  result.opt <- factor(c(), levels = c("no", "yes"))
  j <- 1
  while (j <= length(pred.bayes[,1])) {
    if (as.numeric(pred.bayes[j,2] > i)) {
      result[j] = "yes"
    } else {
      result[j] = "no"
    }
    if (as.numeric(pred.opt[j,2] > i)) {
      result.opt[j] = "yes"
    } else {
      result.opt[j] = "no"
    }
    j = j+1
  }
  # calculate confusion matrix, true positive rate and false positive rate
  cm <- table(test$y, result, dnn =c('t','p'))
  tpr[idx] <- cm[2,2] / (cm[2,2] + cm[2,1])
  fpr[idx] <- cm[1,2] / (cm[1,1] + cm[1,2])
  cm.opt <- table(test$y, result.opt, dnn =c('t','p'))
  tpr.opt[idx] <- cm.opt[2,2] / (cm.opt[2,2] + cm.opt[2,1])
  fpr.opt[idx] <- cm.opt[1,2] / (cm.opt[1,1] + cm.opt[1,2])
  idx = idx + 1
}

# plot tpr (true positive rate) over fpr (false pos. rate) for both models
plot(x = c(1,fpr,0), y = c(1,tpr,0), col="red", xlim = c(0,1), ylim = c(0,1), type = 'l',
     xlab = 'False Positive Rate', ylab = 'True Positive Rate')
lines(x = c(1,fpr.opt), y = c(1,tpr.opt), col = "blue")
lines(x=0:1, y=0:1, lty = "dashed")
legend(0.8, 0.15, legend=c("Naive Bayes", "Optimal Tree"), 
       col=c("red", "blue"), lty=1:1, cex=0.8)
