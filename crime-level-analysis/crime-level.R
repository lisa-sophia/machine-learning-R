data = read.table("communities.csv",sep=",", header = TRUE)

set.seed(12345)

# column 101 is the target variable ViolentCrimesPerPop
data[, -c(101)] <- scale(data[, -c(101)])

# perform DIY PCA -> calculate eigenvalues & -vectors of data
S <- cov(data)
sum(diag(S))
s.eigen <- eigen(S)

# nr. of features needed to obtain at least 95% of variance in the data
num_features <- 1
sum <- 0.0
while ( sum < 0.95 ) {
  sum <- sum + s.eigen$values[num_features] / sum(s.eigen$values)
  num_features <- num_features + 1
}

# proportion of variation explained by the first two principal components
(s.eigen$values[1]+s.eigen$values[2]) / sum(s.eigen$values)


# perform PCA analysis using R's princomp() function  
data.princomp <- princomp(data)
# score plot of the first principle component
plot(data.princomp$loadings[,1],
     main='Score plot',
     xlab ='Obs.',
     ylab ='Comp1',
     col='black',
     pch=20)

# find which 5 features contribute the most (by the absolute value) to the 
# first principle component.
sort(abs(data.princomp$loadings[,1]), decreasing=TRUE)[1:5]
# medFamInc, medIncome, PctKids2Par,pctWInvInc and PctPopUnderPov 
# (median family income, median household income, percentage of kids 
# in family housing with two parents, percentage of households with 
# investment / rent income in 1989 and percentage of people under the 
# poverty level) contribute the most -> makes sense, e.g. having a high income 
# (family or otherwise) lowers ones chances of wanting to commit crime to 
# acquire capital. Also kids raised in two parent households is another
# large component to whether a child turns to crime in the future.

# plotting the PC scores in the coordinates (PC1, PC2) with the points 
# colored by ViolentCrimesPerPop
df <- as.data.frame(data.princomp$scores)
df <- cbind(df, 'ViolentCrimesPerPop' = data$ViolentCrimesPerPop)

library(ggplot2)
ggplot(data =  df, color=ViolentCrimesPerPop,
       mapping = aes(x = Comp.1 , y = Comp.2)) 
+ geom_point(size = 1.5, alpha = 0.6)
+ xlab("PC1")
+ ylab("PC2")
# -> it seems that lower PC2 and higher PC1 indicate higher ViolentCrimesPerPop


# trying second order polynomial regression model to predict ViolentCrimesPerPop
# with only PC1 as the feature. 
poly.reg <- lm(df$ViolentCrimesPerPop ~ poly(df$Comp.1, 2))
pred.poly.reg <- predict(poly.reg)

# scatterplot of the target versus the feature + predicted values
ggplot(data =poly.reg, mapping = aes(x = df$Comp.1 ,y = df$ViolentCrimesPerPop)) 
+ geom_line(aes(y = pred.poly.reg, ), color='green', size=1 )
+ geom_point( size = 1, alpha = 0.6)
+ xlab("ViolentCrimesPerPop")
+ ylab("PC1") 
# -> not bad, but also not great

# parametric bootstrap to estimate the confidence and prediction bands
# from the polynomial regression model 
library(boot)

rng=function(d, mle) {
  data1=data.frame(ViolentCrimesPerPop=d$ViolentCrimesPerPop, Comp.1=d$Comp.1)
  n=length(d$ViolentCrimesPerPop)
  #generate new data c: 
  data1$ViolentCrimesPerPop=rnorm(n, predict(mle, newdata=data1), sd(mle$residuals))
  return(data1)
}

f1=function(data1){
  res= lm(data1$ViolentCrimesPerPop ~ poly(data1$Comp.1, 2)) 
  muchViolent=predict(res,newdata=df)
  return(muchViolent)
}

f2=function(data1){
  res=lm(data1$ViolentCrimesPerPop ~ poly(data1$Comp.1, 2))
  ViolentP=predict(res,newdata=df)
  n=length(df$ViolentCrimesPerPop)
  predictedP=rnorm(n, ViolentP,
                   sd(poly.reg$residuals))
  return(predictedP)
}
res=boot(df, statistic=f1, R=1000, mle=poly.reg , ran.gen=rng, sim="parametric")

brev <- envelope(res)
res2=boot(df, statistic=f2, R=10000, mle=poly.reg , ran.gen=rng, sim="parametric")
brev2 <- envelope(res2)

# add the confidence & prediction bands into the previous plot 
ggplot(data =poly.reg, mapping = aes(x = df$Comp.1 ,y = df$ViolentCrimesPerPop)) + 
  geom_line(aes(y = pred.poly.reg, ), color='green',size=1 ) + 
  geom_point( size = 1, alpha = 0.6) +
  geom_line(aes(y = brev$point[1,]), color='blue', size=1,)+
  geom_line(aes(y = brev$point[2,]), color='orange', size=1)+
  geom_line(aes(y = brev2$point[1,]), color='cyan', size=1)+
  geom_line(aes(y = brev2$point[2,]), color='red', size=1)+
  xlab("ViolentCrimesPerPop") + 
  ylab("PC1") 
# -> quite wide band, but now most point are contained!
