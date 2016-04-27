install.packages("randomForest")
library(randomForest)
rfNews()


# simulate the data
x1=rnorm(1000)
x2=rnorm(1000,x1,1)
y=2*x1+rnorm(1000,0,.5)
df=data.frame(y,x1,x2,x3=rnorm(1000),x4=rnorm(1000),x5=rnorm(1000))
# run the randomForest implementation
library(randomForest)
rf1 <- randomForest(y~., data=df, mtry=2, ntree=50, importance=TRUE)
importance(rf1,type=1)
# run the party implementation<br>l
ibrary(party)
cf1 <- cforest(y~.,data=df,control=cforest_unbiased(mtry=2,ntree=50))
varimp(cf1)
varimp(cf1,conditional=TRUE)

#########################
set.seed(1)
n=500
library(clusterGeneration)
#install.packages("mnormt")
library(mnormt)
S=genPositiveDefMat("eigen",dim=15)
S=genPositiveDefMat("unifcorrmat",dim=15)
X=rmnorm(n,varcov=S$Sigma)
#install.packages("corrplot")
library(corrplot)
corrplot(cor(X), order = "hclust")

# generate datasets 
P=exp(Score)/(1+exp(Score))
Y=rbinom(n,size=1,prob=P)
 df=data.frame(Y,X)
 allX=paste("X",1:ncol(X),sep="")
names(df)=c("Y",allX)



######################
#Setup a binary classification problem
require(randomForest)
data(iris)
set.seed(1)
dat <- iris
dat$Species <- factor(ifelse(dat$Species=='virginica',1,0))
trainrows <- runif(nrow(dat)) > 0.3
train <- dat[trainrows,]
test <- dat[!trainrows,]

#Build a decision tree
require(rpart)
model.rpart <- rpart(Species~., train)



arf<-randomForest(Species~.,data=train,importance=TRUE,proximity=TRUE,ntree=500, keep.forest=TRUE)
#plot variable importance
varImpPlot(arf)
importance(arf,type=1)

testp4<-predict(arf,test,type='prob')[,2]
pred4<-prediction(testp4,test$good_bad)
perf4 <- performance(pred4,"tpr","fpr")
#plotting logistic results vs. random forest ROC
#plotting logistic results vs. random forest ROC
plot(perf,col='red',lty=1, main='ROC Logistic Vs. RF');
plot(perf4, col='blue',lty=2,add=TRUE);
legend(0.6,0.6,c('simple','RF'),col=c('red','blue'),lwd=3)


