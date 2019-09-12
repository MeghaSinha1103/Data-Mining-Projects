install.packages("trees")

library(treeClust)
library(ISLR)

dataset_Train = read.csv('carseatsTrain.csv')
dataset_Test = read.csv('carseatsTest.csv')
attach(dataset_Train)
attach(dataset_Test)



High = ifelse(Sales<=8, "No", "Yes")
Carseats1 = data.frame(dataset_Train,High)

tree.carseats=tree(High~.-Sales,Carseats1)
summary(tree.carseats)


plot(tree.carseats)
text(tree.carseats, pretty=0)



High2= ifelse(dataset_Test$Sales<=8, "No", "Yes")
Carseats2 = data.frame(dataset_Test,High2)


tree.pred = predict(tree.carseats, Carseats2, type="class")
table(tree.pred, High2)


install.packages("tree")
library(trees)

set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
cv.carseats


#################### Using SVM for classification #########################
dataset_Train = read.csv('BankTrain.csv')
plot(dataset_Train$x1,dataset_Train$x2,col=dataset_Train$y+2,xlab="x1",ylab="x2",main="Bank Notes", 
     pch=c(dataset_Train$y+1))
+abline(intercept,slope)
legend("bottomright",
       legend = c("forged", "genuine"),
       col = c("green","red"),
       pch=c(2,1),
       horiz = FALSE)

########################################
install.packages("e1071")
library (e1071)


dataset_Train = read.csv('BankTrain.csv')
dataset_Test = read.csv('BankTest.csv',na.strings = "?")
fix(dataset_Test)
# na.omit(dataset_Train)
# na.omit(dataset_Test)
# dataset_Train

dat=data.frame(x1=dataset_Train$x1,x2=dataset_Train$x2, y=as.factor(dataset_Train$y)) 


svmfit=svm(y~x1+x2., data=dat , kernel ="linear", cost=10, scale=FALSE)
plot(svmfit , dat)
ypred=predict(svmfit, dataset_Test) 
data.frame(ypred)
summary(svmfit)

tune.out=tune(svm ,dat$y~dat$x1+dat$x2,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100) ))

summary(tune.out)

bestmod =tune.out$best.model
summary (bestmod)

testdata =data.frame(x1=dataset_Test$x1,x2=dataset_Test$x2, y=as.factor(dataset_Test$y))

ypred=predict(tune.out$best.model, dataset_Test)  
data.frame(ypred)

# names(ypred) <- NULL
# svm.pred<-rep(0,960)
# svm.pred[testdata$y == 1]<-1
# svm.pred

# table(predict = ypred , truth = testdata$y )
data.frame(ypred)
table(ypred,dataset_Test)
mean(ypred==svm.pred)



#############Question 5 radial kernel##############

dat=data.frame(x1=dataset_Train$x1,x2=dataset_Train$x2, y=as.factor(dataset_Train$y)) 

svmfit=svm(dat$y~., data=dat, kernel="radial", gamma=1, cost=1) 
plot(svmfit , dat)

summary (svmfit) 

# svmfit=svm(dat$y~., data=dat, kernel="radial",gamma=1, cost=1)
# plot(svmfit ,dat)

tune.out=tune(svm , dat$y~dat$x1+dat$x2, data=dat, kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4))) 
summary (tune.out) 

# table(true=dat[-train ,"y"], pred=predict (tune.out$best .model, newx=dat[-train,]))

bestmod =tune.out$best.model
summary (bestmod)
ypred=predict (bestmod, testdata) 


names(ypred) <- NULL
svm.pred<-rep(0,960)
svm.pred[testdata$y == 1]<-1
svm.pred


table(predict= ypred,truth=svm.pred)
mean(ypred==svm.pred)

###########################
svmfit=svm(dat$y~., data=dat, kernel="radial", gamma=2, cost=1) 
ypred=predict (svmfit, testdata) 
names(ypred) <- NULL
svm.pred<-rep(0,960)
svm.pred[testdata$y == 1]<-1
svm.pred
table(predict = ypred,truth=svm.pred)










