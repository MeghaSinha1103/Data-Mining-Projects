set.seed(1) 
x=matrix(rnorm (20*2), ncol=2) 
y=c(rep(-1,10), rep(1,10)) 
x[y==1,]=x[y==1,] + 1

ployt(x, col=(3-y))


dat=data.frame(x=x, y=as.factor(y)) 
 library (e1071) 

svmfit=svm(y~., data=dat , kernel ="linear", cost=10, scale=FALSE)
plot(svmfit , dat)
summary(svmfit)


set.seed(1) 
  tune.out=tune(svm ,y~.,data=dat,kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100) ))
summary(tune.out)