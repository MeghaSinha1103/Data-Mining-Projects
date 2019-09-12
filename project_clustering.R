library(e1071)

training_Data=read.csv("BankTrain.csv",header=TRUE,na.strings="?")
test_Data=read.csv("BankTest.csv",header=TRUE,na.strings="?")

dat=data.frame(x1=training_Data$x1,x2=training_Data$x2, y=as.factor(training_Data$y))
tune.out=tune(svm ,y~x1+x2,data=dat ,kernel ="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100) ))

summary(tune.out)
svmfit=svm(dat$y~., data=dat , kernel ="linear", cost=0.01,  scale=FALSE)

plot(svmfit,dat)



best_mod = tune.out$best.model
ypredict=predict(best_mod,test_Data)

table(predict =ypredict , truth=test_Data$y )



mean(ypredict == test_Data$y )

svmfit=svm(dat$y~., data=dat , kernel ="radial", cost=10,gamma=0.5,  scale=FALSE)

plot(svmfit,dat)
set.seed(1)
tune.out=tune(svm , y~x1+x2, data=dat, kernel ="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),
                          gamma=c(0.5,1,2,3,4) ))
summary(tune.out)
best_mod = tune.out$best.model
ypredict=predict(best_mod,test_Data)
table(predict =ypredict , truth=test_Data$y )



mean(ypredict == test_Data$y )


################Question 4
library(readr)
data <- read.csv("A3data2.csv", header = TRUE)
# Scaling the dataset
scale_data <- scale(data[-3])

# Plotting the clusters with different colours
plot(scale_data, col=(data$Cluster+2), 
     main="Available clusters in the data",
     xlab="x1", ylab="x2", 
     pch=20, cex=.85)

# k-means with K=3 and nstart = 3
kdata3 = kmeans(scale_data,3, nstart=3)
kdata3$tot.withinss



plot(scale_data, col=(kdata3$cluster+2), 
     main="K-Means Clustering for 3 clusters with nstart=3", 
     xlab="x1", ylab="x2", 
     pch=20, cex=.8)

# k-means with K=3 and nstart = 1
kdata1 = kmeans(scale_data,3, nstart=1)
kdata1$tot.withinss

# k-means with K=3 and nstart = 20
kdata20 = kmeans(scale_data,3, nstart=20)
kdata20$tot.withinss



# Clustering using Complete Linkage and Euclidean distance
h_complete = hclust(dist(scale_data), method="complete")
h_complete
plot(h_complete, col = 8, main="Complete Linkage for 3 clusters", xlab="", 
     sub="", cex=.9, labels = FALSE)

# Clustering using Single Linkage
h_single=hclust(dist(scale_data, method = "euclidean" ), method="single")
h_single
plot(h_single, col = 8, 
     main="Single Linkage for 3 clusters", xlab="", 
     sub="", cex=.9, labels = FALSE)

cutree(h_single, k=3)
plot(scale_data, col = cutree(h_single,k=3)+10,
     main="Single Linkage for 3 clusters", 
     xlab="x1", ylab="x2", sub="", cex=.9 )

plot(scale_data, col = cutree(h_complete,k=3)+10,
     main="Complete Linkage for 3 clusters", 
     xlab="x1", ylab="x2", sub="", cex=.9 )

  table(predicted=kdata3$cluster,actual= data$Cluster)
  
  table(cutree(h_single, k = 3), data$Cluster)
  
  # Comparing Hierarchical complete linkage clusters with actual clusters
  table(predicted = cutree(h_complete, k=3), actual = data$Cluster)
  
  
  # Comparing Hierarchical complete linkage clusters with actual clusters
  table(cutree(h_complete, k=3), data$Cluster)
  
  kmean_3 <- kdata3$cluster
  hc_compl_3 <- cutree(h_complete, k=3)
  hc_singl_3 <- cutree(h_single, k=3)
  
  table(kmean_3, hc_compl_3)
  
  table(kmean_3, hc_singl_3)
  
  
  table(hc_compl_3, hc_singl_3)
  
  
  
  
  
  
  
  





