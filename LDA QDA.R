library(boot)
library(ISLR)
library('Rcpp')
library(ggplot2)
library('caret')
library(MASS)
#> You may also find it useful to restart R,
#> In RStudio, that's the menu Session >> Restart R



#Training dataset is loaded
dataset_train = read.csv('BankTrain.csv')
dataset_test = read.csv('BankTest.csv')

#Fit and print summary of model
glm.fit=glm(y~x1+x2, data=dataset_train, family=binomial)

summary(glm.fit)

glm.probablities=predict(glm.fit, dataset_test, type="response")

##based on the summary, both predictors are useful as they have P values less then 0.05.
##this signels a relationship for both predicters and the response variable.
##both predictors have negative Coefficients meaning there is a negative relationship between the predictors
##and the probablity that the bank note has been forged.



########################################

set.seed(2)

boot.fn=function(data,index){
  return(coef(glm(y~x1+x2, data=data,subset=index, family = "binomial")))
}

boot(data = dataset_train, statistic = boot.fn, R = 1000)




########################################################



s = -(coef(glm.fit)[2]/coef(glm.fit)[3])
i = -(coef(glm.fit)[1])/coef(glm.fit)[3] 

#turn to catogrical variable so as to plot on graph
dataset_train$y <- as.factor(dataset_train$y)

ggplot(dataset_train, aes(x=x1, y=x2, group=y)) +
  geom_point(aes(shape=y, color=y)) + geom_abline(aes(intercept =i , slope= s)) +
  ggtitle("Classifying Forged and Genuine Bank Notes") 



########################################################


#testing data decision boundary at 50%
glm.probablities=predict(glm.fit, dataset_test,type = "response" )
head(glm.probablities)

glm.predtest=rep("0",412)
glm.predtest[glm.probablities>.5]="1" 

table(glm.predtest,dataset_test$y)
mean(glm.predtest==dataset_test$y)


specificity(table(glm.predtest,dataset_test$y))
sensitivity(table(glm.predtest,dataset_test$y))



##total accuracy for this model is 88.5%
##False postives(Specificity) for this model is 20/236 or 0.084%. This means the model is incorrectly identify 0.084% notes as forged when they actually genuine 
##True postive(Sensitivity) is 149/176 OR 84.65%. This means that 84.65% of forged bank notes are correctly classified.


############################################################


glm.predtest[glm.probablities>.2]="1"

table(glm.predtest,dataset_test$y)
mean(glm.predtest==dataset_test$y)
specificity(table(glm.predtest,dataset_test$y))
sensitivity(table(glm.predtest,dataset_test$y))

##total accuracy for this model is 87.8%
##False postives(Specificity) for this model is 29/236 or 0.122%. This means the model is incorrectly identify 0.122% notes as forged when they are actually genuine. This error rate has increasedin comparison to the last decision boundary 
##True postive(Sensitivity) is 155/176 OR 88.06%. This means that 88.06% of forged bank notes are correctly classified as such. This is an increase of nearly 4% in correctly identifing forged notes.
##if a bank was trying to identify forged money so as to take it out of the market, it may be better to reduce the decision boundary to 40%. because even with an increase in overall error and increase in type 1 error.
##(incorrectly classifying geniune bank notes as forged) this model picks up forged notes more accuratly then with a decision boundary at 50%. 



##########################################

#Preparing LDA
lda.fit=lda(y~x1+x2, data=dataset_train)

#Predicting
lda.pred=predict(lda.fit, dataset_test)
lda.class=lda.pred$class[1:5]

#Results
table(lda.class,dataset_test$y)
mean(lda.class==dataset_test$y)
specificity(table(lda.class,dataset_test$y))
sensitivity(table(lda.class,dataset_test$y))


###############################################


#Preparing QDA
qda.fit=qda(y~x1+x2,data=dataset_train)
qda.pred = predict(qda.fit,dataset_test)
qda.class=predict(qda.fit,dataset_test)$class

#Results
table(qda.class,dataset_test$y)
mean(qda.class==dataset_test$y)
specificity(table(qda.class,dataset_test$y))
sensitivity(table(qda.class,dataset_test$y))



###################################################

##LDA has a overall accuracy rate of 87%, Specificity 20/236 (0.084%) and senstivity is at 146/176 (82.95%).
##QDA accuracy is at 89%, Specificity 17/236 (0.072%) and senstivity is at 148/176 (84.09%).
##I would choose the logistic regression for this problem. becuase even though it is overclassify genuine bank notes as fake
##it does a better job at identifying forged notes.












