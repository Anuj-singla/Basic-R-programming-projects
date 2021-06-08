white_wine<-read.csv("C:/Users/hp/Downloads/winequality-white.csv",sep=";")
red_wine<-read.csv("C:/Users/hp/Downloads/winequality-red.csv",sep=";")

View(white_wine)
head(white_wine)

View(red_wine)
head(red_wine)

#merging to dataframes

wine<-rbind(white_wine,red_wine)
dim(wine)
head(wine)

summary(wine)

#checking null values

sum(is.na(wine$fixed.acidity))
sum(is.na(wine$volatile.acidity))
sum(is.na(wine$citric.acid))
sum(is.na(wine$residual.sugar))
sum(is.na(wine$chlorides))
sum(is.na(wine$free.sulfur.dioxide))
sum(is.na(wine$total.sulfur.dioxide))
sum(is.na(wine$density))
sum(is.na(wine$pH))
sum(is.na(wine$sulphates))
sum(is.na(wine$alcohol))
sum(is.na(wine$quality ))

# so we can see there are no null values in our dataset

#VISUALIZING DATA

barplot(table(wine$quality))


# counting good bad normal samples and adding its column to wine dataframe
##Transforming Quality from an Integer to a Factor

wine$taste<-ifelse(wine$quality<6,'bad','good')
wine$taste[wine$quality==6]<-'normal'
wine$taste<-as.factor(wine$taste)
table(wine$taste)


barplot(table(wine$taste),col="red")

#pie chart
a=table(wine$taste)
pct=round(a/sum(a)*100)
lbs=paste(c("bad","good","normal")," ",pct,"%",sep = " ")
library(plotrix)
pie(a,labels=lbs,main="pie chart!")

#finding correlation between variables and visualizing it

corr<-cor(wine[,-13])

library(ggplot2)
library(reshape2)

melted_cormat <- melt(corr)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile()


install.packages('e1071')
library(mlbench)
library(caret)
highlyCorrelated <- findCorrelation(corr, cutoff=0.5)
print(highlyCorrelated)

library(e1071)
control <- trainControl(method="repeatedcv", number=10, repeats=3)

#train is used for parameter tuning

model <- train(taste~., data=wine, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)
#every feature have importance more than 0.5 so will not delete any feature

#pair plot

pairs(wine)

#scaling of the data
scaled=scale(wine[,-13])
final_scaled=cbind(scaled[,-12],wine[13])

#handeling imbalance dataset
good=subset(final_scaled, taste == "good")
final_scaled=rbind(final_scaled,good)
bad=subset(final_scaled, taste == "bad")
set.seed(123)
samp<-sample(450)
bad<-bad[samp,]
final_scaled=rbind(final_scaled,bad)
a=table(final_scaled$taste)
pct=round(a/sum(a)*100)
lbs=paste(c("bad","good","normal")," ",pct,"%",sep = " ")
library(plotrix)
pie(a,labels=lbs,main="pie chart!")

barplot(table(final_scaled$taste),col="purple")

#diving our dataset to test and train

set.seed(123)
samp<-sample(nrow(final_scaled),0.7*nrow(final_scaled))
train<-final_scaled[samp,]
test<-final_scaled[-samp,]

dim(train)
dim(test)


#building different models and evaluating them

error_metric=function(CM)
{
  
  T3 =CM[1,1]
  T2 =CM[2,2]
  T1 =CM[3,3]
  den=CM[1,2]+CM[1,3]+CM[2,1]+CM[2,3]+CM[3,1]+CM[3,2]
  accuracy_model  =(T1+T2+T3)/(den+T1+T2+T3)
  print(paste("Accuracy of the model: ",round(accuracy_model,2)))
  
  
}

#DECISION TREE

library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(taste ~ . , train, method = 'class')

# predicting values for test set

predicted_val <- predict(decisionTree_model, test[-12], type = 'class')

#evluating model

CM= table(test[,12] , predicted_val)
error_metric(CM)
#accuracy=0.56

rpart.plot(decisionTree_model)


#RANDOM FOREST
install.packages("randomForest")
library(randomForest)

model <- randomForest(taste ~ ., data = train, ntree = 500)
pred1<-predict(model,test[-12])
CM= table(test[,12] , pred1)
error_metric(CM)
# accuracy=0.79


#KNN 
train_knn<-train[,-12]
train_label<-train[,12]

test_knn<-test[,-12]
test_label<-test[,12]

install.packages("class")
library(class)
accuracy=0

#Finding best value of k according to accuracy for knn algorithm

for (k in 1:25) {
  knn.25<-knn(train=train_knn,test=test_knn,cl=train_label,k=k)
  CM= table(test[,12] , knn.25)
  T3 =CM[1,1]
  T2 =CM[2,2]
  T1 =CM[3,3]
  den=CM[1,2]+CM[1,3]+CM[2,1]+CM[2,3]+CM[3,1]+CM[3,2]
  accuracy_model  =(T1+T2+T3)/(den+T1+T2+T3)
  accuracy=append(accuracy,accuracy_model)
}
plot(1:25,accuracy[-1],type = "o", col = "red", xlab = "number of iteration", ylab = "accuracy",
     main = "elbow method to determine value of K")

#best accuracy comes at k=1 with accuracy as 75%


knn.1<-knn(train=train_knn,test=test_knn,cl=train_label,k=1)
CM= table(test[,12] , knn.1)
print(CM)
error_metric(CM)

#NAIVE BAYES

library(e1071)
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(taste ~ .,train)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl,test[-12])
CM= table(test[,12] , y_pred)
error_metric(CM)






