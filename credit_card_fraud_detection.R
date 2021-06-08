install.packages('ranger')
install.packages("caret")
install.packages("data.table")

#error metrics -- Confusion Matrix
#will calculate recall here on the basis of problem statement

error_metric=function(CM)
{
  
  TN =CM[1,1]
  TP =CM[2,2]
  FP =CM[1,2]
  FN =CM[2,1]
  recall =(TP)/(TP+FN)
  accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
  print(paste("Recall value of the model: ",round(recall,2)))
  print(paste("Accuracy of the model: ",round(accuracy_model,2)))
  
  
}


library(ranger) #for random forest
library(caret)#short for Classification And REgression Training
library(data.table)

#reading dataset

creditcard_data <- read.csv("C:/Users/hp/Desktop/creditcard.csv")

#dimension of dataset

dim(creditcard_data)

#first 6 values of dataset

head(creditcard_data,6)

#analysing dataset

table(creditcard_data$Class)
summary(creditcard_data$Amount)
names(creditcard_data)
var(creditcard_data$Amount)


sd(creditcard_data$Amount)

#doing standard scaling of amount column

creditcard_data$Amount=scale(creditcard_data$Amount)

#delete the first column of dataset i.e. time

NewData=creditcard_data[,-c(1)]
head(NewData)

#spilitng into test train set

install.packages("caTools")
library(caTools)
set.seed(123)
data_sample = sample.split(NewData$Class,SplitRatio=0.80)
print(data_sample)
train_data = subset(NewData,data_sample==TRUE)
test_data = subset(NewData,data_sample==FALSE)
dim(train_data)
dim(test_data)

#building logistic regression model

Logistic_Model=glm(Class~.,train_data,family=binomial())
summary(Logistic_Model)
# https://rpubs.com/Amrabdelhamed611/669768#:~:text=Residuals%20vs%20Leverage%3A%20plots%20cooks,influence%20on%20the%20regression%20model.
plot(Logistic_Model)

#evaluating model

logit_predict = predict(Logistic_Model , test_data[-30] ,type = 'response' )
logit_predict <- ifelse(logit_predict > 0.5,1,0) # Probability check
CM= table(test_data[,30] , logit_predict)

error_metric(CM)
#recall=0.58

#drawing roc curve

library(pROC)
lr.predict <- predict(Logistic_Model,train_data)
auc.gbm = roc(train_data$Class, lr.predict, plot = TRUE, col = "blue")

#building decisiontree

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(Class ~ . , train_data, method = 'class')

# predicting values for test set

predicted_val <- predict(decisionTree_model, test_data[-30], type = 'class')

#evluating model

CM= table(test_data[,30] , predicted_val)
error_metric(CM)
# recall=0.79

#plotting tree

rpart.plot(decisionTree_model)

#building ANN

install.packages("neuralnet")
library(neuralnet)
ANN_model =neuralnet (Class~.,train_data,linear.output=FALSE)
plot(ANN_model)
predANN=compute(ANN_model,test_data[-30])
resultANN=predANN$net.result
resultANN=ifelse(resultANN>0.5,1,0)

#evaluating ANN
CM= table(test_data[,30] , resultANN)
error_metric(CM)
#recall=0.77