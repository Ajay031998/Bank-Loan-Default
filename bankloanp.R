rm(list=ls(all=T))
x = c("ggplot2","corrgram","DMwR","caret","randomForest","unbalanced","c50","dummies","e1071","MASS",
      "rpart","gbm","ROSE","mlbench","mlr","caTools","rBayesianOptimization","pROC"
      ,"pdp","Matrix")
#INSTALL PACKAGES
lapply(x, require, character.only = TRUE)
rm(x)
setwd("C:/Users/HP/.jupyter")
## Read the data
data = read.csv("bank-loan.csv", header = T, na.strings = c(" ", "", "NA"))

str(data)

##################### Missing Values Analysis##############################
missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
data$default[is.na(data$default)] = "0"
sum(is.na(data))
unique(data$default)

###################### Feature Scaling################################


#Normalisation
cnames = c("age","ed","employ","address","income","debtinc","creddebt",
           "othdebt")

for(i in cnames){
  print(i)
  data[,i] = (data[,i] - min(data[,i]))/
    (max(data[,i] - min(data[,i])))
}


##Model Development
###################### Logistic Regression###########

dt = sort(sample(nrow(data), nrow(data)*.8))
train<-data[dt,]
test<-data[-dt,]

data$default = as.numeric(data$default)
model <- glm(default ~.,family=binomial(link='logit'),data=data)
summary(model)

fitted.results <- predict(model,test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$default)
print(paste('Accuracy',1-misClasificError))

# Logistic regression provides 78.23% accuracy.

##################### Decision tree classifier####################

library(rpart)

fit <- rpart(default~., data = train, method = 'class')
predict_unseen <-predict(fit, test, type = 'class')
table_mat <- table(test$default, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

#Decision tree provides 74.11% accuracy.

##################### KNN Implementation####################

library(class)

KNN_Predictions = knn(train[, 1:7], test[, 1:7], train$default, k = 5)

#Confusion matrix
Conf_matrix = table(KNN_Predictions, test$default)

# KNN Implemantation provides 72.94% accuracy.
