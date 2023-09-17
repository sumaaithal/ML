# Classification template

# Importing the dataset
dataset = read.csv('Churn_Modelling.csv')
dataset = dataset[4:14]

# Encoding categorical variables as factor
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c("France","Spain","Germany"),
                                      labels  = c(1,2,3)))

dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c("Male","Female"),
                                   labels  = c(2,1)))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

##fitting xgboost to the training set
#install.packages("xgboost")
library(xgboost)

classifier = xgboost(data = as.matrix(train_set[-11]), 
                     label = train_set$Exited,
                    nrounds = 10)
train_split
library(caret)

folds = createFolds(train_set$Exited ,k=10)

cv = lapply(folds,function(x){
  training_fold = train_set[-x,]
  test_fold = train_set[x,]
  classifier = xgboost(as.matrix(train_set[-11]) , data=training_fold, 
                   type='C-classification',
                   kernel='radial')
  y_pred = predict(classifier,newdata = as.matrix(test_fold[-11]))
  
  y_pred = (y_pred > 0.5)
  cm = table(test_fold[,11],y_pred)
  
  accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
  return(accuracy)
})

accuracy = mean(as.numeric(cv))
