# import the data
dataset = read.csv("Social_Network_Ads.csv")

library(caTools)
library(ggplot2)
library(randomForest)
set.seed(42)

#encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased,levels = c(0,1))

#split the data
split = sample.split(dataset$Purchased,SplitRatio = 0.75)
train_split = subset(dataset,split==TRUE )
test_split = subset(dataset,split==FALSE)

#feature scaling
train_split[-3] = scale(train_split[-3])
test_split[-3] = scale(test_split[-3])

## fitting log reg to training set
classifier = randomForest(x = train_split[-3],
                          y=train_split$Purchased,
                          ntree=10)

y_pred = predict(classifier,newdata = test_split[-3])

#built confusion matrix
cm = table(test_split[,3],y_pred)

cm

#visualizing the data

library(ElemStatLearn)
set = train_split
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3],
     main = 'Random Forest (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_split
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3],
     main = 'Random Forest (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))
