## PCA

# Importing the dataset
dataset = read.csv('Wine.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
library(caret)
library(e1071)
library(MASS)

set.seed(123)
split = sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[,-14] = scale(training_set[,-14])
test_set[,-14] = scale(test_set[,-14])

##applying LDA
lda = lda(Customer_Segment~.,data = training_set)

#lda on training set
training_set = as.data.frame(predict(lda,training_set))

#set the right order for columns and exclude posterier cols
training_set = training_set[c(5,6,1)]

test_set = as.data.frame(predict(lda, test_set))

#put the dependent var to last column
test_set = test_set[c(5,6,1)]

##svm 

classifier = svm(formula = class ~ ., 
                 data=training_set, 
                 type='C-classification',
                 kernel='radial')

y_pred = predict(classifier,newdata = test_set[-3])

#built confusion matrix
cm = table(test_set[,3],y_pred)

cm

#visualizing the data

library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid==2,'deepskyblue', ifelse(y_grid == 1, 'dodgerblue', 'salmon')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2,'blue3',ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3')))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3],
     main = 'SVM (Test set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid==2,'deepskyblue', ifelse(y_grid == 1, 'dodgerblue', 'salmon')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2,'blue3',ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3')))
