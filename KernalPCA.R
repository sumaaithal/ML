# import the data
dataset = read.csv("Social_Network_Ads.csv")

library(caTools)
library(ggplot2)

set.seed(42)

#split the data
split = sample.split(dataset$Purchased,SplitRatio = 0.75)
train_split = subset(dataset,split==TRUE )
test_split = subset(dataset,split==FALSE)

#feature scaling
train_split[,1:2] = scale(train_split[,1:2])
test_split[,1:2] = scale(test_split[,1:2])

#applying Kernal PCA
#install.packages('kernlab')
library(kernlab)

kpca = kpca(~. , data = train_split[-3], kernel='rbfdot',
            features=2)

training_split_pca = as.data.frame(predict(kpca, train_split))
training_split_pca$Purchased = train_split$Purchased

test_split_pca = as.data.frame(predict(kpca, test_split))
test_split_pca$Purchased = test_split$Purchased


## fitting log reg to training set
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data =  training_split_pca)

## predict on test set
prob_pred = predict(classifier,
                    type='response',
                    newdata = test_split_pca[-3])
prob_pred

y_pred = ifelse(prob_pred > 0.5,1,0)

y_pred

#built confusion matrix
cm = table(test_split[,3],y_pred)

cm

#visualizing the data

library(ElemStatLearn)
set = training_split_pca
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('V1', 'V2')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_split_pca
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('V1', 'V2')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))
