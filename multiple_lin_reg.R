# import the data
dataset = read.csv("50_Startups.csv")

#install packages
#install.packages("caTools")
#install.packages("ggplot2")

#categorical encoding
dataset$State = factor(dataset$State,
                       levels = c('New York','California','Florida'),
                       labels = c(1,2,3))

library(caTools)
library(ggplot2)

set.seed(42)

#split the data
split = sample.split(dataset$Profit,SplitRatio = 2/3)
train_split = subset(dataset,split==TRUE )
test_split = subset(dataset,split==FALSE)

#regression
reg = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
         data = train_split)

#reg = lm(formula = Profit ~ . ,data = dataset)

#predicting test set result

y_pred = predict(reg,newdata =  test_split)

y_pred

#backward eliminition

reg = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
         data = dataset)

summary(reg)

reg = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
         data = dataset)

summary(reg)

reg = lm(formula = Profit ~ R.D.Spend + Administration,
         data = dataset)

summary(reg)

reg = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
         data = dataset)

summary(reg)
