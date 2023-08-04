# import the data
dataset = read.csv("Position_Salaries.csv")
dataset = dataset[2:3]
dataset

#install packages
#install.packages("caTools")
#install.packages("ggplot2")
#install.packages("e1071")

library(caTools)
library(ggplot2)
library(e1071)

set.seed(42)

#fitting polynomial regression
reg = svm(formula = Salary ~ . , data=dataset, type='eps-regression')

#prediction 
y_pred = predict(reg,data.frame(Level=6.5))

#visualize linear regression
ggplot()+
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             color='red')+
  geom_line(aes(x=dataset$Level, y=predict(reg,newdata = dataset)), 
            color='blue')+
  ggtitle("Level Vs Salary(SVR)")+
  xlab("Level")+
  ylab("Salary")



#visualize polynomial regression
x_grid = seq(min(dataset$Level),max(dataset$Level),
             0.1)
ggplot()+
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             color='red')+
  geom_line(aes(x=x_grid, y=predict(reg,
                                    newdata = data.frame(Level=x_grid))), 
            color='blue')+
  ggtitle("Level Vs Salary(Linear Regression)")+
  xlab("Level")+
  ylab("Salary")
