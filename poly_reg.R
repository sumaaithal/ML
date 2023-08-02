# import the data
dataset = read.csv("Position_Salaries.csv")
dataset = dataset[2:3]
dataset

#install packages
#install.packages("caTools")
#install.packages("ggplot2")

library(caTools)
library(ggplot2)

set.seed(42)

#fitting linear regression
lin_reg = lm(formula = Salary ~ Level, data = dataset)

#new level
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4

#fitting polynomial regression
poly_reg = lm(formula = Salary ~ ., 
              data = dataset)

#visualize linear regression
ggplot()+
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             color='red')+
  geom_line(aes(x=dataset$Level, y=predict(lin_reg,newdata = dataset)), 
            color='blue')+
  ggtitle("Level Vs Salary(Linear Regression)")+
  xlab("Level")+
  ylab("Salary")

#visualize polynomial regression
ggplot()+
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             color='red')+
  geom_line(aes(x=dataset$Level, y=predict(poly_reg,newdata = dataset)), 
            color='blue')+
  ggtitle("Level Vs Salary(Linear Regression)")+
  xlab("Level")+
  ylab("Salary")


#visualize polynomial regression
x_grid = seq(min(dataset$Level),max(dataset$Level),
             0.1)
ggplot()+
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             color='red')+
  geom_line(aes(x=x_grid, y=predict(poly_reg,
                                    newdata = data.frame(Level=x_grid))), 
            color='blue')+
  ggtitle("Level Vs Salary(Linear Regression)")+
  xlab("Level")+
  ylab("Salary")

## predict using linear regression
y_pred = predict(lin_reg,data.frame(Level=6.5))
y_pred

## predict using polynomial regression
y_pred = predict(poly_reg,data.frame(Level=6.5,
                                     Level2=6.5^2,
                                     Level3=6.5^3,
                                     Level4=6.5^4))
y_pred
