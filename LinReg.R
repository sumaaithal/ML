# import the data
dataset = read.csv("Salary_Data.csv")

#install packages
#install.packages("caTools")
#install.packages("ggplot2")

library(caTools)
library(ggplot2)

set.seed(42)

#split the data
split = sample.split(dataset$Salary,SplitRatio = 2/3)
train_split = subset(dataset,split==TRUE )
test_split = subset(dataset,split==FALSE)

regressor = lm(formula = Salary~YearsExperience,
               data = train_split)

y_pred = predict(regressor,newdata = test_split)

y_pred

#plotting the data
ggplot() +
  geom_point(aes(x=train_split$YearsExperience,
                 y=train_split$Salary),
             color='red')+
  geom_line(aes(x=train_split$YearsExperience, 
                y=predict(regressor,newdata = train_split)),
            color='blue')+
  ggtitle("salary vs experience(train)") +
  xlab("experience")+
  ylab("salary")

ggplot() +
  geom_point(aes(x=test_split$YearsExperience,
                 y=test_split$Salary),
             color='red')+
  geom_line(aes(x=train_split$YearsExperience, 
                y=predict(regressor,newdata = train_split)),
            color='blue')+
  ggtitle("salary vs experience(test)") +
  xlab("experience")+
  ylab("salary")
