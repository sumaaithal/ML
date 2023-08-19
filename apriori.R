#import dataset

dataset = read.csv("Market_Basket_Optimisation.csv",header = FALSE)

#install.packages("arules")

library(arules)

dataset = read.transactions("Market_Basket_Optimisation.csv",sep = ',',
                            rm.duplicates = TRUE)
summary(dataset)

itemFrequencyPlot(dataset,topN=10)

##training apriori to dataset

rules = apriori(data = dataset,
                parameter = list(support = 0.004, 
                                 confidence=0.2))
## visualize the results
inspect(sort(rules, by='lift')[1:10])






