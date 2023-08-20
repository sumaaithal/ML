#import dataset

dataset = read.csv("Market_Basket_Optimisation.csv",header = FALSE)

#install.packages("arules")

library(arules)

dataset = read.transactions("Market_Basket_Optimisation.csv",sep = ',', rm.duplicates = TRUE)
                            
itemFrequencyPlot(dataset,topN=10)
                            
##training eclat to dataset
                            
rules = eclat(data = dataset,parameter = list(support = 0.004,minlen=2))

## visualize the results
inspect(sort(rules, by='support')[1:10])
