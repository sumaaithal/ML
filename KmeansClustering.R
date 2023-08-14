##import mall dataset
dataset = read.csv("Mall_Customers.csv")

X <- dataset[4:5]

#using elbow method to find optimal number of clusters
set.seed(6)
wcss <- vector()

for(i in 1:10){
  wcss[i] <- sum(kmeans(X,i)$withinss)
}

plot(1:10,wcss,type = 'b',main = paste("Clusters of clients"),
     xlab = "# of clusters",ylab = "Annual income")

#applying kmeans to dataset with 5 clusters
set.seed(6)

kmeans <- kmeans(X,centers = 5,iter.max = 300,nstart = 10)

#visualizing the clusers
library(cluster)

clusplot(X,
         kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("clusters of client"),
         xlab = "annual income",
         ylab = "spending score"
)
