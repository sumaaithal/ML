##import mall dataset
dataset = read.csv("Mall_Customers.csv")

X <- dataset[4:5]

#using elbow method to find optimal number of clusters
set.seed(6)

##using the dendrograms to find optimal number of clusters
dg = hclust(dist(X,method = 'euclidean'),method = 'ward.D')
plot(dg, main=paste('Dendrogram'),
     xlab='customers',ylab='Euclidean distances')

#fitting hirarchical cluster algo
hc = hclust(dist(X,method = 'euclidean'),method = 'ward.D')
y_hc = cutree(hc,k=5)

#visualize the cluster
#visualizing the clusers
library(cluster)

clusplot(X,
         y_hc,
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