cust_data = read.csv(file='Wholesale_customers_data.csv', header = TRUE)
print(dim(cust_data))
print(str(cust_data))
View(cust_data)
# checking if there are any NAs in data
print(apply(cust_data, 2, function (x) sum(is.na(x))))
print(summary(cust_data))
cust_data<-cust_data[,c(-1,-2)]
# excluding the non-useful columns from the dataset
# verifying the dataset post columns deletion
dim(cust_data)
library(factoextra)
# computing and printing the hopikins statistic
print(get_clust_tendency(cust_data, graph=FALSE,n=50,seed = 123))
library(NbClust)
# Computing the optimal number of clusters through the NbClust function
# with distance as euclidean and using kmeans
NbClust(cust_data,distance="euclidean", method="kmeans")
# computing the the intra-cluster distance with Ks ranging from 2 to 10
library(purrr)
tot_withinss <- map_dbl(2:10, function(k){
  model <- kmeans(cust_data, centers = k, nstart = 50)
  model$tot.withinss
})
# converting the Ks and computed intra-cluster distances to a dataframe
screeplot_df <- data.frame(k = 2:10,tot_withinss = tot_withinss)
# plotting the elbow curve
library(ggplot2)
print( ggplot(screeplot_df, aes(x = k, y = tot_withinss)) +
         geom_line() +
         scale_x_continuous(breaks = 1:10) +
         labs(x = "k", y = "Within Cluster Sum of Squares") +
         ggtitle("Total Within Cluster Sum of Squares by # of Clusters
(k)") +
         geom_point(data = screeplot_df[2,], aes(x = k, y = tot_withinss),
                    col = "red2", pch = 4, size = 7))
library(cluster)
# runing kmeans in cust_data dataset to obtain 3 clusters
kmeansout <- kmeans(cust_data, centers = 3, nstart = 50)
print (kmeansout)
fviz_cluster(kmeansout,data=cust_data)
