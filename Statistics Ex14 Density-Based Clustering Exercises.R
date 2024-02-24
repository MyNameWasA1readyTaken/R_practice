## 1
data(iris)
iris <- iris[,-c(5)]

## 2
Ex2_Data <- data.frame(scale(iris))

## 3
library(dbscan)
kNNdistplot(Ex2_Data,5)
abline(h=0.79)

## 4
Ex4_thresh <- dbscan(Ex2_Data,eps=.79)
Ex4_thresh

## 5
library(factoextra)
fviz_cluster(Ex4_thresh,Ex2_Data)

## 6
Ex6_Data <- Ex2_Data
Ex6_Data$cluster <- Ex4_thresh$cluster

## 7
kNNdistplot(Ex2_Data,5)
abline(h=0.4)
abline(h=0.5)
abline(h=1.8)

Ex7_.4_thresh <- dbscan(Ex2_Data,eps=.4)
Ex7_.5_thresh <- dbscan(Ex2_Data,eps=.5)
Ex7_1.8_thresh <- dbscan(Ex2_Data,eps=1.8)

fviz_cluster(Ex7_.4_thresh,Ex2_Data)
fviz_cluster(Ex7_.5_thresh,Ex2_Data)
fviz_cluster(Ex7_1.8_thresh,Ex2_Data)

## 8
setwd("/home/marcus/Documents/Rwork/R exercises/")
Data <- read.csv("Wholesale-customers-data.csv")

customers <- Data[,c("Fresh","Milk")]
customers_scale <- data.frame(scale(customers))
kNNdistplot(customers_scale,5)
abline(h=.74)

db_clusters_customers <- dbscan(customers_scale,eps=.74)
db_clusters_customers

fviz_cluster(db_clusters_customers,customers_scale)

## 9
milkrange <- c(mean(customers$Milk) - 2.5 * sd(customers$Milk),mean(customers$Milk) + 2.5 * sd(customers$Milk))
freshrange <- c(mean(customers$Fresh) - 2.5 * sd(customers$Fresh),mean(customers$Fresh) + 2.5 * sd(customers$Fresh))

customers_core <- customers
customers_core <- customers_core[customers_core$Milk > milkrange[1] & customers_core$Milk < milkrange[2],]
customers_core <- customers_core[customers_core$Fresh > freshrange[1] & customers_core$Fresh < freshrange[2],]

km_clusters_customers <- kmeans(customers_core,4)

fviz_cluster(km_clusters_customers,customers_core)

## 10
#p1
Ex10_assn <- db_clusters_customers$cluster

#p2
Ex10_dist <- dist(customers)

#p3
library(cluster)
Ex10_silhouette <- silhouette(Ex10_assn,Ex10_dist)

#p4
fviz_silhouette(Ex10_silhouette)

#p5
Ex10_km_assn <- km_clusters_customers$cluster
Ex10_km_dist <- dist(customers_core)
Ex10_km_silhouette <- silhouette(Ex10_km_assn,Ex10_km_dist)
fviz_silhouette(Ex10_km_silhouette)
