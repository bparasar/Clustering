#Assignment 4: Clustering Algorithms
#Basundhara Parasar

library(readxl)
library(tidyverse)  
library(cluster)    
library(factoextra) 
library(kernlab)
library(dbscan)

#Load the CRISA Bathsoap worksheets into R
path <- "Assgt4_clusBathSoap_Data.xls"

bath_soap <- path %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map(read_excel, path = path)


#Extract the summary sheet and convert into a dataframe
bath_soap_summary <- as.data.frame(bath_soap[[1]])
attach(bath_soap_summary)

#Calculate Brand Loyalty
brand_loyalty <- (`Brand Runs` * 0.5) + +((`Vol/Tran`/`Trans / Brand Runs`)*0.5)

#Combine Brand Loyalty with the original dataset
bath_soap_summary <- cbind(bath_soap_summary,brand_loyalty)


str(bath_soap_summary)

#Select features that indicate Purchase Behaviour
purchase_behaviour <- bath_soap_summary[,c("No. of Brands",
                                           "Total Volume",
                                           "Value","Avg. Price","brand_loyalty")]

#Scale the features
purchase_behaviour <- scale(purchase_behaviour)

#Select features that indicate Basis for Purchase
basis_for_purchase <- bath_soap_summary[,c("Pr Cat 1","Pr Cat 2",
                                           "Pr Cat 3","PropCat 14",
                                           "PropCat 5","Pur Vol Promo 6 %")]
#Scale the features
basis_for_purchase <- scale(basis_for_purchase)

#Combine Purchase Behaviour and Basis for Purchase
combine <- cbind(purchase_behaviour,basis_for_purchase)

#----Clustering Algorithms------#
#Buid clusters using Purchase Behaviour, Basis for Purchase and 
#combination of both.
#Play around with parameters in order to get the best clusters

#Elbow method to find best K
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(purchase_behaviour, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


set.seed(123)

fviz_nbclust(purchase_behaviour, kmeans, method = "wss")


k3 <- kmeans(purchase_behaviour, centers = 3, nstart = 25)
p3 <- fviz_cluster(k3, geom = "point",  data = purchase_behaviour) + ggtitle("k = 3")

str(purchase_behaviour)

purchase_behaviour <- as.matrix(purchase_behaviour)
eudist <- distNumeric(purchase_behaviour,purchase_behaviour,method="se")

#Perform Clustering using K-Mediod
kmed_result <- pam(x=purchase_behaviour, k=3)
kmed_viz <- fviz_cluster(kmed_result, geom = "point",  data = purchase_behaviour) + ggtitle("k = 3")

# Ward Hierarchical Clustering
d <- dist(purchase_behaviour, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=3, border="red")

#Perform Clustering using DBSCAN
dbscan <- dbscan(purchase_behaviour,eps=1,minPts = 5)
dbscan_viz <- fviz_cluster(dbscan, geom = "point",  data = purchase_behaviour) + ggtitle("DBSCAN")
