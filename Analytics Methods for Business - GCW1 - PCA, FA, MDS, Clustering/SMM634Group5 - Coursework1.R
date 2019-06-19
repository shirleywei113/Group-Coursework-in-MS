######################################################
############## Q1 ####################################
######################################################
# read the table
cars_data <- read.table("cars.dat", header = TRUE, sep = ",")
# standardise the data
cars_data_scale <- scale(cars_data[, -5])
# perform PCA
PCA_cars_data_scale <- prcomp(cars_data_scale)
# show the PVE(proportion of variance explained) and the cumulative proportion
summary(PCA_cars_data_scale)
# compute the proportion of variance explained by each principal component
pr.var = PCA_cars_data_scale$sdev ^ 2
pve = pr.var/sum(pr.var)
# plot the PVE
plot(pve, xlab = " Principal Component", ylab = "Proportion of Variance Explained", 
     ylim = c(0,1), type = "b")
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = "b")
# reproduce subject to sign change
PCA_cars_data_scale$rotation = -PCA_cars_data_scale$rotation
PCA_cars_data_scale$x = -PCA_cars_data_scale$x
# plot the PCA result
library(ggplot2)
library(ggfortify)
autoplot(PCA_cars_data_scale, loadings = TRUE,loadings.label = TRUE, loadings.label.size = 4)

######################################################
############## Q2 ####################################
######################################################
# read the table
cars_data <- read.table("cars.dat", header = TRUE, sep = ",")
# standardise the data
cars_data_scale <- scale(cars_data[, 8:18])
cars_data_scale <- cbind(cars_data[, 1:7], cars_data_scale)
# calculate the Euclidean distances
dist_cars_data_scale <- dist(cars_data_scale)
# perform classical MDS
MDS_cars_data_scale <- cmdscale(dist_cars_data_scale)
# plot the coordinates for the cars 
plot(MDS_cars_data_scale, pch = 1, xlab = "Dimension 1", ylab = "Dimension 2", 
     xlim = c(-10, 10), ylim = c(-10, 3)) 
text(MDS_cars_data_scale[c("Mercedes-Benz CL600", "Mercedes-Benz SL600", "Honda Insight", 
                           "Toyota Prius", "Porsche 911 GT2", "GMC Yukon XL 2500 SLT"), ], 
     rownames(MDS_cars_data_scale[c("Mercedes-Benz CL600", "Mercedes-Benz SL600", "Honda Insight", 
                                    "Toyota Prius", "Porsche 911 GT2", "GMC Yukon XL 2500 SLT"), ]), 
     cex = 1, pos = 3)
text(t(as.matrix(MDS_cars_data_scale["Chevrolet Suburban 1500 LT", ])), 
     labels = "Chevrolet Suburban 1500 LT", cex = 1, pos = 1)
# Analysis of the cars
D1_analysis <- rbind(cars_data_scale[c("Mercedes-Benz CL600", "Mercedes-Benz SL600"), ],
                     cars_data_scale[c("Honda Insight", "Toyota Prius"), ])
D1_analysis
D2_analysis <- rbind(cars_data_scale[c("GMC Yukon XL 2500 SLT", "Chevrolet Suburban 1500 LT"), ],
                     cars_data_scale[c("Porsche 911 GT2", "Mercedes-Benz SL600"), ])
D2_analysis

######################################################
############## Q4 ####################################
######################################################
## (a) ##
# transpose the data and calculate the Euclidean distance matrix
USArrests_dist <- as.matrix(dist(t(USArrests)))
USArrests_dist
## (b) ##
# perform the hierarchical clustering with average linkage
USArrests_hc_ave <- hclust(dist(USArrests), method = "average")
# plot the dendrogram
plot(USArrests_hc_ave, main = "USArrests Hierarchical Clustering with Average Linkage", xlab = "",
     sub = "", cex = 0.9)
## (c) ##
# cut the dendrogram into 4 clusters
USArrests_hc_ave_cut <- cutree(USArrests_hc_ave, k = 4)
USArrests_hc_ave_cut
# plot the dendrogram with 4 clusters
plot(USArrests_hc_ave, main = "USArrests Hierarchical Clustering with Average Linkage", xlab = "",
     sub = "", cex = 0.9)
rect.hclust(USArrests_hc_ave, k = 4, border = "red")
## (d) ##
# access the states in cluster1
USArrests_hc_ave_cut[which(USArrests_hc_ave_cut == 1)]
## (e) ##
# standardise the data
USArrests_sc <- scale(USArrests)
# perform the hierarchical clustering with average linkage
USArrests_sc_hc_ave <- hclust(dist(USArrests_sc), method = "average")
# cut the dendrogram into 4 clusters
USArrests_sc_hc_ave_cut <- cutree(USArrests_sc_hc_ave, k = 4)
USArrests_sc_hc_ave_cut
# plot the dendrogram with 4 clusters
plot(USArrests_sc_hc_ave, main = "Standardised USArrests Hierarchical Clustering with Average Linkage", 
     xlab = "", sub = "", cex = 0.9)
rect.hclust(USArrests_sc_hc_ave, k = 4, border = "red")
# access the states in cluster1
USArrests_sc_hc_ave_cut[which(USArrests_sc_hc_ave_cut == 1)]
# Plot the distribution of the instances before/after scaling
plot(USArrests_hc_ave_cut, xlab = "Distribution of the instances", ylab = "class", main = "Result of cutree function", 
     xaxt = "n", yaxt = "n")
axis(2, 1:4, c(1, 2, 3, 4))
plot(USArrests_sc_hc_ave_cut, xlab = "Distribution of the instances", ylab = "class", 
     main = "Result of cutree function(After scaled)", xaxt = "n", yaxt = "n")
axis(2, 1:4, c(1, 2, 3, 4))