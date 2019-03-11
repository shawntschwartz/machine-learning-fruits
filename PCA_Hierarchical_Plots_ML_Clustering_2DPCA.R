# Shawn Schwartz
# Alfaro Lab 2019 - ICB Chaet Paper 2019
# 02/18/2019
# PCA Agglomerative Plots for Unsupervised Machine Learning (Hierarchical)

############
# INCLUDES #
############
rm(list=ls())
library(pavo)
library(tidyverse)
library(ggplot2)
library(ggimage)
library(ggbiplot)

#########
# SETUP #
#########
# set file paths 
wdir_path <- "~/Developer/ML-Fruits"
img_path <- "/Users/shawn/Developer/ML-Fruits/combinedall/"
fig_path <- "Analyses/"

setwd(wdir_path)


twoclusts_data <- read.csv(file = "DesiredClusters_2overallPredictions_outputData.csv", header = TRUE, sep = ",")
fourclusts_data <- read.csv(file = "DesiredClusters_4overallPredictions_outputData.csv", header = TRUE, sep = ",")
twentyoneclusts_data <- read.csv(file = "DesiredClusters_21overallPredictions_outputData.csv", header = TRUE, sep = ",")

two_pca_data <- read.csv(file = "PCA_Data_for_2_clusters.csv", header = FALSE, sep = ",")
four_pca_data <- read.csv(file = "PCA_Data_for_4_clusters.csv", header = FALSE, sep = ",")
twentyone_pca_data <- read.csv(file = "PCA_Data_for_21_clusters.csv", header = FALSE, sep = ",")

# construct image paths for plotting images on scatter plots
image_paths <- str_c(img_path, twoclusts_data$family, "_", twoclusts_data$genus)

# get cluster IDs from ML Kmeans predictions
two_clusters <- twoclusts_data$clusterID
clusterID_2 <- as.factor(two_clusters)

four_clusters <- fourclusts_data$clusterID
clusterID_4 <- as.factor(four_clusters)

twentyone_clusters <- twentyoneclusts_data$clusterID
clusterID_21 <- as.factor(twentyone_clusters)

#plts_names <- str_c(twoclusts_data$family, "_", substr(twoclusts_data$genus,0,2))
plts_names <- str_c(twoclusts_data$family)

centroids_2 <- read.csv(file = "PCA_Centroids_Data_for_2_clusters.csv", header = FALSE, sep = ",")
centroids_4 <- read.csv(file = "PCA_Centroids_Data_for_4_clusters.csv", header = FALSE, sep = ",")
centroids_21 <- read.csv(file = "PCA_Centroids-Data_for_21_clusters.csv", header = FALSE, sep = ",")

# get coordinates for ML predicted centroids for feature space
centroids_2$id <- rbind(0, 1)
centroidID_2 <- centroids_2$id

centroids_4$id <- rbind(0, 1, 2, 3)
centroidID_4 <- centroids_4$id

centroids_21$id <- rbind(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
centroidID_21 <- centroids_21$id

##################
# MAKE PCA PLOTS #
##################
library(ggrepel)
# plot 2D PCA with color labels [2 clusters]
pdf(paste0(fig_path,"ml_pca_2clusters_points_hierarchical_no_labels.pdf"))
ggplot(two_pca_data,aes(two_pca_data$V1,two_pca_data$V2,col=clusterID_2)) +
  #geom_image(aes(image=image_paths), size=.03) +
  #geom_text(aes(label=plts_names), hjust=0, vjust=0) +
  #geom_label_repel(aes(label=plts_names), box.padding = 0.1, point.padding = 0.2, segment.color = 'grey50') +
  #geom_text_repel(aes(label=plts_names), box.padding = 0.35, point.padding = 0.5) +
  xlab("PCA Reduced Component 1") +
  ylab("PCA Reduced Component 2") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  geom_point() +
  geom_point(mapping = aes(centroids_2$V1, centroids_2$V2, colour = factor(centroids_2$id)), shape=42, size=10, data = centroids_2)
dev.off()

# plot 2D PCA with color labels [4 clusters]
pdf(paste0(fig_path,"ml_pca_4clusters_points_hierarchical_labels.pdf"))
ggplot(four_pca_data,aes(four_pca_data$V1,four_pca_data$V2,col=clusterID_4)) +
  #geom_image(aes(image=image_paths), size=.03) +
  #geom_text(aes(label=plts_names), hjust=0, vjust=0) +
  geom_label_repel(aes(label=plts_names), box.padding = 0.1, point.padding = 0.2, segment.color = 'grey50') +
  #geom_text_repel(aes(label=plts_names), box.padding = 0.35, point.padding = 0.5) +
  xlab("PCA Reduced Component 1") +
  ylab("PCA Reduced Component 2") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  geom_point() +
  geom_point(mapping = aes(centroids_4$V1, centroids_4$V2, colour = factor(centroids_4$id)), shape=42, size=10, data = centroids_4)
dev.off()

# plot 2D PCA with color labels [21 clusters]
pdf(paste0(fig_path,"ml_pca_21clusters_points_hierarchical_no_labels.pdf"))
ggplot(twentyone_pca_data,aes(twentyone_pca_data$V1,twentyone_pca_data$V2,col=clusterID_21)) +
  #geom_image(aes(image=image_paths), size=.03) +
  #geom_text(aes(label=plts_names), hjust=0, vjust=0) +
  #geom_label_repel(aes(label=plts_names), box.padding = 0.1, point.padding = 0.2, segment.color = 'grey50') +
  #geom_text_repel(aes(label=plts_names), box.padding = 0.35, point.padding = 0.5) +
  xlab("PCA Reduced Component 1") +
  ylab("PCA Reduced Component 2") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  geom_point() +
  geom_point(mapping = aes(centroids_21$V1, centroids_21$V2, colour = factor(centroids_21$id)), shape=42, size=10, data = centroids_21)
dev.off()
