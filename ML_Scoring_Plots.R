# Shawn Schwartz
# Alfaro Lab 2019 - ICB Chaet Paper 2019
# 02/21/2019
# Unsupervised Machine Learning Scoring Plots for Manuscript

############
# INCLUDES #
############
rm(list=ls())
library(tidyverse)
library(ggplot2)

#########
# SETUP #
#########
# set file paths 
wdir_path <- "~/Dropbox/ICB_Chaet_paper_2019/unsupervised-machine-learning"
fig_path <- "Analyses/"

setwd(wdir_path)

################
# READ IN DATA #
################
# get all PCA csvs from ML output

# get ML Silhouette Scores for Partitional KMeans Clustering
kmeans_part_ml_silscores <- read.csv(file = "~/Dropbox/ICB_Chaet_paper_2019/unsupervised-machine-learning/Partitional_K-Means_Clustering/_ML-Output/Silhouette_Scores.csv", header = FALSE, sep = ",")
names(kmeans_part_ml_silscores)[1] <- "ml.score"
kmeans_part_ml_silscores$n.clusters <- seq.int(nrow(kmeans_part_ml_silscores))+1

# get ML Silhouette Scores for Hierarchical Agglomerative Clustering
aglo_hier_ml_silscores <- read.csv(file = "~/Dropbox/ICB_Chaet_paper_2019/unsupervised-machine-learning/Hierarchical_Agglomerative_Clustering/_ML-Output/Silhouette_Scores.csv", header = FALSE, sep = ",")
names(aglo_hier_ml_silscores)[1] <- "ml.score"
aglo_hier_ml_silscores$n.clusters <- seq.int(nrow(aglo_hier_ml_silscores))+1

##################
# MAKE BAR PLOTS #
##################
pdf(paste0(fig_path,"silhouette_scores_ml_kmeans.pdf"))
ggplot(data = kmeans_part_ml_silscores, aes(x = kmeans_part_ml_silscores$n.clusters, y = kmeans_part_ml_silscores$ml.score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(kmeans_part_ml_silscores$ml.score, digits = 3)), vjust = -0.5, color = "black", size = 2.5) +
  scale_x_discrete(name = "Number of Clusters", limits=c(kmeans_part_ml_silscores$n.clusters)) +
  ylab("Silhouette Score") +
  theme_minimal()
dev.off()

pdf(paste0(fig_path,"silhouette_scores_ml_hierarchical.pdf"))
ggplot(data = aglo_hier_ml_silscores, aes(x = aglo_hier_ml_silscores$n.clusters, y = aglo_hier_ml_silscores$ml.score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(aglo_hier_ml_silscores$ml.score, digits = 3)), vjust = -0.5, color = "black", size = 2.5) +
  scale_x_discrete(name = "Number of Clusters", limits=c(kmeans_part_ml_silscores$n.clusters)) +
  ylab("Silhouette Score") +
  theme_minimal()
dev.off()
