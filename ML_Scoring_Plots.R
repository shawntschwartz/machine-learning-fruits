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
wdir_path <- "~/Developer/machine-learning-fruits/"
fig_path <- "Analyses/"

setwd(wdir_path)

################
# READ IN DATA #
################
# get all PCA csvs from ML output

# get ML Silhouette Scores for Hierarchical Agglomerative Clustering
aglo_hier_ml_silscores <- read.csv(file = "~/Developer/machine-learning-fruits/_ML-Output/_CSVs/Silhouette_Scores.csv", header = FALSE, sep = ",")
names(aglo_hier_ml_silscores)[1] <- "ml.score"
aglo_hier_ml_silscores$n.clusters <- seq.int(nrow(aglo_hier_ml_silscores))+1

##################
# MAKE BAR PLOTS #
##################
pdf(paste0(fig_path,"silhouette_scores_ml_hierarchical.pdf"))
ggplot(data = aglo_hier_ml_silscores, aes(x = aglo_hier_ml_silscores$n.clusters, y = aglo_hier_ml_silscores$ml.score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(aglo_hier_ml_silscores$ml.score, digits = 3)), vjust = -0.5, color = "black", size = 2.5) +
  scale_x_discrete(name = "Number of Clusters", limits=c(aglo_hier_ml_silscores$n.clusters)) +
  ylab("Silhouette Score") +
  theme_minimal()
dev.off()
