# kmeans clustering and visualization

mydata <- readRDS('../../Data/biomarkers_data_standardized.rds')

library(ggplot2)
library(ggpubr)
library(factoextra)
library(fastcluster)
library(cluster)
library(tidyverse)
library(dendextend)
library(corrplot)

str(mydata)


input <- mydata[,c(6,7,8,9,14,15)]
pdf(file="final_corr_cluster.pdf")
corrmatrix <- cor(input)
corrplot(corrmatrix, method = 'number')
dev.off()

proteins <- mydata[,c(14,15)]

val = input[sample(nrow(input), 10000),]
val_protein = proteins[sample(nrow(proteins), 10000),]

pdf(file="protein_elbow.pdf") 
fviz_nbclust(val_protein, kmeans, method = c("wss"))
dev.off()

pdf(file="protein_gap_stat.pdf") 
fviz_nbclust(val_protein, kmeans, method = c("gap_stat"))
dev.off()


pdf(file="final_silhouette_score.pdf") 
fviz_nbclust(val, kmeans, method = c("silhouette"))
dev.off()

# Compute k-means with k = 10
set.seed(1)

res.km_10 <- kmeans(input, 10, nstart = 25)
# K-means clusters showing the group of each individuals
res.km_10$cluster
# K-means plotting

pdf(file = "Cluster_plot_k_means_10.pdf")
fviz_cluster(res.km_10, data = input,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
dev.off()

save(res.km_10, file="km_cluster_10.Rdata")

# Compute k-means with k = 6

res.km_6 <- kmeans(input, 6, nstart = 25)
# K-means clusters showing the group of each individuals
res.km_6$cluster
# K-means plotting
pdf(file = "final_Cluster_plot_k_means_6.pdf")
fviz_cluster(res.km_6, data = input,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_minimal(),
             palette = "Dark2"
)
dev.off()

save(res.km_6, file="km_cluster_6.Rdata")

# Compute k-means with k = 2

res.km_2 <- kmeans(input, 2, nstart = 25)
# K-means clusters showing the group of each individuals
res.km_2$cluster
# K-means plotting
pdf(file = "Cluster_plot_k_means_2.pdf")
fviz_cluster(res.km_2, data = input,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_minimal(),
             palette = "Dark2"
)
dev.off()

save(res.km_2, file="km_cluster_2.Rdata")

# Compute k-means with k = 8
load("km_cluster_8.Rdata")
res.km_8 <- kmeans(input, 8, nstart = 25)
# K-means clusters showing the group of each individuals
res.km_8$cluster
# K-means plotting
pdf(file = "Cluster_plot_k_means_8.pdf")
fviz_cluster(res.km_8, data = input,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_minimal(),
             palette = "BrBG"
)
dev.off()

save(res.km_8, file="km_cluster_8.Rdata")



protein_km_3 <- kmeans(proteins, 3, nstart = 25)
# K-means clusters showing the group of each individuals
protein_km_3$cluster
# K-means plotting
pdf(file = "Cluster_plot_proteins_k_means_8.pdf")
fviz_cluster(protein_km_3, data = proteins,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_minimal(),
             palette = "Dark2"
)
dev.off()

save(protein_km_3, file="protein_km_cluster_3.Rdata")
