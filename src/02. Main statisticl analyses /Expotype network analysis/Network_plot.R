#K-means_network_plot

# plot igraph network for univariate analysis of clusters

library(igraph)
library(tidyverse)
library(tibble)
library(fastDummies)



mydata <- readRDS('../../../Data/biomarkers_data_standardized.rds')

mydata <- mutate(mydata, km_cluster_identity_6 = res.km_6$cluster) %>%
  mutate(mydata, km_cluster_identity_6 = as.factor(km_cluster_identity_6))

input <- mydata[,c(6,7,8,9,14,15)]

mydata <- mydata %>% mutate(fastDummies::dummy_cols(mydata, select_columns = "km_cluster_identity_6"))

y <- c('km_cluster_identity_6_1','km_cluster_identity_6_2','km_cluster_identity_6_3',' km_cluster_identity_6_4','km_cluster_identity_6_5','km_cluster_identity_6_6')
x <- c('Platelet_count','Lymphocyte_count','Monocyte_count','Neutrophil_count','C_reactive_protein','IGF_1')

# logistic LASSO 6
# IGF Neutrophil Platelet

#stab6 <- stab
stab6$Lambda
# s28


# logistic LASSO 2
# IGF Neutrophil Platelet

#stab2 <- stab
stab2$Lambda
#s24


# logistic LASSO 4
# IGF Neutrophil Platelet

#stab4 <- stab
stab4$Lambda


# LASSO LTL 
# 2, 4, 6

#stab_LTL <- stab



library(ggplot2)
library(RColorBrewer)
g1 <- graph.data.frame(beta, directed=FALSE)
V(g1)[1:6]$color <- "aquamarine4"
V(g1)[8:13]$color <- "darkgoldenrod2"
V(g1)[7]$color <- "coral3"
E(g1)$color <- ifelse(E(g1)$value < 0, "darkblue", "darkred")
E(g1)$width <- edge.betweenness(g1)

V(g1)

print(V(g1)$color)
pdf(file = "cv_lasso_graph.pdf")
plot(g1, layout= layout.gem, main="Expotype Network Analysis", vertex.label.color= "black", vertex.label.size = 0.3)
dev.off()

