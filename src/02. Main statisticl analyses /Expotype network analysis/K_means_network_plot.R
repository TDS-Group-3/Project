#K-means_network_plot

# plot igraph network for univariate analysis of clusters

library(igraph)
library(tidyverse)
library(tibble)
library(fastDummies)



mydata <- readRDS('../../Data/biomarkers_data_standardized.rds')

mydata <- mutate(mydata, km_cluster_identity_6 = res.km_6$cluster) %>%
  mutate(mydata, km_cluster_identity_6 = as.factor(km_cluster_identity_6))

input <- mydata[,c(6,7,8,9,14,15)]

mydata <- mydata %>% mutate(fastDummies::dummy_cols(mydata, select_columns = "km_cluster_identity_6"))

y <- c('km_cluster_identity_6_1','km_cluster_identity_6_2','km_cluster_identity_6_3',' km_cluster_identity_6_4','km_cluster_identity_6_5','km_cluster_identity_6_6')
x <- c('Platelet_count','Lymphocyte_count','Monocyte_count','Neutrophil_count','C_reactive_protein','IGF_1')

library(glmnet)

# with apply:
ylist <- (mydata[,c(17:22)])
xlist <- (input)

library(broom)
df_beta = NULL
foo = function(X){
for (i in 1:ncol(ylist)){
  model <- cv.glmnet(x = as.matrix(X), y = (ylist[[i]]), family = binomial, nfolds = 5, intercept=FALSE)
  lambda_best <- model$lambda.1se
  fit = glmnet(x = as.matrix(X), y =(ylist[[i]]), family = binomial, lambda = lambda_best, intercept=FALSE)
  beta <- tidy(coef(fit))
  beta_df = rbind(df_beta, beta)
}
  return(beta_df)
}

# manual bish

model1 <- cv.glmnet(x = as.matrix(xlist), y = (ylist[[1]]), family = binomial, nfolds = 5, intercept=FALSE)
lambda_best <- model1$lambda.1se
fit1 = glmnet(x = as.matrix(xlist), y =(ylist[[1]]), family = binomial, lambda = lambda_best, intercept=FALSE)
beta1 <- tidy(coef(fit1))

model2 <- cv.glmnet(x = as.matrix(xlist), y = (ylist[[2]]), family = binomial, nfolds = 5, intercept=FALSE)
lambda_best <- model2$lambda.1se
fit2 = glmnet(x = as.matrix(xlist), y =(ylist[[2]]), family = binomial, lambda = lambda_best, intercept=FALSE)
beta2 <- tidy(coef(fit2))

model3 <- cv.glmnet(x = as.matrix(xlist), y = (ylist[[3]]), family = binomial, nfolds = 5, intercept=FALSE)
lambda_best <- model3$lambda.1se
fit3 = glmnet(x = as.matrix(xlist), y =(ylist[[3]]), family = binomial, lambda = lambda_best, intercept=FALSE)
beta3 <- tidy(coef(fit3))

model4 <- cv.glmnet(x = as.matrix(xlist), y = (ylist[[4]]), family = binomial, nfolds = 5, intercept=FALSE)
lambda_best <- model4$lambda.1se
fit4 = glmnet(x = as.matrix(xlist), y =(ylist[[4]]), family = binomial, lambda = lambda_best, intercept=FALSE)
beta4 <- tidy(coef(fit4))

model5 <- cv.glmnet(x = as.matrix(xlist), y = (ylist[[5]]), family = binomial, nfolds = 5, intercept=FALSE)
lambda_best <- model5$lambda.1se
fit5 = glmnet(x = as.matrix(xlist), y =(ylist[[5]]), family = binomial, lambda = lambda_best, intercept=FALSE)
beta5 <- tidy(coef(fit5))

model6 <- cv.glmnet(x = as.matrix(xlist), y = (ylist[[6]]), family = binomial, nfolds = 5, intercept=FALSE)
lambda_best <- model6$lambda.1se
fit6 = glmnet(x = as.matrix(xlist), y =(ylist[[6]]), family = binomial, lambda = lambda_best, intercept=FALSE)
beta6 <- tidy(coef(fit6))

beta = NULL
for (i in 1:6) {
  beta_t <- get(paste0("beta",i))
  beta_t <- beta_t %>% mutate(cluster = i, .before = column)
  beta = rbind(beta, beta_t)
}
# lasso <- foo(xlist)
 
beta = beta[-3]
# t <- glmnet(x = as.matrix(xlist), y = (ylist[1]))

str(xlist)

# get coefs
#library(broom)
#coefs = lapply(mods, tidy, simplify = F)
# combine
#dplyr::bind_rows(coefs, .id = "mod")

summary(model_2)$coefficients[,1]
summary(model_2)$coefficients[,4]*(-1)

coefs$`Platelet_count vs km_cluster_identity_6_1`
# Network plot

network <- readRDS("univ_km_6.rds")
str(network)

network <- tibble::rownames_to_column(network, "Cluster")
str(network)

network <- network %>% 
  pivot_longer(!Cluster, names_to = "biomarkers", values_to = "pval") %>%
  mutate(pval = ifelse(pval == 0, 1, pval))

str(network)

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


LTL_1 <- lm(mydata$Z.adjusted.T.S.log ~ mydata$km_cluster_identity_6 -1)

summary(LTL_1)

LTL_lasso <- cv.glmnet(x = as.matrix(ylist), y = (mydata[[4]]), family = gaussian, nfolds = 5, intercept=FALSE)
lambda_best <- LTL_lasso$lambda.1se
fit_LTL = glmnet(x = as.matrix(ylist), y =(mydata[[4]]), family = gaussian, lambda = lambda_best, intercept=FALSE)
beta_LTL <- tidy(coef(fit_LTL))

beta_LTL <- beta_LTL[-2]



beta_append <- rbind(c("Z.adjusted.T.S.log", 2, beta_LTL$value[1]),c("Z.adjusted.T.S.log", 4, beta_LTL$value[2]))
beta_append <- as.data.frame(beta_append)

beta <- rbind(beta, beta_cool)                  # Print row-binded data frame

beta_cool <- setNames(beta_append, names(beta))
library(tidyverse)
 

