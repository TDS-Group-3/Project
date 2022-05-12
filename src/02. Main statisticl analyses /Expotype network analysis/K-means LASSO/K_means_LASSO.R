# Stability selection logistic LASSO for biomarkers

LoadPackages = function(packages) {
  for (i in 1:length(packages)) {
    suppressPackageStartupMessages(library(packages[i],
                                           character.only = TRUE))
  }
}

LoadPackages(c("focus", "glmnet", "pheatmap", "parallel", "tidyverse", "foreach", "doParallel"))

mydata <- readRDS('../../Data/biomarkers_data_standardized.rds')

load(file="km_cluster_6.Rdata") #res.km_6

mydata <- mutate(mydata, km_cluster_identity_6 = res.km_6$cluster) %>%
  mutate(mydata, km_cluster_identity_6 = as.factor(km_cluster_identity_6))

summary(mydata$km_cluster_identity_6)


require(fastDummies)

results <- fastDummies::dummy_cols(mydata, select_columns = "km_cluster_identity_6")

ylist <- select(results, contains("km_cluster_identity_6_"))
x <- mydata[,c(6,7,8,9,14,15)]

# Running stability selection

t0=Sys.time()

set.seed(1)


cl <- makePSOCKcluster(8, setup_strategy = "sequential") 
clusterExport(cl, c("mydata","x","ylist", "LoadPackages"))
clusterEvalQ(cl, LoadPackages(c("focus", "glmnet", "pheatmap", "parallel", "tidyverse", "foreach", "doParallel")))

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = cl)

#check if it is registered (optional)
foreach::getDoParRegistered()

foo <- foreach (i = 1:ncol(ylist)) %dopar% {
  
  stab=VariableSelection(xdata=x, ydata=ylist[,i], verbose=FALSE,
                         K = 1000,
                         family="binomial")
  t1=Sys.time()
  print(t1-t0)
  
  # append function to add to selection proportions
  
  
  t1=Sys.time()
  print(t1-t0)
  
  pdf(file= paste0("CalibrationPlot_group",i,".pdf"))
  CalibrationPlot(stab)
  dev.off()
  
  # Calibrated selection proportions 
  
  selprop=SelectionProportions(stab)
  
  selprop <- as.data.frame(selprop)
  
  saveRDS(selprop, file = paste0("logistic_LASSO_selection_proportions_",i,".rds"))
  
  # Calibrated parameters
  
  hat_params=Argmax(stab)
  
  LASSO_parameters <- as.data.frame(hat_params)
  
  saveRDS(LASSO_parameters, file = paste0("logistic_LASSO_parameters_",i,".rds"))
  
  # betas
  
  beta_matrix = Beta(stab)
  
  k_means_betas <- as.data.frame(beta_matrix)
  
  saveRDS(k_means_betas, file = paste0("logistic_LASSO_betas_",i,".rds"))
  
  
  # Selected Variables
  
  LASSO_selected_variables <- SelectedVariables(stab)
  
  LASSO_selected_variables <- as.data.frame(LASSO_selected_variables)
  
  saveRDS(LASSO_selected_variables, file = paste0("logistic_LASSO_selected_variables_",i,".rds"))
  
}
foo
stopCluster(cl)
closeAllConnections()
