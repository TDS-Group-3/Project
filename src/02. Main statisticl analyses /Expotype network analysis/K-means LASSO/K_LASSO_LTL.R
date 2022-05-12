# Stability selection logistic LASSO for biomarkers

LoadPackages = function(packages) {
  for (i in 1:length(packages)) {
    suppressPackageStartupMessages(library(packages[i],
                                           character.only = TRUE))
  }
}

LoadPackages(c("focus", "glmnet", "pheatmap", "parallel", "tidyverse", "foreach", "doParallel"))

mydata <- readRDS('../../../Data/biomarkers_data_standardized.rds')

load(file="../km_cluster_6.Rdata") #res.km_6

mydata <- mutate(mydata, km_cluster_identity_6 = res.km_6$cluster) %>%
  mutate(mydata, km_cluster_identity_6 = as.factor(km_cluster_identity_6))

summary(mydata$km_cluster_identity_6)


require(fastDummies)

results <- fastDummies::dummy_cols(mydata, select_columns = "km_cluster_identity_6")

ylist <- select(results, contains("km_cluster_identity_6_"))

x <- as.data.frame(mydata$Z.adjusted.T.S.log)

# Running stability selection

t0=Sys.time()

set.seed(1)
  
  stab=VariableSelection(xdata=ylist, ydata= x[[1]] , verbose=FALSE,
                         K = 1000,
                         n_cores = 4,
                         family="gaussian")
  t1=Sys.time()
  print(t1-t0)
  
  save(stab, file = paste0("VS_","LTL",".Rdata"))
  
  # append function to add to selection proportions
  
  
  pdf(file= paste0("CalibrationPlot_","LTL",".pdf"))
  CalibrationPlot(stab)
  dev.off()
  
  # Calibrated selection proportions 
  
  selprop = SelectionProportions(stab)
  
  selprop <- as.data.frame(selprop)
  
  saveRDS(selprop, file = paste0("LASSO_selection_proportions_","LTL",".rds"))
  
  # Calibrated parameters
  
  hat_params=Argmax(stab)
  
  LASSO_parameters <- as.data.frame(hat_params)
  
  saveRDS(LASSO_parameters, file = paste0("LASSO_parameters_","LTL",".rds"))
  
  # betas
  
  beta_matrix = stab$Beta
  
  save(beta_matrix, file = paste0("LASSO_betas_","LTL",".Rdata"))
  
  
  # Selected Variables
  
  LASSO_selected_variables <- SelectedVariables(stab)
  
  LASSO_selected_variables <- as.data.frame(LASSO_selected_variables)
  
  saveRDS(LASSO_selected_variables, file = paste0("LASSO_selected_variables_","LTL",".rds"))
  

