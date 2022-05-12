# Stability selection Exclusive LASSO

LoadPackages = function(packages) {
  for (i in 1:length(packages)) {
    suppressPackageStartupMessages(library(packages[i],
                                           character.only = TRUE))
  }
}

LoadPackages(c("focus", "glmnet", "pheatmap", "parallel", "tidyverse", "foreach", "doParallel"))

mydata <- readRDS('../../final_data/complete_data_variable_selection_dummy.rds')
groupings <- readRDS('../../final_data/group_list.rds')
groupings_x <- as_vector(groupings$group_number)

str(groupings_x)


# Running stability selection

t0=Sys.time()

set.seed(1)


cl <- makePSOCKcluster(8, setup_strategy = "sequential") 
clusterExport(cl, c("mydata", "groupings", "groupings_x", "LoadPackages"))
clusterEvalQ(cl, LoadPackages(c("focus", "glmnet", "pheatmap", "parallel", "tidyverse", "foreach", "doParallel")))

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = cl)

#check if it is registered (optional)
foreach::getDoParRegistered()

foo <- foreach (i = unique(groupings_x)) %dopar% {
  
    var_frame <- groupings[groupings$group_number == i,]
    vars <- var_frame$level_name
    head(var_frame)
    
    
    y <- mydata[, which(names(mydata) == "Z.adjusted.T.S.log")]
    x <- mydata %>% select(all_of(vars))
    
    stab=VariableSelection(xdata=x, ydata=y, verbose=FALSE,
                          PFER_thr = 5,
                          K = 1000,
                          family="gaussian")
    t1=Sys.time()
    print(t1-t0)
    
    # append function to add to selection proportions
    
    
    t1=Sys.time()
    print(t1-t0)
    
    pdf(file= paste0("CalibrarionPlot_group",i,".pdf"))
    CalibrationPlot(stab)
    dev.off
    
    # Calibrated selection proportions 
    
    selprop=SelectionProportions(stab)
    
    selprop <- as.data.frame(selprop)
    
    saveRDS(selprop, file = paste0("Exclusive_LASSO_selection_proportions_",i,".rds"))
    
    # Calibrated parameters
    
    hat_params=Argmax(stab)
    
    LASSO_parameters <- as.data.frame(hat_params)
    
    saveRDS(LASSO_parameters, file = paste0("Exclusive_LASSO_parameters_",i,".rds"))
    
    
    # Selected Variables
    
    LASSO_selected_variables <- SelectedVariables(stab)
    
    LASSO_selected_variables <- as.data.frame(LASSO_selected_variables)
    
    saveRDS(LASSO_selected_variables, file = paste0("Exclusive_LASSO_selected_variables_",i,".rds"))
    
}
foo
stopCluster(cl)
closeAllConnections()
