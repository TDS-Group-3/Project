# Stability selection Lasso

mydata <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/final_data/secondary_analysis_sex/complete_data_female_variable_selection_dummy.rds")

y <- as.data.frame(mydata[, which(names(mydata) == "Z.adjusted.T.S.log")])
x <- mydata[, -which(names(mydata) == "Z.adjusted.T.S.log")] 


LoadPackages = function(packages) {
  for (i in 1:length(packages)) {
    suppressPackageStartupMessages(library(packages[i],
                                           character.only = TRUE))
  }
}

t <- c(rep(1,ncol(x)),0,0)
LoadPackages(c("focus", "glmnet", "pheatmap","parallel"))

# Running stability selection
t0=Sys.time()
set.seed(1)
out=VariableSelection(xdata=x, ydata=y, verbose=FALSE, 
                      n_cores = 8,
                      K = 1000,
                      family="gaussian")
t1=Sys.time()
print(t1-t0)


# Calibrated selection proportions 

selprop=SelectionProportions(out)

selprop <- as.data.frame(selprop)

saveRDS(selprop, file = "no_pfer_female_LASSO_selection_proportions.rds")

# Calibrated parameters

hat_params=Argmax(out)

LASSO_parameters <- as.data.frame(hat_params)

saveRDS(LASSO_parameters, file = "no_pfer_female_LASSO_parameters.rds")

summary(out)


CalibrationPlot(out)


# Selected Variables

LASSO_selected_variables <- SelectedVariables(out)

LASSO_selected_variables <- as.data.frame(LASSO_selected_variables)

saveRDS(LASSO_selected_variables, file = "no_pfer_female_LASSO_selected_variables.rds")
