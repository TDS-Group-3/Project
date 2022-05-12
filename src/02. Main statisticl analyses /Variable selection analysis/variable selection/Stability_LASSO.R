# Stability selection Lasso

mydata <- readRDS('final_data/complete_data_variable_selection_dummy.rds')

y <- mydata[, which(names(mydata) == "Z.adjusted.T.S.log"), drop = FALSE]
x <- mydata[, -which(names(mydata) == "Z.adjusted.T.S.log"), drop = FALSE] 


LoadPackages = function(packages) {
  for (i in 1:length(packages)) {
    suppressPackageStartupMessages(library(packages[i],
                                           character.only = TRUE))
  }
}

LoadPackages(c("focus", "glmnet", "pheatmap","parallel"))

# Running stability selection
t0=Sys.time()
set.seed(1)
out=VariableSelection(xdata=x, ydata=y, verbose=FALSE,
                      n_cores = 8,
                      PFER_thr = 5,
                      K = 1000,
                      family="gaussian")
t1=Sys.time()
print(t1-t0)

CalibrationPlot(out)

# Calibrated selection proportions 

selprop=SelectionProportions(out)

selprop <- as.data.frame(selprop)

saveRDS(selprop, file = "LASSO_selection_proportions.rds")

# Calibrated parameters

hat_params=Argmax(out)

LASSO_parameters <- as.data.frame(hat_params)

saveRDS(LASSO_parameters, file = "LASSO_parameters.rds")

summary(out)

# Selected Variables

LASSO_selected_variables <- SelectedVariables(out)

LASSO_selected_variables <- as.data.frame(LASSO_selected_variables)

saveRDS(LASSO_selected_variables, file = "LASSO_selected_variables.rds")
