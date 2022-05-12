# Stability selection Group Lasso
library(tidyverse)

mydata <- readRDS('../../final_data/complete_data_variable_selection_dummy.rds')
groupings <- readRDS('../../final_data/group_list.rds')
groupings_x <- as_vector(groupings$variable_number)


str(groupings_x)

y <- mydata[, which(names(mydata) == "Z.adjusted.T.S.log")]
x <- mydata[, -which(names(mydata) == "Z.adjusted.T.S.log")] 


LoadPackages = function(packages) {
  for (i in 1:length(packages)) {
    suppressPackageStartupMessages(library(packages[i],
                                           character.only = TRUE))
  }
}

LoadPackages(c("focus", "glmnet", "pheatmap","parallel", "gglasso"))

# Running stability selection

set.seed(1)

t0=Sys.time()

if (requireNamespace("gglasso", quietly = TRUE)) {
  set.seed(1)
  ManualGridGroupLasso <- function(xdata, ydata, family, group_x, ...) {
    # Defining the grouping
    group <- groupings_x
    
    if (family == "binomial") {
      ytmp <- ydata
      ytmp[ytmp == min(ytmp)] <- -1
      ytmp[ytmp == max(ytmp)] <- 1
      return(gglasso::gglasso(xdata, ytmp, loss = "logit", group = group, ...))
    } else {
      return(gglasso::gglasso(xdata, ydata, lambda = lambda, loss = "ls", group = group, ...))
    }
  }
  Lambda <- LambdaGridRegression(
    xdata = x, ydata = y,
    family = "gaussian", Lambda_cardinal = 100,
    implementation = ManualGridGroupLasso,
    group_x = groupings_x
  )
  
  GroupLasso <- function(xdata, ydata, Lambda, family, group_x, ...) {
    # Defining the grouping
    group <- groupings_x
    
    # Running the regression
    if (family == "binomial") {
      ytmp <- ydata
      ytmp[ytmp == min(ytmp)] <- -1
      ytmp[ytmp == max(ytmp)] <- 1
      mymodel <- gglasso::gglasso(xdata, ytmp, lambda = Lambda, loss = "logit", group = group, ...)
    }
    if (family == "gaussian") {
      mymodel <- gglasso::gglasso(xdata, ydata, lambda = Lambda, loss = "ls", group = group, ...)
    }
    # Extracting and formatting the beta coefficients
    beta_full <- t(as.matrix(mymodel$beta))
    beta_full <- beta_full[, colnames(xdata)]
    
    selected <- ifelse(beta_full != 0, yes = 1, no = 0)
    
    return(list(selected = selected, beta_full = beta_full))
  }
  stab <- VariableSelection(
    xdata = x, ydata = y,
    implementation = GroupLasso, family = "gaussian", Lambda = Lambda,
    PFER_thr = 5,
    n_cores = 8,
    K = 1000,
    group_x = groupings_x,
    group_penalisation = TRUE
  )
  summary(stab)
}


t1=Sys.time()
print(t1-t0)

CalibrationPlot(stab)

# Calibrated selection proportions 

selprop=SelectionProportions(stab)

selprop <- as.data.frame(selprop)

saveRDS(selprop, file = "Group_LASSO_2_selection_proportions.rds")

# Calibrated parameters

hat_params=Argmax(stab)

LASSO_parameters <- as.data.frame(hat_params)

saveRDS(LASSO_parameters, file = "Group_LASSO_2_parameters.rds")


# Selected Variables

LASSO_selected_variables <- SelectedVariables(stab)

LASSO_selected_variables <- as.data.frame(LASSO_selected_variables)

saveRDS(LASSO_selected_variables, file = "Group_LASSO_2_selected_variables.rds")