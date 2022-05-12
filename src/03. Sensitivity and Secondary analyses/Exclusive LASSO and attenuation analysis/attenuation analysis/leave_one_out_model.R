# filter all variables selected by exclusive lasso on training set 
exclusive_lasso_training_remove <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/04_exclusive_lasso/exclusive_lasso_training/exclusive_lasso_training_remove.rds")
my_data <- exclusive_lasso_training_remove

# Create function
foo = function(data) {
  model = lm(Z.adjusted.T.S.log ~ ., data = data)
  
  coef_pval = data.frame(summary(model)$coefficients[-1,c(1:2,4)])
  ci = data.frame(confint(model, level=0.95)[-1,])
  
  res = cbind(coef_pval, ci)
  colnames(res) = c("coef", "coef.se", "pval", "95% CI lower", "95% CI upper")
  return(res)
}

# -----------------------------------------------------------------------------#

# Model 1: Demographics 
# Baseline Characteristics + Early life + Ethnicity + Education
data = my_data[, c(1:2, 134:140, 37:41, 15:20, 141)]
model1 = foo(data)
saveRDS(model1, "08_sensitivity_analysis/02_leave_one_out/model1_demographics.rds")

# ------------------------------------------------------------------------------#

# Model 2: Demographics + Social 
# Baseline Characteristics + Early life + Ethnicity + Education 
# Work + Diet + Lifestyle
data = my_data[, c(1:2, 134:140, 37:41, 15:20, 
                   3:14, 97:120, 24:36, 
                   141)]
model2 = foo(data)
saveRDS(model2, "08_sensitivity_analysis/02_leave_one_out/model2_demographics_social.rds")

# ------------------------------------------------------------------------------#

# Model 3: Demographics + Social + Health Risk Factors
# Baseline Characteristics + Early life + Ethnicity + Education 
# Work + Diet + Lifestyle
# Physical Activity + Mental Health + Sleep + Alcohol + Smoking + Attendance/disability/mobilty allowance
data = my_data[, c(1:2, 134:140, 37:41, 15:20, 
                   3:14, 97:120, 24:36, 
                   52:71, 42:51, 84:90, 121:124, 91:96, 21:23,
                   141)]
model3 = foo(data)
saveRDS(model3, "08_sensitivity_analysis/02_leave_one_out/model3_demographics_social_health_risk_factors.rds")

# ------------------------------------------------------------------------------#

# Model 4: Demographics + Social + Health Risk Factors + Environmental 
# Baseline Characteristics + Early life + Ethnicity + Education 
# Work + Diet + Lifestyle
# Physical Activity + Mental Health + Sleep + Alcohol + Smoking + Attendance/disability/mobilty allowance
# Mobile Phone use + Environment and Pollution + Sun Exposure
data = my_data[, c(1:2, 134:140, 37:41, 15:20, 
                   3:14, 97:120, 24:36, 
                   52:71, 42:51, 84:90, 121:124, 91:96, 21:23,
                   77:83, 72:76, 125:133,
                   141)]
model4 = foo(data)
saveRDS(model4, "08_sensitivity_analysis/02_leave_one_out/model4_all.rds")

