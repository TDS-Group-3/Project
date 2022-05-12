ranger_random_forest <- load("/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/03_random_forest/ranger_random_forest_p3.RData")
print(ranger_random_forest)

# Ranger result

# Call:
#  ranger(formula = Z.adjusted.T.S.log ~ ., data = complete_data_training_dummy, importance = "impurity", mtry = 193) 

# Type:                             Regression 
# Number of trees:                  500 
# Sample size:                      138473 
# Number of independent variables:  578 
# Mtry:                             193 
# Target node size:                 5 
# Variable importance mode:         impurity 
# Splitrule:                        variance 
# OOB prediction error (MSE):       0.01661019 
# R squared (OOB):                  0.0455728 





# Method 1: 
importance <- as.data.frame(ranger::importance(ranger_random_forest))
saveRDS(importance, file="/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/03_random_forest/random_forest_importance.rds")

importance$variable <- rownames(importance)
importance$importance <- importance$`ranger::importance(ranger_random_forest)`
importance <- as.data.frame(importance)

importance[order(importance),]

library(ggplot2)
ggplot(importance, aes(x=reorder(variable,importance), y=importance, fill = importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")

# Plot importance
require(vip)
vip(ranger_random_forest, col= "darkgreen")

# Method 2: 
# read in testing dataset
complete_data_testing_dummy <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/final_data/complete_data_testing_dummy.rds")
names(complete_data_testing_dummy) <- make.names(names(complete_data_testing_dummy), unique = TRUE)

# make prediction on testing set
pred_fun = function(ranger_random_forest, complete_data_testing_dummy) {
  predict(ranger_random_forest, complete_data_testing_dummy)$predictions[,2]
}

require(fastshap)
vip(object = ranger_random_forest, method = "shap", pred_wrapper = pred_fun)



