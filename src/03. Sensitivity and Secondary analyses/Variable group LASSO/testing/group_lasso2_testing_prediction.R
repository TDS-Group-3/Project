## Training dataset 

# read in the data 
complete_data_training_remove_group_lasso2 <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/07_main_group_lasso/05_group_lasso_training_2/complete_data_training_remove_group_lasso2.rds")

# fit in the training set 
data = complete_data_training_remove_group_lasso2
model = lm(Z.adjusted.T.S.log ~ ., data = data)
summary(model)
plot(model)

# ------------------------------------------------------------------------------#

## Testing dataset

# read in the data for testing and select features that's selected by lasso
complete_data_testing_dummy <- readRDS("final_data/complete_data_testing_dummy.rds")
complete_data_testing_remove_group_lasso2 <- complete_data_testing_dummy[, c(2, 3, 70:85, 99:100, 105, 108:114, 
                                                                             141, 218:222, 276:277, 283, 
                                                                             368:371, 389:393, 451:456,
                                                                             467:478, 485:501, 508, 520:523,
                                                                             535:560, 567, 568, 569, 570, 571, 572,
                                                                             577, 578, 579)]

saveRDS(complete_data_testing_remove_group_lasso2, "07_main_group_lasso/06_group_lasso_testing_2/complete_data_testing_remove_group_lasso2.rds")
saveRDS(complete_data_testing_remove_group_lasso2, "final_data/linear_regression_test/complete_data_testing_remove_group_lasso2.rds")

# predict on the prediction

prediction <- predict(model, newdata = complete_data_testing_remove_group_lasso2)

# create prediction table
prediction_table <- data.frame(complete_data_testing_remove_group_lasso2$`Z.adjusted.T.S.log`, prediction)
saveRDS(prediction_table, file = "/rds/general/project/hda_21-22/live/TDS/Group_3/07_main_group_lasso/06_group_lasso_testing_2/prediction_table_group_lasso_by_variable.rds")

# -------------------#
# performance matrix #
# -------------------#

# R Square/Adjusted R Square
# Mean Square Error(MSE)/Root Mean Square Error(RMSE)
# Mean Absolute Error(MAE)

# R^2
R2 = cor(complete_data_testing_remove_group_lasso2$Z.adjusted.T.S.log, prediction)^2

# Adjusted R^2
a = 1-R2
b = dim(prediction_table)[1]-1
c = dim(prediction_table)[1] - 30 - 1

performance_result = data.frame( R2 = cor(complete_data_testing_remove_group_lasso2$Z.adjusted.T.S.log, prediction)^2,
                                 Adjusted_R2 = 1-(a*b)/c,
                                 MSE = mean((complete_data_testing_remove_group_lasso2$Z.adjusted.T.S.log - prediction)^2),
                                 RMSE = sqrt(mean((complete_data_testing_remove_group_lasso2$Z.adjusted.T.S.log - prediction)^2)),
                                 MAE = mean(abs(complete_data_testing_remove_group_lasso2$Z.adjusted.T.S.log - prediction)))

saveRDS(performance_result, "07_main_group_lasso/06_group_lasso_testing_2/performance_group_lasso_by_variable_name.rds")
# R2           Adjusted_R2        MSE      RMSE       MAE
# 0.05435145  0.05414653     0.01644713 0.1282463 0.1029616


