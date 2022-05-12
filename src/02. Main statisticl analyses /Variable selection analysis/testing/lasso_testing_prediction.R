## Training dataset 

# read in the data 
complete_data_training_remove_lasso <- readRDS("05_main_lasso/02_lasso_training/complete_data_training_remove_lasso.rds")

# fit in the training set 
data = complete_data_training_remove_lasso
model = lm(Z.adjusted.T.S.log ~ ., data = data)
summary(model)
plot(model)

# ------------------------------------------------------------------------------#

## Testing dataset

# read in the data for testing and select features that's selected by lasso
complete_data_testing_dummy <- readRDS("final_data/complete_data_testing_dummy.rds")
complete_data_testing_remove_lasso <- complete_data_testing_dummy[, c(1, 2, 3, 71, 78, 92, 99, 100, 105, 120, 124, 128, 129,
                                                                      131, 133, 141, 218, 245, 269, 283, 388, 405, 452,
                                                                      467, 477, 487, 494, 495, 500, 511, 512, 523, 543, 547,
                                                                      551, 556, 558, 567, 578, 579)]

saveRDS(complete_data_testing_remove_lasso, "05_main_lasso/03_lasso_testing/complete_data_testing_remove_lasso.rds")
saveRDS(complete_data_testing_remove_lasso, "final_data/linear_regression_test/complete_data_testing_remove_lasso.rds")

# predict on the prediction

prediction <- predict(model, newdata = complete_data_testing_remove_lasso)

# create prediction table
prediction_table <- data.frame(complete_data_testing_remove_lasso$`Z.adjusted.T.S.log`, prediction)
saveRDS(prediction_table, file = "/rds/general/project/hda_21-22/live/TDS/Group_3/05_main_lasso/03_lasso_testing/prediction_table.rds")

# -------------------#
# performance matrix #
# -------------------#

# R Square/Adjusted R Square
# Mean Square Error(MSE)/Root Mean Square Error(RMSE)
# Mean Absolute Error(MAE)

# R^2
R2 = cor(complete_data_testing_remove_lasso$Z.adjusted.T.S.log, prediction)^2

# Adjusted R^2
a = 1-R2
b = dim(prediction_table)[1]-1
c = dim(prediction_table)[1] - 39 - 1

performance_result = data.frame( R2 = cor(complete_data_testing_remove_lasso$Z.adjusted.T.S.log, prediction)^2,
                                 Adjusted_R2 = 1-(a*b)/c,
                                 MSE = mean((complete_data_testing_remove_lasso$Z.adjusted.T.S.log - prediction)^2),
                                 RMSE = sqrt(mean((complete_data_testing_remove_lasso$Z.adjusted.T.S.log - prediction)^2)),
                                 MAE = mean(abs(complete_data_testing_remove_lasso$Z.adjusted.T.S.log - prediction)))

saveRDS(performance_result, file = "/rds/general/project/hda_21-22/live/TDS/Group_3/05_main_lasso/03_lasso_testing/performance_result_lasso.rds")


