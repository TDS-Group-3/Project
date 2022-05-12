## Training dataset 

# read in the data 
exclusive_lasso_training_remove <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/04_exclusive_lasso/exclusive_lasso_training/exclusive_lasso_training_remove.rds")

# fit in the training set 
data = exclusive_lasso_training_remove
model = lm(Z.adjusted.T.S.log ~ ., data = data)
summary(model)
plot(model)

# ------------------------------------------------------------------------------#

## Testing dataset

# read in the data for testing and select features that's selected by lasso
complete_data_testing_dummy <- readRDS("final_data/complete_data_testing_dummy.rds")
exclusive_lasso_testing_remove <- complete_data_testing_dummy[, c(2, 3, 6, 8, 10, 11, 17, 21, 41, 50,
                                                                  57, 63, 66, 69, 71, 77, 78, 79, 80,
                                                                  82, 87, 89, 91:93, 96, 98, 99, 105:109, 
                                                                  111, 113, 116, 120, 124, 128, 129, 133,
                                                                  141, 143, 147, 149, 151, 155, 169,
                                                                  180, 192, 198, 205, 206, 209, 218, 220, 227, 231, 235,
                                                                  243, 261, 264, 269, 277, 279, 283,
                                                                  284, 286, 307, 309, 310, 315, 326, 327, 331, 335,
                                                                  342, 343, 347, 349, 351, 357, 362, 365, 367, 372, 374, 377, 379,
                                                                  382, 384, 385, 388, 390, 392, 399, 403, 407, 408, 411, 417, 421,
                                                                  429, 430, 442, 448, 452, 461, 465, 467, 469, 471, 477, 483, 487, 494,
                                                                  495, 496, 500, 508, 511, 512, 514, 526, 531, 533, 535, 536, 538, 541,
                                                                  543, 547, 550, 553, 558, 559, 560, 569, 577, 578, 579)]

saveRDS(exclusive_lasso_testing_remove, "/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/04_exclusive_lasso/exclusive_lasso_testing/exclusive_lasso_testing_remove.rds")

# predict on the prediction

prediction <- predict(model, newdata = exclusive_lasso_testing_remove)

# create prediction table
prediction_table <- data.frame(exclusive_lasso_testing_remove$Z.adjusted.T.S.log, prediction)
saveRDS(prediction_table, file = "/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/04_exclusive_lasso/exclusive_lasso_testing/prediction_table_exclusive_lasso.rds")

# -------------------#
# performance matrix #
# -------------------#

# R Square/Adjusted R Square
# Mean Square Error(MSE)/Root Mean Square Error(RMSE)
# Mean Absolute Error(MAE)

# R^2
R2 = cor(exclusive_lasso_testing_remove$Z.adjusted.T.S.log, prediction)^2

# Adjusted R^2
a = 1-R2
b = dim(prediction_table)[1]-1
c = dim(prediction_table)[1] - 101 - 1

performance_result = data.frame( R2 = cor(exclusive_lasso_testing_remove$Z.adjusted.T.S.log, prediction)^2,
                                 Adjusted_R2 = 1-(a*b)/c,
                                 MSE = mean((exclusive_lasso_testing_remove$Z.adjusted.T.S.log - prediction)^2),
                                 RMSE = sqrt(mean((exclusive_lasso_testing_remove$Z.adjusted.T.S.log - prediction)^2)),
                                 MAE = mean(abs(exclusive_lasso_testing_remove$Z.adjusted.T.S.log - prediction)))

saveRDS(performance_result, "/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/04_exclusive_lasso/exclusive_lasso_testing/performance_exclusive_lasso.rds")
# R2           Adjusted_R2        MSE      RMSE       MAE
# 0.05513012  0.05444044 0.01643353 0.1281933 0.1028898

