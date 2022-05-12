## Training dataset 

# read in the data 
complete_data_female_training_remove_lasso <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/female/complete_data_female_training_remove_lasso.rds")

# fit in the training set 
data = complete_data_female_training_remove_lasso
model = lm(Z.adjusted.T.S.log ~ ., data = data)
summary(model)
plot(model)

# ------------------------------------------------------------------------------#

## Testing dataset

# read in the data for testing and select features that's selected by lasso
complete_data_female_testing_dummy <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/final_data/secondary_analysis_sex/complete_data_female_testing_dummy.rds")
complete_data_female_testing_remove_dummy <- complete_data_female_testing_dummy[, c(1, 2, 68, 77, 88, 95, 105,
                                                                                    116, 119, 120, 124, 125, 126,
                                                                                    127, 129, 208, 265, 273, 337, 396,
                                                                                    475, 477, 521, 525, 540, 543, 546, 557,
                                                                                    568, 570, 572, 573, 581, 594, 597, 
                                                                                    600, 602, 605, 606, 608, 609)]

saveRDS(complete_data_female_testing_remove_dummy, "/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/female/complete_data_female_testing_remove_dummy.rds")

# predict on the prediction

prediction <- predict(model, newdata = complete_data_female_testing_remove_dummy)

# create prediction table (19 features)
prediction_table <- data.frame(complete_data_female_testing_remove_dummy$Z.adjusted.T.S.log, prediction)
saveRDS(prediction_table, file = "/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/female/prediction_table_female_lasso.rds")

# -------------------#
# performance matrix #
# -------------------#

# R Square/Adjusted R Square
# Mean Square Error(MSE)/Root Mean Square Error(RMSE)
# Mean Absolute Error(MAE)

# R^2
R2 = cor(complete_data_female_testing_remove_dummy$Z.adjusted.T.S.log, prediction)^2

# Adjusted R^2
a = 1-R2
b = dim(prediction_table)[1]-1
c = dim(prediction_table)[1] - 19 - 1

performance_result = data.frame( R2 = cor(complete_data_female_testing_remove_dummy$Z.adjusted.T.S.log, prediction)^2,
                                 Adjusted_R2 = 1-(a*b)/c,
                                 MSE = mean((complete_data_female_testing_remove_dummy$Z.adjusted.T.S.log - prediction)^2),
                                 RMSE = sqrt(mean((complete_data_female_testing_remove_dummy$Z.adjusted.T.S.log - prediction)^2)),
                                 MAE = mean(abs(complete_data_female_testing_remove_dummy$Z.adjusted.T.S.log - prediction)))

saveRDS(performance_result, "/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/female/performance_female_lasso.rds")

# R2          Adjusted_R2        MSE          RMSE           MAE
# 0.04046636  0.04022339     0.01656346     0.1286991    0.1034752

