## Training dataset 

# read in the data 
complete_data_male_training_remove_lasso <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/male/complete_data_male_training_remove_lasso.rds")

# fit in the training set 
data = complete_data_male_training_remove_lasso
model = lm(Z.adjusted.T.S.log ~ ., data = data)
summary(model)
plot(model)

# ------------------------------------------------------------------------------#

## Testing dataset

# read in the data for testing and select features that's selected by lasso
complete_data_male_testing_dummy <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/final_data/secondary_analysis_sex/complete_data_male_testing_dummy.rds")
complete_data_male_testing_remove_dummy <- complete_data_male_testing_dummy[, c(1, 2, 76, 77, 84, 91, 119,
                                                                                122, 123, 127, 128, 130, 132,
                                                                                140, 204, 217, 219, 225, 226, 302,
                                                                                306, 351, 366, 372, 387, 389,
                                                                                397, 486, 499, 510, 511, 522, 534, 
                                                                                546, 557, 560, 566, 577, 586, 588, 589)]

saveRDS(complete_data_male_testing_remove_dummy, "/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/male/complete_data_male_testing_remove_dummy.rds")

# predict on the prediction (15 features)
prediction <- predict(model, newdata = complete_data_male_testing_remove_dummy)

# create prediction table (19 features)
prediction_table <- data.frame(complete_data_male_testing_remove_dummy$Z.adjusted.T.S.log, prediction)
saveRDS(prediction_table, file = "/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/male/prediction_table_male_lasso.rds")

# -------------------#
# performance matrix #
# -------------------#

# R Square/Adjusted R Square
# Mean Square Error(MSE)/Root Mean Square Error(RMSE)
# Mean Absolute Error(MAE)

# R^2
R2 = cor(complete_data_male_testing_remove_dummy$Z.adjusted.T.S.log, prediction)^2

# Adjusted R^2
a = 1-R2
b = dim(prediction_table)[1]-1
c = dim(prediction_table)[1] - 15 - 1

performance_result = data.frame( R2 = cor(complete_data_male_testing_remove_dummy$Z.adjusted.T.S.log, prediction)^2,
                                 Adjusted_R2 = 1-(a*b)/c,
                                 MSE = mean((complete_data_male_testing_remove_dummy$Z.adjusted.T.S.log - prediction)^2),
                                 RMSE = sqrt(mean((complete_data_male_testing_remove_dummy$Z.adjusted.T.S.log - prediction)^2)),
                                 MAE = mean(abs(complete_data_male_testing_remove_dummy$Z.adjusted.T.S.log - prediction)))

saveRDS(performance_result, "/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/male/performance_male_lasso.rds")

# R2          Adjusted_R2        MSE      RMSE       MAE
# 0.0547981   0.05457437    0.02067963   0.1438041  0.1151114



