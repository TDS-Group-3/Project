# read in ranger model
ranger_random_forest_p3 <- load("/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/03_random_forest/ranger_random_forest_p3.RData")
print(ranger_random_forest)

# read in testing dataset
complete_data_testing_dummy <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/final_data/complete_data_testing_dummy.rds")
names(complete_data_testing_dummy) <- make.names(names(complete_data_testing_dummy), unique = TRUE)

# make prediction on testing set
prediction <- predict(ranger_random_forest, data = complete_data_testing_dummy)
str(prediction)

# create prediction table
prediction_table <- data.frame(complete_data_testing_dummy$`Z.adjusted.T.S.log`, prediction$predictions)
saveRDS(prediction_table, file = "/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/03_random_forest/prediction_table.rds")

# Calculating RMSE
residual <- prediction_table$complete_data_testing_dummy.Z.adjusted.T.S.log - prediction_table$prediction.predictions
rmse <- sqrt(sum(residual^2)/length(residual))
rmse
# [1] 0.1111089

# Calcualting performance matrix ----------------------------------------------
# R Square/Adjusted R Square
# Mean Square Error(MSE)/Root Mean Square Error(RMSE)
# Mean Absolute Error(MAE)

# R^2
R2 = cor(prediction_table$complete_data_testing_dummy.Z.adjusted.T.S.log, prediction_table$prediction.predictions)^2

# Adjusted R^2
a = 1-R2
b = dim(prediction_table)[1]-1
c = dim(prediction_table)[1] - 578 - 1

performance_result = data.frame( R2 = cor(prediction_table$complete_data_testing_dummy.Z.adjusted.T.S.log, prediction_table$prediction.predictions)^2,
                                 Adjusted_R2 = 1-(a*b)/c,
                                 MSE = mean((prediction_table$complete_data_testing_dummy.Z.adjusted.T.S.log - prediction_table$prediction.predictions)^2),
                                 RMSE = sqrt(mean((prediction_table$complete_data_testing_dummy.Z.adjusted.T.S.log - prediction_table$prediction.predictions)^2)),
                                 MAE = mean(abs(prediction_table$complete_data_testing_dummy.Z.adjusted.T.S.log - prediction_table$prediction.predictions)))

saveRDS(performance_result, "/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/03_random_forest/performance_random_forest.rds")
# R2          Adjusted_R2        MSE      RMSE        MAE
# 0.3199693   0.3171189       0.01234519   0.1111089   0.08436937


# Make a plot
test <- complete_data_testing_dummy
library(dplyr)
library(ggplot2)
test %>% 
  mutate(predicted = predict(ranger_random_forest, test)$predictions) %>% 
  ggplot(aes(predicted, Z.adjusted.T.S.log)) +
  geom_point(colour = "#ff6767", alpha = 0.3) +
  labs(title = "Predicted and observed") + 
  theme_bw(18)

plot(prediction_table$complete_data_testing_dummy.Z.adjusted.T.S.log ~ prediction_table$prediction.predictions)
grid(); abline(0,1)

