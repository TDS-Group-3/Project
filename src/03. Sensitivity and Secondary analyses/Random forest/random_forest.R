set.seed(1234)

# read in the data: complete_data_variable_selection_dummy.rds
complete_data_training_dummy <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/final_data/complete_data_training_dummy.rds")

names(complete_data_training_dummy) <- make.names(names(complete_data_training_dummy), unique = TRUE)

library(ranger)
ranger_random_forest <- ranger(
  formula = Z.adjusted.T.S.log ~ ., 
  data = complete_data_training_dummy, importance = "impurity", mtry = 193)

save(ranger_random_forest, file = "/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/03_random_forest/ranger_random_forest_p3.RData")


