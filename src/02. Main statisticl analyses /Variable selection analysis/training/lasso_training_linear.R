# Read in data (use only 578 variables, last one is TLT)
complete_data_training_dummy <- readRDS("final_data/complete_data_training_dummy.rds")
no_pfer_LASSO_selection_proportions <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/05_main_lasso/01_lasso_variable_selection/no_pfer_LASSO_selection_proportions.rds")
my_data <- subset(no_pfer_LASSO_selection_proportions, selprop > 0.9)

colname = rownames(my_data)
my_data$col_index <- lapply(colname, function(x) (which(colnames(complete_data_training_dummy) == x)))

# Slice the dataset (only select the siginificant variable)
# 40

# Body.mass.index.BMI.    1
# Age.at.recruitment.     2
# Sex_Female.             3
# Age.completed.full.time.education_[15, 20).  71
# Qualifications_A levels/AS levels or equivalent.  78
# Time.spent.watching.television.TV.                92
# Pub.or.social.club_Yes                            99
# Religious.group_Yes                               100
# Walk..excluding.work._Yes.                        105
# Ethnic.background_African                         120
# Ethnic.background_Any other white background.     124
# Ethnic.background_Caribbean.                      128
# Ethnic.background_Chinese.                        129
# Ethnic.background_Irish                           131
# Ethnic.background_Other ethnic group              133
# Serious.illness..injury.or.assault.to.yourself_Yes  141
# Usual.walking.pace_Brisk pace                       218
# Duration.walking.for.pleasure_Unknown.              245
# Duration.of.light.DIY_Between 15 and 30 minutes.    269
# Heavy.DIY..eg..weeding..lawn.mowing..carpentry..digging._Yes. 283
# Current.tobacco.smoking_Yes, on most or all days.             388
# Past.tobacco.smoking_Current tobacco smoker.                  389 removed
# Fresh.fruit.intake                                            405
# Cheese.intake_5-6 times a week                                452
# Bread.intake_[10, 15)                                         467
# Bread.type_White                                              477
# Cereal.type_Muesli                                            487
# Salt.added.to.food_Usually                                    494
# Coffee.type_Ground coffee (include espresso, filter etc)      495
# Major.dietary.changes.in.the.last.5.years_Yes, because of illness. 500
# Average.weekly.beer.plus.cider.intake.                            511
# Average.weekly.spirits.intake                                 512
# Alcohol.usually.taken.with.meals_Yes                          523
# Ease.of.skin.tanning_Get very tanned.                         543
# Facial.ageing_Younger than you are                            547
# Use.of.sun.uv.protection_Sometimes                            551
# Country.of.birth.UK.elsewhere_Scotland.                       556
# Country.of.birth.UK.elsewhere_Wales                           558
# Hair.colour.natural..before.greying_Black                     567
# Maternal.smoking.around.birth_Yes                             578

# TLT 579

# We excluded this in our model since they are exactly the same as Current.tobacco.smoking_Yes, on most or all days 
# `Past.tobacco.smoking_Current tobacco smoker`       NA         NA      NA       NA 

complete_data_training_remove_lasso <- complete_data_training_dummy[, c(1, 2, 3, 71, 78, 92, 99, 100, 105, 120, 124, 128, 129,
                                                                        131, 133, 141, 218, 245, 269, 283, 388, 405, 452,
                                                                        467, 477, 487, 494, 495, 500, 511, 512, 523, 543, 547,
                                                                        551, 556, 558, 567, 578, 579)]

saveRDS(complete_data_training_remove_lasso, "05_main_lasso/02_lasso_training/complete_data_training_remove_lasso.rds")
saveRDS(complete_data_training_remove_lasso, "final_data/linear_regression_train/complete_data_training_remove_lasso.rds")


# Create function
foo = function(data) {
  model = lm(Z.adjusted.T.S.log ~ ., data = data)
  
  coef_pval = data.frame(summary(model)$coefficients[-1,c(1:2,4)])
  ci = data.frame(confint(model, level=0.95)[-1,])
  
  res = cbind(coef_pval, ci)
  colnames(res) = c("coef", "coef.se", "pval", "95% CI lower", "95% CI upper")
  return(res)
}

data = complete_data_training_remove_lasso
model_linear_lasso = foo(data)
saveRDS(model_linear_lasso, "05_main_lasso/02_lasso_training/model_linear_lasso.rds")

