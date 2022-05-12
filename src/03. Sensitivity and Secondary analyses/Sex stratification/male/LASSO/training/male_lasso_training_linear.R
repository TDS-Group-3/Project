# Read in data (use only 578 variables, last one is TLT)
complete_data_male_training_dummy <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/final_data/secondary_analysis_sex/complete_data_male_training_dummy.rds")

no_pfer_male_LASSO_selection_proportions <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/male/no_pfer_male_LASSO_selection_proportions.rds")
my_data <- no_pfer_male_LASSO_selection_proportions

# Slice the dataset (only select the siginificant variable 84%)

# Age.at.recruitment
# Time.spent.watching.television.TV
# Ethnic.background_African
# Ethnic.background_Any other white background
# Ethnic.background_Caribbean
# Average.weekly.beer.plus.cider.intake
# Ethnic.background_Chinese
# Hair.balding.pattern_Pattern 4
# Maternal.smoking.around.birth_Yes
# Country.of.birth.UK.elsewhere_Wales
# Body.mass.index.BMI
# Getting.up.in.morning_Very easy
# Average.weekly.spirits.intake
# Current.tobacco.smoking_Yes, on most or all days
# Alcohol.usually.taken.with.meals_Yes
# Skin.colour_Black
# Usual.walking.pace_Slow pace
# Ethnic.background_Irish
# Past.tobacco.smoking_Current tobacco smoker
# Major.dietary.changes.in.the.last.5.years_Yes, because of illness
# Past.tobacco.smoking_Just tried once or twice
# Comparative.body.size.at.age..0_Plumper
# Ethnic.background_Any other mixed background
# Ethnic.background_Other ethnic group
# Qualifications_A levels/AS levels or equivalent
# Serious.illness..injury.or.assault.to.yourself_Yes
# Facial.ageing_Younger than you are
# Hair.colour.natural..before.greying_Black
# Frequency.of.stair.climbing.in.last.4.weeks_None
# Cereal.type_Muesli
# Usual.walking.pace_Brisk pace
# Number.of.days.week.of.moderate.physical.activity.10plus.minutes
# Frequency.of.stair.climbing.in.last.4.weeks_More than 20 times a day
# Number.of.children.fathered
# Professional.qualifications_Yes
# Smoking.status_Current
# Age.completed.full.time.education_Unknown
# Duration.of.other.exercises_Between 15 and 30 minutes
# Weekly.usage.of.mobile.phone.in.last.3.months_Unknown
# Nap.during.day_Unknown
# Frequency.of.other.exercises.in.last.4.weeks_Once in the last 4 weeks

# TLT 589

# We excluded these in our model since they are exactly the same as Current.tobacco.smoking_Yes, on most or all days 
# Excluded `Past.tobacco.smoking_Current tobacco smoker`   388    NA         NA      NA       NA 

# get all variables that are selected
library(dplyr)
variable <- subset(my_data, selprop >= 0.84)

# get column index match with complete_data_training
colname = rownames(variable)
variable$col_index <- lapply(colname, function(x) (which(colnames(complete_data_male_training_dummy) == x)))




complete_data_male_training_remove_lasso <- complete_data_male_training_dummy[, c(1, 2, 76, 77, 84, 91, 119,
                                                                                  122, 123, 127, 128, 130, 132,
                                                                                  140, 204, 217, 219, 225, 226, 302,
                                                                                  306, 351, 366, 372, 387, 389,
                                                                                  397, 486, 499, 510, 511, 522, 534, 
                                                                                  546, 557, 560, 566, 577, 586, 588, 589)]

saveRDS(complete_data_male_training_remove_lasso, "/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/male/complete_data_male_training_remove_lasso.rds")
saveRDS(complete_data_male_training_remove_lasso, "/rds/general/project/hda_21-22/live/TDS/Group_3/final_data/secondary_analysis_sex/complete_data_male_training_remove_lasso.rds")


# Create function
foo = function(data) {
  model = lm(Z.adjusted.T.S.log ~ ., data = data)
  
  coef_pval = data.frame(summary(model)$coefficients[-1,c(1:2,4)])
  ci = data.frame(confint(model, level=0.95)[-1,])
  
  res = cbind(coef_pval, ci)
  colnames(res) = c("coef", "coef.se", "pval", "95% CI lower", "95% CI upper")
  return(res)
}

data = complete_data_male_training_remove_lasso
model_linear_male_lasso = foo(data)
saveRDS(model_linear_male_lasso, "/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/male/model_linear_male_lasso.rds")

