# Read in data (use only 578 variables, last one is TLT)
complete_data_female_training_dummy <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/final_data/secondary_analysis_sex/complete_data_female_training_dummy.rds")

no_pfer_female_LASSO_selection_proportions <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/female/no_pfer_female_LASSO_selection_proportions.rds")
my_data <- no_pfer_female_LASSO_selection_proportions

# Slice the dataset (only select the siginificant variable 88%)
# Body.mass.index.BMI.        
# Age.at.recruitment
# Ethnic.background_African
# Ethnic.background_Caribbean
# Ethnic.background_Chinese
# Bilateral.oophorectomy.both.ovaries.removed_Yes
# Age.at.menopause.last.menstrual.period
# Usual.walking.pace_Brisk pace
# Country.of.birth.UK.elsewhere_Scotland
# Ethnic.background_Any other white background
# Maternal.smoking.around.birth_Yes
# Ever.had.hysterectomy.womb.removed_Yes
# Country.of.birth.UK.elsewhere_Elsewhere
# Time.spent.watching.television.TV
# Hair.colour.natural..before.greying_Black
# Ever.used.hormone.replacement.therapy.HRT_Yes
# Age.at.first.live.birth
# Use.of.sun.uv.protection_Never/rarely
# Ethnic.background_Any other mixed background
# Ethnic.background_Other ethnic group
# Cycle_Yes
# Frequency.of.friend.family.visits_About once a month
# Time.spend.outdoors.in.summer
# Ethnic.background_Indian
# Walking.for.pleasure..not.as.a.means.of.transport._Yes
# Skin.colour_Black
# Qualifications_A levels/AS levels or equivalent
# Cereal.type_Muesli
# Weekly.usage.of.mobile.phone.in.last.3.months_1-3 hours
# Birth.weight.of.first.child_Unknown
# Had.menopause_Not sure - had a hysterectomy
# Dried.fruit.intake
# Ever.had.breast.cancer.screening...mammogram_Yes
# Pub.or.social.club_Yes
# Ethnic.background_Irish
# Ever.had.cervical.smear.test_Yes
# Heavy.DIY..eg..weeding..lawn.mowing..carpentry..digging._Yes
# Age.at.last.live.birth
# Years.since.last.cervical.smear.test
# Cereal.type_Biscuit cereal (e.g. Weetabix)

# TLT 609


# get all variables that are selected
library(dplyr)
variable <- subset(my_data, selprop >= 0.88)

# get column index match with complete_data_training
colname = rownames(variable)
variable$col_index <- lapply(colname, function(x) (which(colnames(complete_data_female_training_dummy) == x)))



complete_data_female_training_remove_lasso <- complete_data_female_training_dummy[, c(1, 2, 68, 77, 88, 95, 105,
                                                                                      116, 119, 120, 124, 125, 126,
                                                                                      127, 129, 208, 265, 273, 337, 396,
                                                                                      475, 477, 521, 525, 540, 543, 546, 557,
                                                                                      568, 570, 572, 573, 581, 594, 597, 
                                                                                      600, 602, 605, 606, 608, 609)]

saveRDS(complete_data_female_training_remove_lasso, "/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/female/complete_data_female_training_remove_lasso.rds")
saveRDS(complete_data_female_training_remove_lasso, "/rds/general/project/hda_21-22/live/TDS/Group_3/final_data/secondary_analysis_sex/complete_data_female_training_remove_lasso.rds")


# Create function
foo = function(data) {
  model = lm(Z.adjusted.T.S.log ~ ., data = data)
  
  coef_pval = data.frame(summary(model)$coefficients[-1,c(1:2,4)])
  ci = data.frame(confint(model, level=0.95)[-1,])
  
  res = cbind(coef_pval, ci)
  colnames(res) = c("coef", "coef.se", "pval", "95% CI lower", "95% CI upper")
  return(res)
}

data = complete_data_female_training_remove_lasso
model_linear_female_lasso = foo(data)
saveRDS(model_linear_female_lasso, "/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/female/model_linear_female_lasso.rds")

