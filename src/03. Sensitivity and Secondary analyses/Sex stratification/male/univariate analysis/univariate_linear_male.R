complete_data_male_complete_dummy <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/final_data/secondary_analysis_sex/complete_data_male_complete_dummy.rds")  

foo = function(X) {
  model0 = lm(Z.adjusted.T.S.log ~ Body.mass.index.BMI + Age.at.recruitment, 
              data = complete_data_male_complete_dummy)
  model1 = lm(Z.adjusted.T.S.log ~ Body.mass.index.BMI + Age.at.recruitment + X, 
              data = complete_data_male_complete_dummy)
  
  res = c(summary(model1)$coefficients["X", 1:2], confint(model1, "X", level=0.95), anova(model0, model1)$`Pr(>F)`[2])
  names(res) = c("coef", "coef.se", "95% CI lower", "95% CI upper", "pval")
  return(res)
}

univ_male = t(apply(complete_data_male_complete_dummy[, 3:588], 2, FUN = foo))
univ_male = data.frame(univ_male)

saveRDS(univ_male, "/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/01_univariate_analysis/univariate_linear_table_male.rds")
