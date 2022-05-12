complete_data_variable_selection_dummy <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/final_data/complete_data_variable_selection_dummy.rds")  

foo = function(X) {
  model0 = lm(Z.adjusted.T.S.log ~ Body.mass.index.BMI + Age.at.recruitment + Sex_Female, 
                data = complete_data_variable_selection_dummy)
  model1 = lm(Z.adjusted.T.S.log ~ Body.mass.index.BMI + Age.at.recruitment + Sex_Female + X, 
                data = complete_data_variable_selection_dummy)

  res = c(summary(model1)$coefficients["X", 1:2], confint(model1, "X", level=0.95), anova(model0, model1)$`Pr(>F)`[2])
  names(res) = c("coef", "coef.se", "95% CI lower", "95% CI upper", "pval")
  return(res)
}

univ1 = t(apply(complete_data_variable_selection_dummy[, 4:578], 2, FUN = foo))
univ1 = data.frame(univ1)


saveRDS(univ1, "/rds/general/project/hda_21-22/live/TDS/Group_3/04_univariate_linear/univariate_linear_table.rds")





