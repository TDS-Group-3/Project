# Read in data (use only 578 variables, last one is TLT)
complete_data_training_dummy <- readRDS("final_data/complete_data_training_dummy.rds")

no_pfer_Group_LASSO_2_selection_proportions <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/07_main_group_lasso/04_group_lasso_variable_selection_2/no_pfer_Group_LASSO_2_selection_proportions.rds")
my_data <- no_pfer_Group_LASSO_2_selection_proportions

my_data <- subset(my_data, selprop > 0.9)

colname = rownames(my_data)
my_data$col_index <- lapply(colname, function(x) (which(colnames(complete_data_training_dummy) == x)))

my_data$col_index <- as.numeric(my_data$col_index)

# Slice the dataset (only select the siginificant variable)
# [1] "Age.at.recruitment"                                                         
# [2] "Sex_Female"                                                                 
# [3] "Age.completed.full.time.education_[10, 15)"                                 
# [4] "Age.completed.full.time.education_[15, 20)"                                 
# [5] "Age.completed.full.time.education_[20, 25)"                                 
# [6] "Age.completed.full.time.education_[25, 30)"                                 
# [7] "Age.completed.full.time.education_[30, 35)"                                 
# [8] "Age.completed.full.time.education_{35}"                                     
# [9] "Age.completed.full.time.education_Never went to school"                     
# [10] "Age.completed.full.time.education_Unknown"                                  
# [11] "Qualifications_A levels/AS levels or equivalent"                            
# [12] "Qualifications_College or University degree"                                
# [13] "Qualifications_CSEs or equivalent"                                          
# [14] "Qualifications_NVQ or HND or HNC or equivalent"                             
# [15] "Qualifications_O levels/GCSEs or equivalent"                                
# [16] "Qualifications_Other professional qualifications eg: nursing, teaching"     
# [17] "Qualifications_Unknown"                                                     
# [18] "Professional.qualifications_Yes"                                            
# [19] "Pub.or.social.club_Yes"                                                     
# [20] "Religious.group_Yes"                                                        
# [21] "Walk..excluding.work._Yes"                                                  
# [22] "Frequency.of.friend.family.visits_2-4 times a week"                         
# [23] "Frequency.of.friend.family.visits_About once a month"                       
# [24] "Frequency.of.friend.family.visits_About once a week"                        
# [25] "Frequency.of.friend.family.visits_Almost daily"                             
# [26] "Frequency.of.friend.family.visits_No friends/family outside household"      
# [27] "Frequency.of.friend.family.visits_Once every few months"                    
# [28] "Frequency.of.friend.family.visits_Unknown"                                  
# [29] "Serious.illness..injury.or.assault.to.yourself_Yes"                         
# [30] "Usual.walking.pace_Brisk pace"                                              
# [31] "Usual.walking.pace_None of the above"                                       
# [32] "Usual.walking.pace_Slow pace"                                               
# [33] "Usual.walking.pace_Unable to walk"                                          
# [34] "Usual.walking.pace_Unknown"                                                 
# [35] "Other.exercises..eg..swimming..cycling..keep.fit..bowling._Unknown"         
# [36] "Other.exercises..eg..swimming..cycling..keep.fit..bowling._Yes"             
# [37] "Heavy.DIY..eg..weeding..lawn.mowing..carpentry..digging._Unknown"           
# [38] "Heavy.DIY..eg..weeding..lawn.mowing..carpentry..digging._Yes"               
# [39] "Morning.evening.person.chronotype_Definitely a 'morning' person"            
# [40] "Morning.evening.person.chronotype_More a 'morning' than 'evening' person"   
# [41] "Morning.evening.person.chronotype_More an 'evening' than a 'morning' person"
# [42] "Morning.evening.person.chronotype_Unknown"                                  
# [43] "Past.tobacco.smoking_Current tobacco smoker"                                
# [44] "Past.tobacco.smoking_Just tried once or twice"                              
# [45] "Past.tobacco.smoking_Smoked occasionally"                                   
# [46] "Past.tobacco.smoking_Smoked on most or all days"                            
# [47] "Past.tobacco.smoking_Unknown"                                               
# [48] "Cheese.intake_2-4 times a week"                                             
# [49] "Cheese.intake_5-6 times a week"                                             
# [50] "Cheese.intake_Less than once a week"                                        
# [51] "Cheese.intake_Once a week"                                                  
# [52] "Cheese.intake_Once or more daily"                                           
# [53] "Cheese.intake_Unknown"                                                      
# [54] "Bread.intake_[10, 15)"                                                      
# [55] "Bread.intake_[15, 20)"                                                      
# [56] "Bread.intake_[20, 25)"                                                      
# [57] "Bread.intake_[25, 30)"                                                      
# [58] "Bread.intake_[30, 200]"                                                     
# [59] "Bread.intake_[5, 10)"                                                       
# [60] "Bread.intake_Unknown"                                                       
# [61] "Bread.type_Brown"                                                           
# [62] "Bread.type_Other type of bread"                                             
# [63] "Bread.type_Unknown"                                                         
# [64] "Bread.type_White"                                                           
# [65] "Bread.type_Wholemeal or wholegrain"                                         
# [66] "Cereal.type_Biscuit cereal (e.g. Weetabix)"                                 
# [67] "Cereal.type_Bran cereal (e.g. All Bran, Branflakes)"                        
# [68] "Cereal.type_Muesli"                                                         
# [69] "Cereal.type_Oat cereal (e.g. Ready Brek, porridge)"                         
# [70] "Cereal.type_Other (e.g. Cornflakes, Frosties)"                              
# [71] "Cereal.type_Unknown"                                                        
# [72] "Salt.added.to.food_Always"                                                  
# [73] "Salt.added.to.food_Sometimes"                                               
# [74] "Salt.added.to.food_Unknown"                                                 
# [75] "Salt.added.to.food_Usually"                                                 
# [76] "Coffee.type_Ground coffee (include espresso, filter etc)"                   
# [77] "Coffee.type_Instant coffee"                                                 
# [78] "Coffee.type_Other type of coffee"                                           
# [79] "Coffee.type_Unknown"                                                        
# [80] "Major.dietary.changes.in.the.last.5.years_Unknown"                          
# [81] "Major.dietary.changes.in.the.last.5.years_Yes, because of illness"          
# [82] "Major.dietary.changes.in.the.last.5.years_Yes, because of other reasons"    
# [83] "Not.eat.sugar.or.foods.drinks.containing.sugar_Yes"                         
# [84] "Alcohol.usually.taken.with.meals_It varies"                                 
# [85] "Alcohol.usually.taken.with.meals_Never consume alcohol"                     
# [86] "Alcohol.usually.taken.with.meals_Unknown"                                   
# [87] "Alcohol.usually.taken.with.meals_Yes"                                       
# [88] "Skin.colour_Black"                                                          
# [89] "Skin.colour_Brown"                                                          
# [90] "Skin.colour_Dark olive"                                                     
# [91] "Skin.colour_Fair"                                                           
# [92] "Skin.colour_Light olive"                                                    
# [93] "Skin.colour_Unknown"                                                        
# [94] "Ease.of.skin.tanning_Get mildly or occasionally tanned"                     
# [95] "Ease.of.skin.tanning_Get moderately tanned"                                 
# [96] "Ease.of.skin.tanning_Get very tanned"                                       
# [97] "Ease.of.skin.tanning_Unknown"                                               
# [98] "Facial.ageing_Older than you are"                                           
# [99] "Facial.ageing_Unknown"                                                      
# [100] "Facial.ageing_Younger than you are"                                         
# [101] "Use.of.sun.uv.protection_Always"                                            
# [102] "Use.of.sun.uv.protection_Most of the time"                                  
# [103] "Use.of.sun.uv.protection_Never/rarely"                                      
# [104] "Use.of.sun.uv.protection_Sometimes"                                         
# [105] "Use.of.sun.uv.protection_Unknown"                                           
# [106] "Country.of.birth.UK.elsewhere_Elsewhere"                                    
# [107] "Country.of.birth.UK.elsewhere_Northern Ireland"                             
# [108] "Country.of.birth.UK.elsewhere_Republic of Ireland"                          
# [109] "Country.of.birth.UK.elsewhere_Scotland"                                     
# [110] "Country.of.birth.UK.elsewhere_Unknown"                                      
# [111] "Country.of.birth.UK.elsewhere_Wales"                                        
# [112] "Breastfed.as.a.baby_Unknown"                                                
# [113] "Breastfed.as.a.baby_Yes"                                                    
# [114] "Hair.colour.natural..before.greying_Black"                                  
# [115] "Hair.colour.natural..before.greying_Blonde"                                 
# [116] "Hair.colour.natural..before.greying_Dark brown"                             
# [117] "Hair.colour.natural..before.greying_Other"                                  
# [118] "Hair.colour.natural..before.greying_Red"                                    
# [119] "Hair.colour.natural..before.greying_Unknown"                                
# [120] "Maternal.smoking.around.birth_Unknown"                                      
# [121] "Maternal.smoking.around.birth_Yes" 

# TLT 579

# Heavy.DIY..eg..weeding..lawn.mowing..carpentry..digging._Unknown  NA    282

complete_data_training_remove_group_lasso2 <- complete_data_training_dummy[, c(2, 3, 70:85, 99:100, 105, 108:114, 
                                                                               141, 218:222, 276:277, 283, 
                                                                               368:371, 389:393, 451:456,
                                                                               467:478, 485:501, 508, 520:523,
                                                                               535:560, 567, 568, 569, 570, 571, 572,
                                                                               577, 578, 579)]

saveRDS(complete_data_training_remove_group_lasso2, "07_main_group_lasso/05_group_lasso_training_2/complete_data_training_remove_group_lasso2.rds")

# Create function
foo = function(data) {
  model = lm(Z.adjusted.T.S.log ~ ., data = data)
  
  coef_pval = data.frame(summary(model)$coefficients[-1,c(1:2,4)])
  ci = data.frame(confint(model, level=0.95)[-1,])
  
  res = cbind(coef_pval, ci)
  colnames(res) = c("coef", "coef.se", "pval", "95% CI lower", "95% CI upper")
  return(res)
}

data = complete_data_training_remove_group_lasso2
model_linear_group_lasso2 = foo(data)
saveRDS(model_linear_group_lasso2, "07_main_group_lasso/05_group_lasso_training_2/model_linear_group_lasso2.rds")
