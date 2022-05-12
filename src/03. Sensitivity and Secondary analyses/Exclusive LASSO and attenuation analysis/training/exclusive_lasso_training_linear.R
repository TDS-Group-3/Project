# read in the data 
no_pfer_Exclusive_LASSO_selection_proportions_complete <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/04_exclusive_lasso/no_pfer_Exclusive_LASSO_selection_proportions_complete.rds")
my_data <- no_pfer_Exclusive_LASSO_selection_proportions_complete

# Read in data (use only 578 variables, last one is TLT)
complete_data_training_dummy <- readRDS("final_data/complete_data_training_dummy.rds")

# get all variables that are selected
library(dplyr)
variable <- subset(my_data, selprop > 0.900)

# get column index match with complete_data_training
colname = rownames(variable)
variable$col_index <- lapply(colname, function(x) (which(colnames(complete_data_training_dummy) == x)))

# save variable name with colname_index
saveRDS(variable, "/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/04_exclusive_lasso/exclusive_lasso_training/exclusive_lasso_selected_variable.rds")


# Slice the dataset (only select the siginificant variable)
# Age.at.recruitment                                    2
# Sex_Female                                            3




# Length.of.mobile.phone.use_Five to eight years        342
# Length.of.mobile.phone.use_More than eight years.     343
# Weekly.usage.of.mobile.phone.in.last.3.months_1-3 hours. 347
# Weekly.usage.of.mobile.phone.in.last.3.months_4-6 hours. 349
# Weekly.usage.of.mobile.phone.in.last.3.months_More than 6 hours.            351
# Hands.free.device.speakerphone.use.with.mobile.phone.in.last.3.month_Unknown 357
# Difference.in.mobile.phone.use.compared.to.two.years.previously_Yes, use is now more frequent 362
# Getting.up.in.morning_Not very easy.                   365
# Getting.up.in.morning_Very easy                        367
# Nap.during.day_Sometimes                               372
# Nap.during.day_Usually                                 374
# Sleeplessness...insomnia_Usually                       377
# Snoring_Yes                                            379
# Daytime.dozing...sleeping.narcolepsy_Sometimes         382
# Pack.years.of.smoking                                  384
# Pack.years.adult.smoking.as.proportion.of.life.span.exposed.to.smoking. 385
# Current.tobacco.smoking_Yes, on most or all days.      388
# Past.tobacco.smoking_Just tried once or twice          390
# Past.tobacco.smoking_Smoked on most or all days        392
# Smoking.smokers.in.household_Current tobacco smoker.   394
# Smoking.status_Previous                                399
# Cooked.vegetable.intake                                403
# Tea.intake                                             407
# Water.intake                                           408
# Oily.fish.intake_Less than once a week                 411
# Non.oily.fish.intake_Less than once a week             417
# Processed.meat.intake_2-4 times a week.                421
# Poultry.intake_Less than once a week.                  429
# Poultry.intake_Once a week                             430
# Lamb.mutton.intake_Once a week                         442
# Pork.intake_Once a week                                448
# Cheese.intake_5-6 times a week                         452
# Milk.type.used_Soya                                    461
# Spread.type_Other type of spread/margarine.            465
# Bread.intake_[10, 15)                                  467
# Bread.intake_[20, 25)                                  469
# Bread.intake_[30, 200]                                 471
# Bread.type_White             477
# Cereal.intake_[5, 10)        483
# Cereal.type_Muesli.          487
# Salt.added.to.food_Usually.  494
# Coffee.type_Ground coffee (include espresso, filter etc).  495
# Coffee.type_Instant coffee                                 496
# Major.dietary.changes.in.the.last.5.years_Yes, because of illness.  500
# Not.eat.sugar.or.foods.drinks.containing.sugar_Yes                  508
# Average.weekly.beer.plus.cider.intake                               511
# Average.weekly.spirits.intake                                       512
# Alcohol.intake.frequency._Daily or almost daily                     514
# Alcohol.intake.versus.10.years.previously_More nowadays             526
# Time.spend.outdoors.in.summer                                       531
# Childhood.sunburn.occasions                                         533
# Skin.colour_Black                                                   535
# Skin.colour_Brown                                                   536
# Skin.colour_Fair                                                    538
# Ease.of.skin.tanning_Get mildly or occasionally tanned              541
# Ease.of.skin.tanning_Get very tanned                                543
# Facial.ageing_Younger than you are                                  547
# Use.of.sun.uv.protection_Never/rarely                               550
# Country.of.birth.UK.elsewhere_Elsewhere                             553
# Country.of.birth.UK.elsewhere_Wales                                 558
# Breastfed.as.a.baby_Unknown                                         559
# Breastfed.as.a.baby_Yes                                             560
# Hair.colour.natural..before.greying_Dark brown                      569
# Maternal.smoking.around.birth_Unknown                               577
# Maternal.smoking.around.birth_Yes                                   578



# Current.employment.status_Full or part-time student                  6
# Current.employment.status_Looking after home and/or family           8
# Current.employment.status_Retired.                                   10
# Current.employment.status_Unable to work because of sickness or disability.  11
# Time.employed.in.main.current.job_[30, 58]                           17
# Time.employed.in.main.current.job_Retired                            19
# Length.of.working.week.for.main.job_[20, 40)                         21
# Job.involves.mainly.walking.or.standing_Always                       41
# Job.involves.heavy.manual.or.physical.work_Usually                   50
# None.of.listed.transport.to.work.car.motor.vehicle.walk.public.transport.cycle_Yes.  57
# Walk_Yes               63
# Public.transport_Yes.  66
# Cycle_Yes              69
# Age.completed.full.time.education_[15, 20).    71
# Age.completed.full.time.education_Unknown.     77
# Qualifications_A levels/AS levels or equivalent.   78
# Qualifications_College or University degree.       79
# Qualifications_CSEs or equivalent    80
# Qualifications_O levels/GCSEs or equivalent.    82
# Attendance.allowance_Yes.   87
# Disability.living.allowance_Yes. 89
# Blue.badge_Yes.      91
# Time.spent.watching.television.TV.   92
# Time.spent.using.computer.         93
# # Plays.computer.games_Sometimes.  96
# Sports.club.or.gym_Yes.           98
# Pub.or.social.club_Yes.           99
# Walk..excluding.work._Yes.        105
# Public.transport..excluding.work._Yes. 106
# Cycle..excluding.work._Yes.            107
# Frequency.of.friend.family.visits_2-4 times a week            108
# Frequency.of.friend.family.visits_About once a month          109
# Frequency.of.friend.family.visits_Almost daily                111
# Frequency.of.friend.family.visits_Once every few months.      113
# Drive.faster.than.motorway.speed.limit_Most of the time       116
# Ethnic.background_African                                     120
# Ethnic.background_Any other white background                  124
# Ethnic.background_Caribbean 128
# Ethnic.background_Chinese.  129
# Ethnic.background_Other ethnic group.   133
# Serious.illness..injury.or.assault.to.yourself_Yes.             141
# Serious.illness..injury.or.assault.of.a.close.relative_Yes      143
# Death.of.a.spouse.or.partner_Yes.   147
# Marital.separation.divorce_Yes.     149
# Financial.difficulties_Yes          151
# Miserableness_Yes                   155
# Worry.too.long.after.embarrassment_Yes      169
# Frequency.of.depressed.mood.in.last.2.weeks_Several days.             180
# Frequency.of.tiredness...lethargy.in.last.2.weeks_Several days.       192
# Able.to.confide_2-4 times a week                                      198
# Number.of.days.week.of.moderate.physical.activity.10plus.minutes      205
# Duration.of.moderate.activity                                         206
# Number.of.days.week.walked.10plus.minutes_1                           209
# Usual.walking.pace_Brisk pace.  218
# Usual.walking.pace_Slow pace.   220
# Frequency.of.stair.climbing.in.last.4.weeks_None.  227
# Frequency.of.walking.for.pleasure.in.last.4.weeks_2-3 times in the last 4 weeks. 231
# Frequency.of.walking.for.pleasure.in.last.4.weeks_Once in the last 4 weeks.      235
# Duration.walking.for.pleasure_Over 3 hours                                       243
# Frequency.of.light.DIY.in.last.4.weeks_2-3 times in the last 4 weeks             261
# Frequency.of.light.DIY.in.last.4.weeks_Once a week                               264
# Duration.of.light.DIY_Between 15 and 30 minutes.                                 269
# Other.exercises..eg..swimming..cycling..keep.fit..bowling._Yes.                  277
# Strenuous.sports_Yes.                                                            279
# Heavy.DIY..eg..weeding..lawn.mowing..carpentry..digging._Yes                     283
# Frequency.of.heavy.DIY.in.last.4.weeks_2-3 times a week.                         284
# Frequency.of.heavy.DIY.in.last.4.weeks_4-5 times a week.                         286
# Duration.of.other.exercises_Between 15 and 30 minutes.                           307
# Duration.of.other.exercises_Between 30 minutes and 1 hour
# 309
# Duration.of.other.exercises_Over 3 hours
# 310
# Particulate.matter.air.pollution.pm2.5..
# 315
# Nitrogen.dioxide.air.pollution..2007
# 326
# Particulate.matter.air.pollution.pm10..2007
# 327
# Greenspace.percentage..buffer.m
# 331
# Domestic.garden.percentage..buffer.300m
# 335

# TLT 579

# need to delete this column since colinear with other term: Time.employed.in.main.current.job_Retired  19
# Smoking.smokers.in.household_Current tobacco smoker.   394
exclusive_lasso_training_remove <- complete_data_training_dummy[, c(2, 3, 6, 8, 10, 11, 17, 21, 41, 50,
                                                                    57, 63, 66, 69, 71, 77, 78, 79, 80,
                                                                    82, 87, 89, 91:93, 96, 98, 99, 105:109, 
                                                                    111, 113, 116, 120, 124, 128, 129, 133,
                                                                    141, 143, 147, 149, 151, 155, 169,
                                                                    180, 192, 198, 205, 206, 209, 218, 220, 227, 231, 235,
                                                                    243, 261, 264, 269, 277, 279, 283,
                                                                    284, 286, 307, 309, 310, 315, 326, 327, 331, 335,
                                                                    342, 343, 347, 349, 351, 357, 362, 365, 367, 372, 374, 377, 379,
                                                                    382, 384, 385, 388, 390, 392, 399, 403, 407, 408, 411, 417, 421,
                                                                    429, 430, 442, 448, 452, 461, 465, 467, 469, 471, 477, 483, 487, 494,
                                                                    495, 496, 500, 508, 511, 512, 514, 526, 531, 533, 535, 536, 538, 541,
                                                                    543, 547, 550, 553, 558, 559, 560, 569, 577, 578, 579
                                                                    )]

saveRDS(exclusive_lasso_training_remove, "/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/04_exclusive_lasso/exclusive_lasso_training/exclusive_lasso_training_remove.rds")


# Create function
foo = function(data) {
  model = lm(Z.adjusted.T.S.log ~ ., data = data)
  
  coef_pval = data.frame(summary(model)$coefficients[-1,c(1:2,4)])
  ci = data.frame(confint(model, level=0.95)[-1,])
  
  res = cbind(coef_pval, ci)
  colnames(res) = c("coef", "coef.se", "pval", "95% CI lower", "95% CI upper")
  return(res)
}

data = exclusive_lasso_training_remove
model_linear_exclusive_lasso = foo(data)
saveRDS(model_linear_exclusive_lasso, "/rds/general/project/hda_21-22/live/TDS/Group_3/08_sensitivity_analysis/04_exclusive_lasso/exclusive_lasso_training/model_linear_exclusive_lasso.rds")

