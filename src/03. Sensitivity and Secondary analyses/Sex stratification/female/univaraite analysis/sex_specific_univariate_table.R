library(tidyverse)
library(docxtools)
library(knitr)
library(dplyr)
female <- univariate_linear_table_female
female <- tibble::rownames_to_column(female, "Variable")

female$Group<-NA
female$Group[1]<-"Baseline Characteristics"
female$Group[2:66]<-"Work"
female$Group[67:82]<-"Education"
female$Group[83:85]<-"Disability"
female$Group[86:113]<-"Lifestyle"
female$Group[114:133]<-"Ethnicity"
female$Group[134:191]<-"Mental Health"
female$Group[192:299]<-"Physical Activity"
female$Group[300:329]<-"Environment"
female$Group[330:350]<-"Electronic Device Use"
female$Group[351:371]<-"Sleep"
female$Group[372:390]<-"Smoking"
female$Group[391:496]<-"Diet"
female$Group[497:518]<-"Alcohol"
female$Group[519:540]<-"Sun Exposure"
female$Group[541:566]<-"Early life"
female$Group[567:606]<-"Female Specific"

female[c('Variables', 'Levels')] <- str_split_fixed(female$Variable, '_', 2)
female <- female[, c(7, 8, 9, 2, 4, 5, 6)]
colnames(female)<-c("Group","Variables","Level", "Beta","95%_CI_lower","95%_CI_upper","p-value")

female_sig <- female[female$`p-value`< 0.05/606, ]

female <- female %>%
  format_engr(sigdig = 3)

female$CI <- paste(female$`95%_CI_lower`, female$`95%_CI_upper`, sep=",")
female$CI <- paste(female$`Beta`, female$`CI`, sep="[")

female$a <- "]"
female$CI <- paste(female$`CI`, female$`a`)

female <- female[, c(1, 2, 3, 8, 7)]

kable(univariate_linear_table)


--------------------------------------------------------------------------------
male <- univariate_linear_table_male  
male <- tibble::rownames_to_column(male, "Variable") 

male$Group<-NA
male$Group[1]<-"Baseline Characteristics"
male$Group[2:66]<-"Work"
male$Group[67:82]<-"Education"
male$Group[83:88]<-"Disability"
male$Group[89:116]<-"Lifestyle"
male$Group[117:136]<-"Ethnicity"
male$Group[137:200]<-"Mental Health"
male$Group[201:308]<-"Physical Activity"
male$Group[309:338]<-"Environment"
male$Group[339:359]<-"Electronic Device Use"
male$Group[360:380]<-"Sleep"
male$Group[381:399]<-"Smoking"
male$Group[400:505]<-"Diet"
male$Group[506:527]<-"Alcohol"
male$Group[528:549]<-"Sun Exposure"
male$Group[550:575]<-"Early life"
male$Group[576:586]<-"Female Specific" 
  
male[c('Variables', 'Levels')] <- str_split_fixed(male$Variable, '_', 2)
male <- male[, c(7, 8, 9, 2, 4, 5, 6)]
colnames(male)<-c("Group","Variables","Level", "Beta","95%_CI_lower","95%_CI_upper","p-value")  
  
male_sig <- male[male$`p-value`< 0.05/586, ]

male <- male %>%
  format_engr(sigdig = 3)

male$CI <- paste(male$`95%_CI_lower`, male$`95%_CI_upper`, sep=",")
male$CI <- paste(male$`Beta`, male$`CI`, sep="[")

male$a <- "]"
male$CI <- paste(male$`CI`, male$`a`)

male <- male[, c(1, 2, 3, 8, 7)]

kable(male) 
  
  
  
  

