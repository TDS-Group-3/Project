library(kableExtra)
library(magrittr)
library(tidyverse)
library(tibble)
library(knitr)
library(docxtools)
library(stringr)


univariate_linear_tab <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/04_univariate_linear/univariate_linear_table.rds")

univariate_linear_table <- tibble::rownames_to_column(univariate_linear_table, "Variable")
univariate_linear_table$Variable <- gsub(".", " ",univariate_linear_table$Variable , fixed = TRUE)

univariate_linear_table$var_type<-NA
univariate_linear_table$var_type[1]<-"Baseline Characteristics"
univariate_linear_table$var_type[2:66]<-"Work"
univariate_linear_table$var_type[67:82]<-"Education"
univariate_linear_table$var_type[83:88]<-"Disability"
univariate_linear_table$var_type[89:116]<-"Lifestyle"
univariate_linear_table$var_type[117:136]<-"Ethnicity"
univariate_linear_table$var_type[137:200]<-"Mental Health"
univariate_linear_table$var_type[201:308]<-"Physical Activity"
univariate_linear_table$var_type[309:338]<-"Environment"
univariate_linear_table$var_type[339:359]<-"Electronic Device Use"
univariate_linear_table$var_type[360:380]<-"Sleep"
univariate_linear_table$var_type[381:399]<-"Smoking"
univariate_linear_table$var_type[400:505]<-"Diet"
univariate_linear_table$var_type[506:527]<-"Alcohol"
univariate_linear_table$var_type[528:549]<-"Sun Exposure"
univariate_linear_table$var_type[550:575]<-"Early life"

univariate_linear_table[c('Variables', 'Levels')] <- str_split_fixed(univariate_linear_table$Variable, '_', 2)

univariate_linear_table <- univariate_linear_table[, c(7, 8, 9, 2, 4, 5, 6)]
colnames(univariate_linear_table)<-c("Group","Variables","Level", "Beta","95%_CI_lower","95%_CI_upper","p-value")

sig_p <- univariate_linear_table[univariate_linear_table$`p-value`<(0.05/575), ]

univariate_linear_table$Group[duplicated(univariate_linear_table$Group)] <- NA
univariate_linear_table$Variables[duplicated(univariate_linear_table$Variables)] <- NA



univariate_linear_table <- univariate_linear_table %>%
  format_engr(sigdig = 3)

univariate_linear_table$CI <- paste(univariate_linear_table$`95%_CI_lower`, univariate_linear_table$`95%_CI_upper`, sep=",")
univariate_linear_table$CI <- paste(univariate_linear_table$`Beta`, univariate_linear_table$`CI`, sep="[")

univariate_linear_table$a <- "]"
univariate_linear_table$CI <- paste(univariate_linear_table$`CI`, univariate_linear_table$`a`)

univariate_linear_table <- univariate_linear_table[, c(1, 2, 3, 8, 7)]







#Table generated in rmarkdown
kable(univariate_linear_table)