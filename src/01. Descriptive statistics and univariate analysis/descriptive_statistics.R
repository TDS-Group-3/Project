# Exploratory data analysis
# Required inputs: env_data_main.rds

# Loading the required packages

library(tidyverse)
library(dplyr)
library(GGally)
library(ggExtra)
library(gridExtra)
library(mltools)
library(pheatmap)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(rlist)
library(zoo)
library(corrplot)
library(gg)

# loading required data

mydata <- readRDS("../../final_data/complete_data.rds")


# Creating a grouping list for complete data

groups <- list()
groups_num <- list()

for (k in 1:ncol(mydata)) {
  if (k %in% 1:4) {
    groups <- list.append(groups, 'Baseline Characteristics')
    groups_num <- list.append(groups_num, 1)
  }
  else if (k %in% 5:17) {
    groups <- list.append(groups, 'Work')
    groups_num <- list.append(groups_num, 2)
  }
  else if (k %in% 18:20) {
    groups <- list.append(groups, 'Education')
    groups_num <- list.append(groups_num, 3)
  }
  else if (k %in% 21:23) {
    groups <- list.append(groups, 'Attendance/Disability/Mobility Allowance')
    groups_num <- list.append(groups_num, 4)
  }
  else if (k %in% 24:39) {
    groups <- list.append(groups, 'Lifestyle')
    groups_num <- list.append(groups_num, 5)
  }
  else if (k %in% 40) {
    groups <- list.append(groups, 'Ethnicity')
    groups_num <- list.append(groups_num, 6)
  }
  else if (k %in% 41:66) {
    groups <- list.append(groups, 'Mental Health')
    groups_num <- list.append(groups_num, 7)
  }
  else if (k %in% 67:89) {
    groups <- list.append(groups, 'Physical Activity')
    groups_num <- list.append(groups_num, 8)
  }
  else if (k %in% 90:118) {
    groups <- list.append(groups, 'Environment/Pollution')
    groups_num <- list.append(groups_num, 9)
  }
  else if (k %in% 119:122) {
    groups <- list.append(groups, 'Mobile Phone Use')
    groups_num <- list.append(groups_num, 10)
  }
  else if (k %in% 123:129) {
    groups <- list.append(groups, 'Sleep')
    groups_num <- list.append(groups_num, 11)
  }
  else if (k %in% 130:136) {
    groups <- list.append(groups, 'Smoking')
    groups_num <- list.append(groups_num, 12)
  }
  else if (k %in% 137:164) {
    groups <- list.append(groups, 'Diet')
    groups_num <- list.append(groups_num, 13)
  }
  else if (k %in% 165:173) {
    groups <- list.append(groups, 'Alcohol')
    groups_num <- list.append(groups_num, 14)
  }
  else if (k %in% 174:181) {
    groups <- list.append(groups, 'Sun Exposure')
    groups_num <- list.append(groups_num, 15)
  }
  else if (k %in% 182:189) {
    groups <- list.append(groups, 'Early life')
    groups_num <- list.append(groups_num, 16)
  }
}  

#feature_groups <- data.frame(row.names = 1:189)

#no_ts <- mydata[,-190]

#feature_groups$group_number <- groups_num
#feature_groups$group_names <- groups
#feature_groups$feature_names <- colnames(no_ts)

#saveRDS(feature_groups, "../../final_data/group_list_not_dummy.rds")

# Creating a grouping list for dummy data

mydata <- readRDS("../../final_data/complete_data_complete_encode.rds")

groups <- list()
groups_num <- list()
var_num <- list()

for (k in 1:ncol(mydata)) {
  if (k %in% 1:4) {
    groups <- list.append(groups, 'Baseline Characteristics')
    groups_num <- list.append(groups_num, 1)
  }
  else if (k %in% 5:69) {
    groups <- list.append(groups, 'Work')
    groups_num <- list.append(groups_num, 2)
  }
  else if (k %in% 70:85) {
    groups <- list.append(groups, 'Education')
    groups_num <- list.append(groups_num, 3)
  }
  else if (k %in% 86:91) {
    groups <- list.append(groups, 'Attendance/Disability/Mobility Allowance')
    groups_num <- list.append(groups_num, 4)
  }
  else if (k %in% 92:119) {
    groups <- list.append(groups, 'Lifestyle')
    groups_num <- list.append(groups_num, 5)
  }
  else if (k %in% 120:139) {
    groups <- list.append(groups, 'Ethnicity')
    groups_num <- list.append(groups_num, 6)
  }
  else if (k %in% 140:203) {
    groups <- list.append(groups, 'Mental Health')
    groups_num <- list.append(groups_num, 7)
  }
  else if (k %in% 204:311) {
    groups <- list.append(groups, 'Physical Activity')
    groups_num <- list.append(groups_num, 8)
  }
  else if (k %in% 312:341) {
    groups <- list.append(groups, 'Environment/Pollution')
    groups_num <- list.append(groups_num, 9)
  }
  else if (k %in% 342:362) {
    groups <- list.append(groups, 'Mobile Phone Use')
    groups_num <- list.append(groups_num, 10)
  }
  else if (k %in% 363:383) {
    groups <- list.append(groups, 'Sleep')
    groups_num <- list.append(groups_num, 11)
  }
  else if (k %in% 384:402) {
    groups <- list.append(groups, 'Smoking')
    groups_num <- list.append(groups_num, 12)
  }
  else if (k %in% 403:508) {
    groups <- list.append(groups, 'Diet')
    groups_num <- list.append(groups_num, 13)
  }
  else if (k %in% 509:530) {
    groups <- list.append(groups, 'Alcohol')
    groups_num <- list.append(groups_num, 14)
  }
  else if (k %in% 531:552) {
    groups <- list.append(groups, 'Sun Exposure')
    groups_num <- list.append(groups_num, 15)
  }
  else if (k %in% 553:578) {
    groups <- list.append(groups, 'Early life')
    groups_num <- list.append(groups_num, 16)
  }
}  

#feature_groups <- data.frame(row.names = 1:578)

#no_ts <- mydata[,-579]

#feature_groups$group_number <- groups_num
#feature_groups$group_name <- groups
#feature_groups$variable_number <- var_levels
#feature_groups$variable_name <- gsub("\\_.*", "", colnames(no_ts))
#feature_groups$level_name <- colnames(no_ts)

#feature_groups$variable_name <- factor(feature_groups$variable_name, levels=colnames(complete_data))

#var_levels <- as.numeric(feature_groups$variable_name)

#saveRDS(feature_groups, "../../final_data/group_list.rds")

# Generating a correlation matrix for all one-hot-encoded variables

one_hot_encoded <- readRDS("../../final_data/complete_data_complete_dummy.rds")

one_hot_encoded <- complete_data_complete_dummy

print(colnames(one_hot_encoded))

one_hot_encoded <- one_hot_encoded[,1:578]

#for (row in 1:nrow(group_list)) {
#  melted_data$group_name <- ifelse(group_list[row,5] == melted_data$variable, group_list$group_name, melted_data$group_name)
#}

cormat <- cor(one_hot_encoded)

saveRDS(cormat, "../../Data/cor_matrix_all.rds")

#cormat <- readRDS("../../Data/cor_matrix_num_vars.rds")

melted_cormat <- melt(cormat)

dimnames(cormat)


corrplot(cormat, tl.pos='n', method='color') %>%
  corrRect(c(1,5,70,86,92,120,140,204,312,342,363,384,403,509,531,553,578))

for (row in 1:nrow(group_list)) {
  melted_cormat$group_name <- ifelse(group_list[row,5] == melted_cormat$Var2, group_list$group_name, melted_cormat$group_name)
}


ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text = element_text(size=10)) +
  scale_fill_gradient2(low = "darkred", high = "darkblue") +
  geom_bracket(mapping=group_list$group_name)

ggsave(filename = "../../outputs/EDA_outputs/cor_matrix_all.pdf", width = 40, height = 30, units = "cm")

# Generating a correlation matrix for only numeric variables

mydata <- readRDS("../../final_data/complete_data.rds")

num_vars <- mydata %>%
  select_if(is.numeric)

cormat_num <- cor(num_vars)

saveRDS(cormat, "../../Data/cor_matrix_num.rds")

#cormat <- readRDS("../../Data/cor_matrix_num_vars.rds")

melted_cormat_num <- melt(cormat_num)

ggplot(data = melted_cormat_num, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(angle = 90, hjust = 1),
        text = element_text(size=10))

ggsave(filename = "../../outputs/EDA_outputs/cor_matrix_num.pdf", width = 40, height = 30, units = "cm")

# Plotting MCA for the categorical variables

num_vars <- mydata %>%
  select_if(is.numeric)

num_vars <- num_vars %>%
  select(!contains('T.S.'))

cat_vars <- mydata[,!(names(mydata) %in% names(num_vars))]

cat_vars <- cat_vars %>%
  select(!contains('T.S.'))

for (k in 1:ncol(cat_vars)) {
  cat_vars[,k] <- as.factor(cat_vars[,k])
}

cat_vars <- cat_vars[1:50000,]

mca_results <- MCA(cat_vars, ncp = 5, graph = FALSE)

saveRDS(mca_results, "../../Data/mca_results.rds")

#mca_results <- readRDS("../../Data/mca_results.rds")

pdf(file="../../outputs/EDA_outputs/mca_ind.pdf", width = 20, height = 20)

fviz_mca_ind(mca_results, 
                ggtheme = theme_minimal())

dev.off()

pdf(file="../../outputs/EDA_outputs/mca_var.pdf", width = 20, height = 20)

fviz_mca_var(mca_results, 
             ggtheme = theme_minimal())

dev.off()

# Plotting the density of each T/S ratio variable

pre_imp <- readRDS("../../final_data/pre_imputation.rds")

ggplot(pre_imp, aes(x=`Z.adjusted.T.S.log`)) +
  geom_density() +
  ylab('Density') +
  xlab('Baseline Z-adjusted T/S log') +
  ggtitle('Z-adjusted T/S log distribution') +
  theme_grey(base_size=15)

ggsave(filename = "../../outputs/EDA_outputs/TS_densities.png", Z_adjusted)

dev.off()

# Plotting the age, sex, and BMI variables against telomere length

ggMarginal(ggplot(pre_imp, aes(x=`Age.at.recruitment`, y=`Z.adjusted.T.S.log`)) +
  geom_point() +
  geom_smooth() +
  ylab('Baseline Z-adjusted T/S log') +
  xlab('Age') +
  ggtitle('Z-adjusted T/S log distribution against age') +
  theme_bw(base_size=15), type="boxplot")

ggMarginal(ggplot(pre_imp, aes(x=`Body.mass.index.BMI`, y=`Z.adjusted.T.S.log`)) +
  geom_point() +
  geom_smooth() +
    ylab('Baseline Z-adjusted T/S log') +
    xlab('Body Mass Index (BMI)') +
    ggtitle('Z-adjusted T/S log distribution against BMI') +
    theme_bw(base_size=15), type="boxplot")
  

ggplot(pre_imp, aes(x=`Sex`, y=`Z.adjusted.T.S.log`, fill=Sex)) +
  geom_boxplot() +
  ylab('Baseline Z-adjusted T/S log') +
  ggtitle('Z-adjusted T/S log distribution stratified by sex') +
  theme_bw(base_size=15)
  

g <- arrangeGrob(age, BMI, sex)

ggsave(filename = "../../outputs/EDA_outputs/adjusting_factors_TLT.pdf", width = 40, height = 30, units = "cm", g)

# Plotting the age, sex, and BMI variables

age2 <- ggplot(pre_imp, aes(x=`Age.at.recruitment`)) +
  geom_density()

BMI2 <- ggplot(pre_imp, aes(x=`Body.mass.index.BMI`)) +
  geom_density()

sex2 <- ggplot(pre_imp, aes(x=`Sex`)) +
  geom_bar()

g2 <- arrangeGrob(age2, BMI2, sex2)

ggsave(filename = "../../outputs/EDA_outputs/adjusting_factors.pdf", width = 20, height = 30, units = "cm", g2)

