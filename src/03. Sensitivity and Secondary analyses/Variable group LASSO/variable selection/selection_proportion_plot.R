
library(tidyverse)

mydata <- readRDS('no_pfer_Group_LASSO_2_selection_proportions.rds')
parameters <- readRDS('no_pfer_Group_LASSO_2_parameters.rds')
groupings <- readRDS('../../final_data/group_list.rds')
model_linear_group_lasso2 <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/07_main_group_lasso/05_group_lasso_training_2/model_linear_group_lasso2.rds") 

mydata <- mydata %>%
  mutate(group_name = groupings$variable_number)
names <- rownames(mydata)
model_linear_group_lasso2$level_name <- gsub("`","", rownames(model_linear_group_lasso2))
rownames(model_linear_group_lasso2) <- gsub("`","", rownames(model_linear_group_lasso2))
selected <- model_linear_group_lasso2$level_name

pdf(file = 'no_pfer_Group_LASSO_2_selection_proportion_plot.pdf', height = 5, width = 20)
par(mar=c(7,4,2,1))
plot(mydata$selprop, type="h", lwd=1, las=1, xlab="", ylab="Selection Proportion", main="[VARIABLE GROUP LASSO] Selection Proportions", xaxt="n",
     col=ifelse(mydata$selprop>=parameters$pi, yes=ifelse(rownames(mydata) %in% selected, 
                                                          ifelse(model_linear_group_lasso2[c(rownames(mydata)), 1] > 0, "red", "blue"), "purple"), no="grey"), cex.lab=1, ylim = c(0, 1), yaxs = "i")
abline(h=parameters[2], lty=2, col="darkred")
for (i in 1:length(mydata$selprop)){
  axis(side=1, at=i, labels=names[i], las=2, cex.axis=0.2, 
       col=ifelse(mydata$selprop[i]>=parameters$pi, yes=ifelse(names[i]  %in% selected, 
                                                               ifelse(model_linear_group_lasso2[model_linear_group_lasso2$level_name == names[i], 1] > 0, "red", "blue"), "purple"), no = "transparent"),
       col.axis=ifelse(mydata$selprop[i]>=parameters$pi, yes= ifelse(names[i]  %in% selected, 
                                                                     ifelse(model_linear_group_lasso2[model_linear_group_lasso2$level_name == names[i], 1] > 0, "red", "blue"), "purple"), no = "transparent"))
}
dev.off()

