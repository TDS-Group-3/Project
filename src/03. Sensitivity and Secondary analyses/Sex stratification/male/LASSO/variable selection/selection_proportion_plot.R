
mydata <- readRDS('no_pfer_male_LASSO_selection_proportions.rds')
parameters <- readRDS('no_pfer_male_LASSO_parameters.rds')
model_linear_male_lasso <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/09_secondary_analysis/02_lasso/male/model_linear_male_lasso.rds") 

names <- rownames(mydata)
model_linear_male_lasso$level_name <- gsub("`","", rownames(model_linear_male_lasso))
rownames(model_linear_male_lasso) <- gsub("`","", rownames(model_linear_male_lasso))
selected <- model_linear_male_lasso$level_name

pdf(file = "no_pfer_male_LASSO_Selection_proportion_plot.pdf", height = 5, width = 20)

par(mar=c(7,5,2,1))
plot(mydata$selprop, type="h", lwd=1, las=1, xlab="", ylab="Selection Proportion",main="[MALE SPECIFIC LASSO] Selection proportions", xaxt="n",
     col=ifelse(mydata$selprop>=parameters$pi, yes=ifelse(rownames(mydata) %in% selected, 
                                                          ifelse(model_linear_male_lasso[c(rownames(mydata)), 1] > 0, "red", "blue"), "purple"), no="grey"), cex.lab=1, ylim = c(0, 1), yaxs = "i")
abline(h=parameters[2], lty=2, col="darkred")
for (i in 1:length(mydata$selprop)){
  axis(side=1, at=i, labels=names[i], las=2, cex.axis=0.2, hadj = 1,
       col=ifelse(mydata$selprop[i]>=parameters$pi, yes=ifelse(names[i]  %in% selected, 
                                                               ifelse(model_linear_male_lasso[model_linear_male_lasso$level_name == names[i], 1] > 0, "red", "blue"), "purple"), no = "transparent"),
       col.axis=ifelse(mydata$selprop[i]>=parameters$pi, yes=ifelse(names[i]  %in% selected, 
                                                                    ifelse(model_linear_male_lasso[model_linear_male_lasso$level_name == names[i], 1] > 0, "red", "blue"), "purple"), no = "transparent"))
}

dev.off()
