
mydata <- readRDS('no_pfer_LASSO_selection_proportions.rds')
parameters <- readRDS('no_pfer_LASSO_parameters.rds')
model_linear_lasso <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/05_main_lasso/02_lasso_training/model_linear_lasso.rds") 

names <- rownames(mydata)
model_linear_lasso$level_name <- gsub("`","", rownames(model_linear_lasso))
rownames(model_linear_lasso) <- gsub("`","", rownames(model_linear_lasso))
selected <- model_linear_lasso$level_name

pdf(file = "no_pfer_LASSO_Selection_proportion_plot.pdf", height = 5, width = 20)



par(mar=c(7,5,2,1))

plot(mydata$selprop, type="h", lwd=1, las=1, xlab="", ylab="Selection Proportion",main="[LASSO] Selection proportions", xaxt="n",
     col=ifelse(mydata$selprop>=parameters$pi , yes= ifelse(rownames(mydata) %in% selected, 
                                                            ifelse(model_linear_lasso[c(rownames(mydata)), 1] > 0, "red", "blue"), "purple"), no = "grey"),
     cex.lab=1, ylim = c(0, 1), yaxs = "i")
for (i in 1:length(mydata$selprop)){
  axis(side=1, at=i, labels=names[i], las=2, cex.axis=0.2, hadj = 1,
       col=ifelse(mydata$selprop[i]>=parameters$pi, yes= ifelse(names[i]  %in% selected, 
                                                                ifelse(model_linear_lasso[model_linear_lasso$level_name == names[i], 1] > 0, "red", "blue"), "purple"), no = "transparent"),
       col.axis=ifelse(mydata$selprop[i]>=parameters$pi, yes= ifelse(names[i]  %in% selected, 
                                                                     ifelse(model_linear_lasso[model_linear_lasso$level_name == names[i], 1] > 0, "red", "blue"), "purple"), no = "transparent"))
}
abline(h=parameters[2], lty=2, col="darkred")
dev.off()

model_linear_lasso[c("Maternal.smoking.around.birth_Yes"), 1]
