# Exclusive Lasso 

# Complete selected Variable Dataset

library(tidyverse)

df <- list.files(pattern = "no_pfer_Exclusive_LASSO_selected_variables_") %>%
  map(readRDS) %>% 
  bind_rows()

saveRDS(df, file="no_pfer_Exclusive_LASSO_selected_variables_complete.rds")

# Complete list of parameters

 param <- list.files(pattern = "no_pfer_Exclusive_LASSO_parameters") %>%
  map(readRDS) %>% 
  bind_rows()

saveRDS(param, file="no_pfer_Exclusive_LASSO_parameters_complete.rds")


# Complete list of selection proportions

prop <- list.files(pattern = "no_pfer_Exclusive_LASSO_selection_proportions") %>%
  map(readRDS) %>% 
  bind_rows()

saveRDS(prop, file="no_pfer_Exclusive_LASSO_selection_proportions_complete.rds")

prop <- readRDS("no_pfer_Exclusive_LASSO_selection_proportions_complete.rds")

pi <- 0.9

head(prop)

pdf(file = "no_pfer_Exclusive_LASSO_selection_proportions.pdf", height = 5, width = 20)
par(mar=c(7,5,2,1))
plot(prop$selprop, type="h", lwd=1, las=1, xlab="", ylab="Selection Proportion", main="[EXCLUSIVE LASSO] Selection Proportions", xaxt="n",
     col=ifelse(prop>=pi, yes="red", no="grey"), cex.lab=1, ylim = c(0, 1), yaxs = "i")
abline(h=pi, lty=2, col="darkred")
for (i in 1:length(prop$selprop)){
  axis(side=1, at=i, labels=names[i], las=2, cex.axis=0.2, 
       col=ifelse(prop$selprop[i]>=pi, yes="red", no = "transparent"),
       col.axis=ifelse(prop$selprop[i]>=pi, yes="red", no = "transparent"))
}
dev.off()
