library(tidyverse)
library(docxtools)
library(knitr)
library(dplyr)
library(stringr)
exclusive_lasso <- `model_linear_exclusive_lasso-2`
exclusive_lasso <- tibble::rownames_to_column(exclusive_lasso, "Variable")

exclusive_lasso$Variable <- gsub("`", "", exclusive_lasso$Variable)
exclusive_lasso$Variable <- gsub(".", " ",exclusive_lasso$Variable , fixed = TRUE)
exclusive_lasso$level <- str_split_fixed(exclusive_lasso$Variable, "_", 2)[,2]

exclusive_lasso$Variable <- str_split_fixed(exclusive_lasso$Variable, "_", 2)[,1]


exclusive_lasso$`95% CI lower` <- round(exclusive_lasso$`95% CI lower`, 3)
exclusive_lasso$`95% CI upper` <- round(exclusive_lasso$`95% CI upper`, 3)
exclusive_lasso$coef <- round(exclusive_lasso$coef, 3)

exclusive_lasso$ci_interval_lower <- paste0("[", exclusive_lasso$`95% CI lower`)
exclusive_lasso$ci_interval_upper <- paste0(exclusive_lasso$`95% CI upper`, "]")
exclusive_lasso$ci_interval <- paste(exclusive_lasso$ci_interval_lower, exclusive_lasso$ci_interval_upper, sep=" , ")

exclusive_lasso$ci_interval <- paste(exclusive_lasso$coef, exclusive_lasso$ci_interval)

changeSciNot <- function(n) {
  output <- formatC(n, format='e', digits=2) #Transforms the number into scientific notation even if small
  output <- sub("e", "x10^", output) #Replace e with 10^
  output
}

exclusive_lasso$pval <- changeSciNot(exclusive_lasso$pval)

exclusive_lasso$Variable[duplicated(exclusive_lasso$Variable)] <- NA

exclusive_lasso$group <- NA
exclusive_lasso$group[1:2]<-"Baseline Characteristics"
exclusive_lasso$group[3:14]<-"Work"
exclusive_lasso$group[15:20] <- "Education"
exclusive_lasso$group[21:36]<-"Lifestyle"
exclusive_lasso$group[37:41]<- "Ethnicity"
exclusive_lasso$group[42:51]<-"Mental Health"
exclusive_lasso$group[52:71]<-"Physical Activity"
exclusive_lasso$group[72:76]<-"Environment"
exclusive_lasso$group[77:83]<-"Electronic Device Use"
exclusive_lasso$group[84:90]<-"Sleep"
exclusive_lasso$group[91:96]<-"Smoking"
exclusive_lasso$group[97:120]<-"Diet"
exclusive_lasso$group[121:124]<-"Alcohol"
exclusive_lasso$group[125:133]<-"Sun Exposure"
exclusive_lasso$group[134:140]<-"Early life"

exclusive_lasso$group[duplicated(exclusive_lasso$group)] <- NA

library(tibble)
exclusive_data <- tibble(mean = c(print(exclusive_lasso$coef)),
                         lower = c(exclusive_lasso$`95% CI lower`),
                         upper = c(exclusive_lasso$`95% CI upper`),
                         category = exclusive_lasso$group,
                         variable = exclusive_lasso$Variable,
                         env_factors = exclusive_lasso$level,
                         coef_CI = c(exclusive_lasso$ci_interval),
                         pval = c(as.character(exclusive_lasso$pval)))

header <- tibble(category = "Group",
                 variable = "Variable",
                 env_factors = c("Level"),
                 coef_CI = c("Coefficient (95% CI)"),
                 pval = c("P-value"),
                 summary=TRUE)



forest <- bind_rows(header, exclusive_data)

library(forestplot)

forest%>% 
  forestplot(labeltext = c(category, variable, env_factors, coef_CI, pval),
             is.summary = summary,
             clip = c(-Inf, Inf), 
             xlog = FALSE,
             lwd.xaxis = 3,
             lwd.ci = 3,
             boxsize = 0.2,
             title = 'Exclusive LASSO Model of Stably-selected Features against LTL',
             graph.pos = 5,
             colgap = unit(0.0005, 'npc'),
             xlab = 'Coefficient',
             hrzl_lines = list(NULL,gpar(col="#444444"),
                               NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, NULL, gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL, gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL),
             txt_gp = fpTxtGp(label = list(gpar(fontface='italic', cex=1.5),gpar(col='dimgrey', cex=1.2),NULL,NULL,NULL,NULL,NULL,NULL,NULL),
                              ticks = gpar(cex=1),
                              xlab = gpar(cex=1),
             ),
             col = fpColors(box = "blue",
                            lines = "red4"),
             fn.ci_norm = fpDrawDiamondCI)


