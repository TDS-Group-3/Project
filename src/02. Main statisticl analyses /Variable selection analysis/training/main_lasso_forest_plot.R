library(tidyverse)
library(docxtools)
library(knitr)
library(dplyr)
library(stringr)

lasso <- `model_linear_lasso-2`
lasso <- tibble::rownames_to_column(lasso, "Variable")

lasso$Variable <- gsub("`", "", lasso$Variable)
lasso$Variable <- gsub(".", " ",lasso$Variable , fixed = TRUE)
lasso$level <- str_split_fixed(lasso$Variable, "_", 2)[,2]

lasso$Variable <- str_split_fixed(lasso$Variable, "_", 2)[,1]


lasso$`95% CI lower` <- round(lasso$`95% CI lower`, 3)
lasso$`95% CI upper` <- round(lasso$`95% CI upper`, 3)
lasso$coef <- round(lasso$coef, 3)

lasso$ci_interval_lower <- paste0("[", lasso$`95% CI lower`)
lasso$ci_interval_upper <- paste0(lasso$`95% CI upper`, "]")
lasso$ci_interval <- paste(lasso$ci_interval_lower, lasso$ci_interval_upper, sep=" , ")

lasso$ci_interval <- paste(lasso$coef, lasso$ci_interval)

changeSciNot <- function(n) {
  output <- formatC(n, format='e', digits=2) #Transforms the number into scientific notation even if small
  output <- sub("e", "x10^", output) #Replace e with 10^
  output
}

lasso$pval <- changeSciNot(lasso$pval)

lasso$Variable[duplicated(lasso$Variable)] <- NA

lasso$group <- NA
lasso$group[1:3]<-"Baseline Characteristics"
lasso$group[4:5] <- "Education"
lasso$group[6:9]<-"Lifestyle"
lasso$group[10:15]<- "Ethnicity"
lasso$group[16]<-"Mental Health"
lasso$group[17:20]<-"Physical Activity"
lasso$group[21]<-"Smoking"
lasso$group[22:29]<-"Diet"
lasso$group[30:32]<-"Alcohol"
lasso$group[33:35]<-"Sun Exposure"
lasso$group[36:39]<-"Early life"

lasso$group[duplicated(lasso$group)] <- NA

library(tibble)
lasso_data <- tibble(mean = c(print(lasso$coef)),
                         lower = c(lasso$`95% CI lower`),
                         upper = c(lasso$`95% CI upper`),
                         category = lasso$group,
                         variable = lasso$Variable,
                         env_factors = lasso$level,
                         coef_CI = c(lasso$ci_interval),
                         pval = c(as.character(lasso$pval)))

header <- tibble(category = "Group",
                 variable = "Variable",
                 env_factors = c("Level"),
                 coef_CI = c("Coefficient (95% CI)"),
                 pval = c("P-value"),
                 summary=TRUE)



forest_lasso <- bind_rows(header, lasso_data)

library(forestplot)

forest_lasso%>% 
  forestplot(labeltext = c(category, variable, env_factors, coef_CI, pval),
             is.summary = summary,
             clip = c(-Inf, Inf), 
             xlog = FALSE,
             lwd.xaxis = 3,
             lwd.ci = 3,
             boxsize = 0.2,
             title = 'LASSO Model of Stably-selected Features against LTL',
             graph.pos = 5,
             colgap = unit(0.0005, 'npc'),
             xlab = 'Coefficient',
             hrzl_lines = list(NULL,gpar(col="#444444"),
                               NULL,NULL,gpar(col="#000044"),
                               NULL, gpar(col="#000044"),
                               NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),gpar(col="#000044"),
                               NULL,NULL,NULL,gpar(col="#000044"),
                               gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL),
             txt_gp = fpTxtGp(label = list(gpar(fontface='italic', cex=1.5),gpar(col='dimgrey', cex=1.2),NULL,NULL,NULL,NULL,NULL,NULL,NULL),
                              ticks = gpar(cex=1),
                              xlab = gpar(cex=1),
             ),
             col = fpColors(box = "blue",
                            lines = "red4"),
             fn.ci_norm = fpDrawDiamondCI)

