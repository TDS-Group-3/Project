library(tidyverse)
library(docxtools)
library(knitr)
library(dplyr)
library(stringr)
group_lasso <- `model_linear_group_lasso2-2`
group_lasso <- tibble::rownames_to_column(group_lasso, "Variable")

group_lasso$Variable <- gsub("`", "", group_lasso$Variable)
group_lasso$Variable <- gsub(".", " ",group_lasso$Variable , fixed = TRUE)
group_lasso$level <- str_split_fixed(group_lasso$Variable, "_", 2)[,2]

group_lasso$Variable <- str_split_fixed(group_lasso$Variable, "_", 2)[,1]


group_lasso$`95% CI lower` <- round(group_lasso$`95% CI lower`, 3)
group_lasso$`95% CI upper` <- round(group_lasso$`95% CI upper`, 3)
group_lasso$coef <- round(group_lasso$coef, 3)

group_lasso$ci_interval_lower <- paste0("[", group_lasso$`95% CI lower`)
group_lasso$ci_interval_upper <- paste0(group_lasso$`95% CI upper`, "]")
group_lasso$ci_interval <- paste(group_lasso$ci_interval_lower, group_lasso$ci_interval_upper, sep=" , ")

group_lasso$ci_interval <- paste(group_lasso$coef, group_lasso$ci_interval)

changeSciNot <- function(n) {
  output <- formatC(n, format='e', digits=2) #Transforms the number into scientific notation even if small
  output <- sub("e", "x10^", output) #Replace e with 10^
  output
}

group_lasso$pval <- changeSciNot(group_lasso$pval)

group_lasso$Variable[duplicated(group_lasso$Variable)] <- NA

group_lasso$group <- NA
group_lasso$group[1:2]<-"Baseline Characteristics"
group_lasso$group[3:18] <- "Education"
group_lasso$group[19:28]<-"Lifestyle"
group_lasso$group[29]<-"Mental Health"
group_lasso$group[30:37]<-"Physical Activity"
group_lasso$group[38:41]<-"Sleep"
group_lasso$group[42:46]<-"Smoking"
group_lasso$group[47:82]<-"Diet"
group_lasso$group[83:86]<-"Alcohol"
group_lasso$group[87:104]<-"Sun Exposure"
group_lasso$group[105:120]<-"Early life"

group_lasso$group[duplicated(group_lasso$group)] <- NA

library(tibble)
group_lasso_data <- tibble(mean = c(print(group_lasso$coef)),
                         lower = c(group_lasso$`95% CI lower`),
                         upper = c(group_lasso$`95% CI upper`),
                         category = group_lasso$group,
                         variable = group_lasso$Variable,
                         env_factors = group_lasso$level,
                         coef_CI = c(group_lasso$ci_interval),
                         pval = c(as.character(group_lasso$pval)))

header <- tibble(category = "Group",
                 variable = "Variable",
                 env_factors = c("Level"),
                 coef_CI = c("Coefficient (95% CI)"),
                 pval = c("P-value"),
                 summary=TRUE)



forest_group <- bind_rows(header, group_lasso_data)

library(forestplot)

forest_group%>% 
  forestplot(labeltext = c(category, variable, env_factors, coef_CI, pval),
             is.summary = summary,
             clip = c(-Inf, Inf), 
             xlog = FALSE,
             lwd.xaxis = 3,
             lwd.ci = 3,
             boxsize = 0.2,
             title = 'Group LASSO Model of Stably-selected Features against LTL',
             graph.pos = 5,
             colgap = unit(0.0005, 'npc'),
             xlab = 'Coefficient',
             hrzl_lines = list(NULL,gpar(col="#444444"),
                               NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL, gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL),
             txt_gp = fpTxtGp(label = list(gpar(fontface='italic', cex=1.5),gpar(col='dimgrey', cex=1.2),NULL,NULL,NULL,NULL,NULL,NULL,NULL),
                              ticks = gpar(cex=1),
                              xlab = gpar(cex=1),
             ),
             col = fpColors(box = "blue",
                            lines = "red4"),
             fn.ci_norm = fpDrawDiamondCI)

