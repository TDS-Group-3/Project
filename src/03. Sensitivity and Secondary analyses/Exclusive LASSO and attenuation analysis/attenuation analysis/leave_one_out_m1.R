library(tidyverse)
library(docxtools)
library(knitr)
library(dplyr)
library(stringr)
library(tibble)

m1 <- `model1_demographics`

m1 <- tibble::rownames_to_column(m1, "Variable")

m1$Variable <- gsub("`", "", m1$Variable)
m1$Variable <- gsub(".", " ", m1$Variable , fixed = TRUE)
m1$level <- str_split_fixed(m1$Variable, "_", 2)[,2]

m1$Variable <- str_split_fixed(m1$Variable, "_", 2)[,1]


m1$`95% CI lower` <- round(m1$`95% CI lower`, 3)
m1$`95% CI upper` <- round(m1$`95% CI upper`, 3)
m1$coef <- round(m1$coef, 3)

m1$ci_interval_lower <- paste0("[", m1$`95% CI lower`)
m1$ci_interval_upper <- paste0(m1$`95% CI upper`, "]")
m1$ci_interval <- paste(m1$ci_interval_lower, m1$ci_interval_upper, sep=" , ")

m1$ci_interval <- paste(m1$coef, m1$ci_interval)

changeSciNot <- function(n) {
  output <- formatC(n, format='e', digits=2) #Transforms the number into scientific notation even if small
  output <- sub("e", "x10^", output) #Replace e with 10^
  output
}

m1$pval <- changeSciNot(m1$pval)

m1$Variable[duplicated(m1$Variable)] <- NA

m1$group <- NA
m1$group[1:2]<-"Baseline Characteristics"
m1$group[3:9]<-"Early life"
m1$group[10:14]<-"Ethnicity"
m1$group[15:20] <- "Education"


m1$group[duplicated(m1$group)] <- NA


data <- tibble(mean = c(print(m1$coef)),
                           lower = c(m1$`95% CI lower`),
                           upper = c(m1$`95% CI upper`),
                           category = m1$group,
                           variable = m1$Variable,
                           env_factors = m1$level,
                           coef_CI = c(m1$ci_interval),
                           pval = c(as.character(m1$pval)))

header <- tibble(category = "Group",
                 variable = "Variable",
                 env_factors = c("Level"),
                 coef_CI = c("Coefficient (95% CI)"),
                 pval = c("P-value"),
                 summary=TRUE)



forest_group <- bind_rows(header, data)

library(forestplot)

forest_group%>% 
  forestplot(labeltext = c(category, variable, env_factors, coef_CI, pval),
             is.summary = summary,
             clip = c(-Inf, Inf), 
             xlog = FALSE,
             lwd.xaxis = 3,
             lwd.ci = 3,
             boxsize = 0.2,
             title = 'Multuvarire Linear Model of Stably-selected Features against LTL(Model 1)',
             graph.pos = 5,
             colgap = unit(0.0005, 'npc'),
             xlab = 'Coefficient',
             hrzl_lines = list(NULL,gpar(col="#444444"),
                               NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL),
 
             txt_gp = fpTxtGp(label = list(gpar(fontface='italic', cex=1.5),gpar(col='dimgrey', cex=1.2),NULL,NULL,NULL,NULL,NULL,NULL,NULL),
                              ticks = gpar(cex=1),
                              xlab = gpar(cex=1),
             ),
             col = fpColors(box = "blue",
                            lines = "red4"),
             fn.ci_norm = fpDrawDiamondCI)
