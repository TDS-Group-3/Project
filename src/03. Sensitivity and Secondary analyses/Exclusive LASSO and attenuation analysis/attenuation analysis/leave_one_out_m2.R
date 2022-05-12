library(tidyverse)
library(docxtools)
library(knitr)
library(dplyr)
library(stringr)
library(tibble)

m2 <- `model2_demographics_social`

m2 <- tibble::rownames_to_column(m2, "Variable")

m2$Variable <- gsub("`", "", m2$Variable)
m2$Variable <- gsub(".", " ", m2$Variable , fixed = TRUE)
m2$level <- str_split_fixed(m2$Variable, "_", 2)[,2]

m2$Variable <- str_split_fixed(m2$Variable, "_", 2)[,1]


m2$`95% CI lower` <- round(m2$`95% CI lower`, 3)
m2$`95% CI upper` <- round(m2$`95% CI upper`, 3)
m2$coef <- round(m2$coef, 3)

m2$ci_interval_lower <- paste0("[", m2$`95% CI lower`)
m2$ci_interval_upper <- paste0(m2$`95% CI upper`, "]")
m2$ci_interval <- paste(m2$ci_interval_lower, m2$ci_interval_upper, sep=" , ")

m2$ci_interval <- paste(m2$coef, m2$ci_interval)

changeSciNot <- function(n) {
  output <- formatC(n, format='e', digits=2) #Transforms the number into scientific notation even if small
  output <- sub("e", "x10^", output) #Replace e with 10^
  output
}

m2$pval <- changeSciNot(m2$pval)

m2$Variable[duplicated(m2$Variable)] <- NA

m2$group <- NA
m2$group[1:2]<-"Baseline Characteristics"
m2$group[3:9]<-"Early life"
m2$group[10:14]<-"Ethnicity"
m2$group[15:20] <- "Education"
m2$group[21:28] <- "Work"
m2$group[29:40] <- "Diet"
m2$group[41:45] <- "Lifestyle"


m2$group[duplicated(m2$group)] <- NA


data <- tibble(mean = c(print(m2$coef)),
               lower = c(m2$`95% CI lower`),
               upper = c(m2$`95% CI upper`),
               category = m2$group,
               variable = m2$Variable,
               env_factors = m2$level,
               coef_CI = c(m2$ci_interval),
               pval = c(as.character(m2$pval)))

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
             title = 'Multuvarire Linear Model of Stably-selected Features against LTL(Model 2)',
             graph.pos = 5,
             colgap = unit(0.0005, 'npc'),
             xlab = 'Coefficient',
             hrzl_lines = list(NULL,gpar(col="#444444"),
                               NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL),
             
             txt_gp = fpTxtGp(label = list(gpar(fontface='italic', cex=1.5),gpar(col='dimgrey', cex=1.2),NULL,NULL,NULL,NULL,NULL,NULL,NULL),
                              ticks = gpar(cex=1),
                              xlab = gpar(cex=1),
             ),
             col = fpColors(box = "blue",
                            lines = "red4"),
             fn.ci_norm = fpDrawDiamondCI)
