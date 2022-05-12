library(tidyverse)
library(docxtools)
library(knitr)
library(dplyr)
library(stringr)
library(tibble)

m4 <- `model4_all`

m4 <- tibble::rownames_to_column(m4, "Variable")

m4$Variable <- gsub("`", "", m4$Variable)
m4$Variable <- gsub(".", " ", m4$Variable , fixed = TRUE)
m4$level <- str_split_fixed(m4$Variable, "_", 2)[,2]

m4$Variable <- str_split_fixed(m4$Variable, "_", 2)[,1]


m4$`95% CI lower` <- round(m4$`95% CI lower`, 3)
m4$`95% CI upper` <- round(m4$`95% CI upper`, 3)
m4$coef <- round(m4$coef, 3)

m4$ci_interval_lower <- paste0("[", m4$`95% CI lower`)
m4$ci_interval_upper <- paste0(m4$`95% CI upper`, "]")
m4$ci_interval <- paste(m4$ci_interval_lower, m4$ci_interval_upper, sep=" , ")

m4$ci_interval <- paste(m4$coef, m4$ci_interval)

changeSciNot <- function(n) {
  output <- formatC(n, format='e', digits=2) #Transforms the number into scientific notation even if small
  output <- sub("e", "x10^", output) #Replace e with 10^
  output
}

m4$pval <- changeSciNot(m4$pval)

m4$Variable[duplicated(m4$Variable)] <- NA

m4$group <- NA
m4$group[1:2]<-"Baseline Characteristics"
m4$group[3:9]<-"Early life"
m4$group[10:14]<-"Ethnicity"
m4$group[15:20] <- "Education"
m4$group[21:28] <- "Work"
m4$group[29:40] <- "Diet"
m4$group[41:45] <- "Lifestyle"
m4$group[46:55] <- "Physical Activity"
m4$group[56:65] <- "Mental Health"
m4$group[66:72] <- "Sleeping"
m4$group[73:76] <- "Alcohol"
m4$group[77:79] <- "Smoking"
m4$group[80:82] <- "Disability"
m4$group[83:89] <- "Electronic Device Use"
m4$group[90:94] <- "Environment"
m4$group[95:101] <- "Sun Exposure"

m4$group[duplicated(m4$group)] <- NA


data <- tibble(mean = c(print(m4$coef)),
               lower = c(m4$`95% CI lower`),
               upper = c(m4$`95% CI upper`),
               category = m4$group,
               variable = m4$Variable,
               env_factors = m4$level,
               coef_CI = c(m4$ci_interval),
               pval = c(as.character(m4$pval)))

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
             title = 'Multuvarire Linear Model of Stably-selected Features against LTL(Model 4)',
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
                               NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL),
             
             txt_gp = fpTxtGp(label = list(gpar(fontface='italic', cex=1.5),gpar(col='dimgrey', cex=1.2),NULL,NULL,NULL,NULL,NULL,NULL,NULL),
                              ticks = gpar(cex=1),
                              xlab = gpar(cex=1),
             ),
             col = fpColors(box = "blue",
                            lines = "red4"),
             fn.ci_norm = fpDrawDiamondCI)
