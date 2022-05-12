library(tidyverse)
library(docxtools)
library(knitr)
library(dplyr)
library(stringr)
library(tibble)

m3 <- `model3_demographics_social_health_risk_factors`

m3 <- tibble::rownames_to_column(m3, "Variable")

m3$Variable <- gsub("`", "", m3$Variable)
m3$Variable <- gsub(".", " ", m3$Variable , fixed = TRUE)
m3$level <- str_split_fixed(m3$Variable, "_", 2)[,2]

m3$Variable <- str_split_fixed(m3$Variable, "_", 2)[,1]


m3$`95% CI lower` <- round(m3$`95% CI lower`, 3)
m3$`95% CI upper` <- round(m3$`95% CI upper`, 3)
m3$coef <- round(m3$coef, 3)

m3$ci_interval_lower <- paste0("[", m3$`95% CI lower`)
m3$ci_interval_upper <- paste0(m3$`95% CI upper`, "]")
m3$ci_interval <- paste(m3$ci_interval_lower, m3$ci_interval_upper, sep=" , ")

m3$ci_interval <- paste(m3$coef, m3$ci_interval)

changeSciNot <- function(n) {
  output <- formatC(n, format='e', digits=2) #Transforms the number into scientific notation even if small
  output <- sub("e", "x10^", output) #Replace e with 10^
  output
}

m3$pval <- changeSciNot(m3$pval)

m3$Variable[duplicated(m3$Variable)] <- NA

m3$group <- NA
m3$group[1:2]<-"Baseline Characteristics"
m3$group[3:9]<-"Early life"
m3$group[10:14]<-"Ethnicity"
m3$group[15:20] <- "Education"
m3$group[21:28] <- "Work"
m3$group[29:40] <- "Diet"
m3$group[41:45] <- "Lifestyle"
m3$group[46:55] <- "Physical Activity"
m3$group[56:65] <- "Mental Health"
m3$group[66:72] <- "Sleeping"
m3$group[73:76] <- "Alcohol"
m3$group[77:79] <- "Smoking"
m3$group[80:82] <- "Disability"



m3$group[duplicated(m3$group)] <- NA


data <- tibble(mean = c(print(m3$coef)),
               lower = c(m3$`95% CI lower`),
               upper = c(m3$`95% CI upper`),
               category = m3$group,
               variable = m3$Variable,
               env_factors = m3$level,
               coef_CI = c(m3$ci_interval),
               pval = c(as.character(m3$pval)))

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
             title = 'Multuvarire Linear Model of Stably-selected Features against LTL(Model 3)',
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
                               NULL,NULL,NULL),
             
             txt_gp = fpTxtGp(label = list(gpar(fontface='italic', cex=1.5),gpar(col='dimgrey', cex=1.2),NULL,NULL,NULL,NULL,NULL,NULL,NULL),
                              ticks = gpar(cex=1),
                              xlab = gpar(cex=1),
             ),
             col = fpColors(box = "blue",
                            lines = "red4"),
             fn.ci_norm = fpDrawDiamondCI)
