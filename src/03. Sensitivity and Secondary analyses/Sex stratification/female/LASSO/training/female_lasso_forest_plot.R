library(tidyverse)
library(docxtools)
library(knitr)
library(dplyr)
library(stringr)


my_data <- `model_linear_female_lasso-2`

# getting the row name as variable name
my_data <- tibble::rownames_to_column(my_data, "variable")

# duplicate a column called old_variable (for later group_list)
my_data$old_variable <- my_data$variable
my_data$old_variable <- gsub("`", "", my_data$old_variable)

# modify the row name
my_data$variable <- gsub("`", "", my_data$variable)
my_data$variable <- gsub(".", " ", my_data$variable , fixed = TRUE)

# get the level
my_data$level <- str_split_fixed(my_data$variable, "_", 2)[,2]

# getting rid of the level
my_data$variable <- str_split_fixed(my_data$variable, "_", 2)[,1]

# round the data
my_data$`95% CI lower` <- round(my_data$`95% CI lower`, 3)
my_data$`95% CI upper` <- round(my_data$`95% CI upper`, 3)
my_data$coef <- round(my_data$coef, 3)

# get 95% ci in the same column
my_data$ci_interval_lower <- paste0("[", my_data$`95% CI lower`)
my_data$ci_interval_upper <- paste0(my_data$`95% CI upper`, "]")
my_data$ci_interval <- paste(my_data$ci_interval_lower, my_data$ci_interval_upper, sep=" , ")

# get 95% ci and coef in the same column as ci_interval
my_data$ci_interval <- paste(my_data$coef, my_data$ci_interval)

# change p value to smaller e
changeSciNot <- function(n) {
  output <- formatC(n, format='e', digits=2) # Transforms the number into scientific notation even if small
  output <- sub("e", "x10^", output) # Replace e with 10^
  output
}
my_data$pval <- changeSciNot(my_data$pval)

# change duplicated variable name to NA
my_data$variable[duplicated(my_data$variable)] <- NA

# get group list 
group_list <- group_list
my_group <- subset(group_list[, c("group_name", "level_name")])
names(my_group)[names(my_group) == "level_name"] <- "old_variable"

# df: we have group name
df <- left_join(my_data, my_group, by = "old_variable")
df$group_name[df$group_name == "NULL"]  <- "Female Specific"
# df: remove duplicated group name
df$group_name[duplicated(df$group_name)] <- NA
df$group_name <- gsub("NA", "", df$group_name)

library(tibble)
forest_df <- tibble(mean = c(print(df$coef)),
                    lower = c(df$`95% CI lower`),
                    upper = c(df$`95% CI upper`),
                    Group = df$group_name,
                    Variable = df$variable,
                    Level = df$level,
                    Coefficient_95_CI = c(df$ci_interval),
                    P_value = c(as.character(df$pval)))

header <- tibble(Group = "Group",
                 Variable = "Variable",
                 Level = c("Level"),
                 Coefficient_95_CI = c("Coefficient (95% CI)"),
                 P_value = c("P-value"),
                 summary=TRUE)

forest <- bind_rows(header, forest_df)


library(forestplot)

forest %>% 
  forestplot(labeltext = c(Group, Variable, Level, Coefficient_95_CI, P_value),
             is.summary = summary,
             clip = c(-Inf, Inf), 
             xlog = FALSE,
             lwd.xaxis = 3,
             lwd.ci = 3,
             boxsize = 0.2,
             title = 'Female-specific LASSO Model of Stably-selected Features against LTL',
             graph.pos = 5,
             hrzl_lines = list(NULL,gpar(col="#444444"),
                               NULL, gpar(col="#000044"),
                               gpar(col="#000044"),
                               gpar(col="#000044"),
                               NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,gpar(col="#000044"),gpar(col="#000044"),
                               NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,gpar(col="#000044"),
                               NULL,NULL,NULL, gpar(col="#000044"),
                               NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL),
             colgap = unit(0.0005, 'npc'),
             xlab = 'Coefficient',
             txt_gp = fpTxtGp(label = list(gpar(fontface='italic', cex=1.5),gpar(col='dimgrey', cex=1.2),NULL,NULL,NULL,NULL,NULL,NULL,NULL),
                              ticks = gpar(cex=1),
                              xlab = gpar(cex=1)),
             col = fpColors(box = "blue",
                            lines = "red4"),
             fn.ci_norm = fpDrawDiamondCI)




