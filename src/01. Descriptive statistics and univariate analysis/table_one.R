## Table 1


library(table1)
library(htmlTable)
library(tidyverse)
library(kableExtra)

mydata <- readRDS('final_data/pre_imputation.rds')


pvalue <- function(x, ...) {
  x <- x[-length(x)]  # Remove "overall" group
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

pdf(file="Table1")
table1(~. | mydata$Sex, data = mydata, overall="Total", extra.col=list(`P-value`=pvalue), extra.col.pos=3)
dev.off()

