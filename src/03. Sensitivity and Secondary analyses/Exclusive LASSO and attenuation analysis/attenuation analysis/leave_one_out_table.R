m1<- model1_demographics
m2<- model2_demographics_social
m3<- model3_demographics_social_health_risk_factors
m4<- model4_all

library(tidyverse)
library(docxtools)
library(knitr)
library(dplyr)

# getting the row name as variable name
m1 <- tibble::rownames_to_column(m1, "Variable")

m1$Variable<- gsub(".", " ",m1$Variable, fixed = TRUE)
m1$Variable<- gsub("`", "",m1$Variable, fixed = TRUE)

m1 <- m1[, c(1,2,4,5,6)]

m1 <- m1 %>%
  format_engr(sigdig = 3)

m1$CI <- paste(m1$`95% CI lower`, m1$`95% CI upper`, sep=",")
m1$CI <- paste(m1$`coef`, m1$`CI`, sep="[")

m1$a <- "]"
m1$CI <- paste(m1$`CI`, m1$`a`)

m1 <- m1[, c(1,6,3)]

m2 <- tibble::rownames_to_column(m2, "Variable")
m2$Variable<- gsub(".", " ",m2$Variable, fixed = TRUE)
m2$Variable<- gsub("`", "",m2$Variable, fixed = TRUE)

m2 <- m2[, c(1,2,4,5,6)]

m2 <- m2 %>%
  format_engr(sigdig = 3)

m2$CI <- paste(m2$`95% CI lower`, m2$`95% CI upper`, sep=",")
m2$CI <- paste(m2$`coef`, m2$`CI`, sep="[")

m2$a <- "]"
m2$CI <- paste(m2$`CI`, m2$`a`)

m2 <- m2[, c(1,6,3)]

m1_2 <- dplyr::right_join(m1, m2,"Variable")

m3 <- tibble::rownames_to_column(m3, "Variable")
m3$Variable<- gsub(".", " ",m3$Variable, fixed = TRUE)
m3$Variable<- gsub("`", "",m3$Variable, fixed = TRUE)

m3 <- m3[, c(1,2,4,5,6)]

m3 <- m3 %>%
  format_engr(sigdig = 3)

m3$CI <- paste(m3$`95% CI lower`, m3$`95% CI upper`, sep=",")
m3$CI <- paste(m3$`coef`, m3$`CI`, sep="[")

m3$a <- "]"
m3$CI <- paste(m3$`CI`, m3$`a`)

m3 <- m3[, c(1,6,3)]

m1_2_3 <- dplyr::right_join(m1_2, m3,"Variable")

m4 <- tibble::rownames_to_column(m4, "Variable")
m4$Variable<- gsub(".", " ",m4$Variable, fixed = TRUE)
m4$Variable<- gsub("`", "",m4$Variable, fixed = TRUE)

m4 <- m4[, c(1,2,4,5,6)]

m4 <- m4 %>%
  format_engr(sigdig = 3)

m4$CI <- paste(m4$`95% CI lower`, m4$`95% CI upper`, sep=",")
m4$CI <- paste(m4$`coef`, m4$`CI`, sep="[")

m4$a <- "]"
m4$CI <- paste(m4$`CI`, m4$`a`)

m4 <- m4[, c(1,6,3)]

m_all <- dplyr::right_join(m1_2_3, m4,"Variable")


kable(m_all)

