library(tidyverse)
library(docxtools)
library(knitr)
library(dplyr)
library(ggplot2)
m1<- model1_demographics
m2<- model2_demographics_social
m3<- model3_demographics_social_health_risk_factors
m4<- model4_all


m1 <- tibble::rownames_to_column(m1, "Variable")
m1$group <- 'Model1'

m1$Variable<- gsub(".", " ",m1$Variable, fixed = TRUE)
m1$Variable<- gsub("`", "",m1$Variable, fixed = TRUE)

m1$`95% CI lower` <- round(m1$`95% CI lower`, 3)
m1$`95% CI upper` <- round(m1$`95% CI upper`, 3)
m1$coef <- round(m1$coef, 3)

m1 <- m1[, c(1,2,4,5,6,7)]

m1$Variable<-factor(m1$Variable, levels = m1$Variable)
-------------------------------------------------------------------------------
m2 <- tibble::rownames_to_column(m2, "Variable")
m2$group <- 'Model2'

m2$Variable<- gsub(".", " ",m2$Variable, fixed = TRUE)
m2$Variable<- gsub("`", "",m2$Variable, fixed = TRUE)

m2$`95% CI lower` <- round(m2$`95% CI lower`, 3)
m2$`95% CI upper` <- round(m2$`95% CI upper`, 3)
m2$coef <- round(m2$coef, 3)


m2 <- m2[, c(1,2,4,5,6,7)]

m2$Variable<-factor(m2$Variable, levels = m2$Variable)
--------------------------------------------------------------------------------
m3 <- tibble::rownames_to_column(m3, "Variable")
m3$group <- 'Model3'

m3$Variable<- gsub(".", " ",m3$Variable, fixed = TRUE)
m3$Variable<- gsub("`", "",m3$Variable, fixed = TRUE)

m3$`95% CI lower` <- round(m3$`95% CI lower`, 3)
m3$`95% CI upper` <- round(m3$`95% CI upper`, 3)
m3$coef <- round(m3$coef, 3)


m3 <- m3[, c(1,2,4,5,6,7)]

m3$Variable<-factor(m3$Variable, levels = m3$Variable)
--------------------------------------------------------------------------------
m4 <- tibble::rownames_to_column(m4, "Variable")
m4$group <- 'Model4'

m4$Variable<- gsub(".", " ",m4$Variable, fixed = TRUE)
m4$Variable<- gsub("`", "",m4$Variable, fixed = TRUE)

m4$`95% CI lower` <- round(m4$`95% CI lower`, 3)
m4$`95% CI upper` <- round(m4$`95% CI upper`, 3)
m4$coef <- round(m4$coef, 3)


m4 <- m4[, c(1,2,4,5,6,7)]

m4$Variable<-factor(m4$Variable, levels = m4$Variable)


df1<- rbind(m1,m2,m3,m4)


rects <- data.frame(x1=c("Age at recruitment","Current employment status_Full or part-time student",
                         "Number of days week of moderate physical activity 10plus minutes",
                         "Length of mobile phone use_Five to eight years"),
                    x2=c("Current employment status_Full or part-time student","Number of days week of moderate physical activity 10plus minutes",
                         "Length of mobile phone use_Five to eight years", "Facial ageing_Younger than you are"),
                    gr = c("Model1_demographic", "Model2_demographic_social",
                           "Model3_demographic_social_heath_risk",
                           "Model4_all"))
 

p = ggplot()+
  xlab('Variable') + ylab("Coefficient (95% Confidence Interval)") +
  geom_pointrange(data=df1,
                  aes(x = Variable, y = coef, ymin =`95% CI lower`, ymax = `95% CI upper`),size = 0.01)+
  geom_hline(aes(fill = Variable),yintercept = 0, linetype = 2)+
  geom_rect(data = rects, aes(xmin = x1, xmax = x2, ymin =-Inf, ymax = Inf, fill = gr), alpha = 0.4)+
  geom_errorbar(data=df1,
                aes(x = Variable, y = coef, ymin =`95% CI lower`, ymax = `95% CI upper`),width = 0.2,cex = 0.3)+ 
  facet_wrap(~group,strip.position ="left",nrow = 9,scales = "free_y") +
  theme(plot.title = element_text(size = 16,face = "bold"),
        legend.position ="none",
        panel.background = element_blank(),
        axis.text.y.right = element_text(vjust = 0.5, hjust = 1, face = "bold"),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1, face = "bold"),
        axis.title=element_text(size=20,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))
p


