library(tibble)
library(ggrepel)

univariate_linear_table <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/04_univariate_linear/univariate_linear_table.rds")

univariate_linear_table <- tibble::rownames_to_column(univariate_linear_table, "Variable")

group_list <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_3/final_data/group_list.rds")

#specifying variable type
univariate_linear_table$var_type<-NA
univariate_linear_table$var_type[1]<-"Baseline Characteristics"
univariate_linear_table$var_type[2:66]<-"Work"
univariate_linear_table$var_type[67:82]<-"Education"
univariate_linear_table$var_type[83:88]<-"Disability"
univariate_linear_table$var_type[89:116]<-"Lifestyle"
univariate_linear_table$var_type[117:136]<-"Ethnicity"
univariate_linear_table$var_type[137:200]<-"Mental Health"
univariate_linear_table$var_type[201:308]<-"Physical Activity"
univariate_linear_table$var_type[309:338]<-"Environment"
univariate_linear_table$var_type[339:359]<-"Electronic Device Use"
univariate_linear_table$var_type[360:380]<-"Sleep"
univariate_linear_table$var_type[381:399]<-"Smoking"
univariate_linear_table$var_type[400:505]<-"Diet"
univariate_linear_table$var_type[506:527]<-"Alcohol"
univariate_linear_table$var_type[528:549]<-"Sun Exposure"
univariate_linear_table$var_type[550:575]<-"Early life"

group.colors = c("#999999", "#CC0000" , "#B45F06","#BF9000" , "#F1C232", "#FFE599",
                 "#B6D7A8","#93C47D","#6AA84F", "#38761D", "#76A5AF",
                 "#45818E", "#0B5394", "#073763", "#8e7cc3", "#674ea7","#351c75")


ggplot(data=univariate_linear_table,aes(x=1:nrow(univariate_linear_table), y= ifelse(coef > 0, log10(pval), -log10(pval)),
                     color= ifelse(-log10(pval) > -log10(0.05/ncol(univariate_linear_table)),var_type, "Not significant")))+
  geom_point()+
  scale_color_manual(values = group.colors, breaks=c("Not significant", "Baseline Characteristics","Work","Education","Disability",
                                                     "Lifestyle","Ethnicity","Mental Health","Physical Activity",
                                                     "Environment","Electronic Device Use","Sleep",
                                                     "Smoking","Diet","Alcohol","Sun Exposure","Early life"))+
  labs(color="Variable group")+
  geom_hline(yintercept = log10(0.05/ncol(univariate_linear_table)), col="red3") +
  annotate("text",x=0,y=3.75,color="red3",label="Bonferroni",size=3) +
  geom_hline(yintercept = -log10(0.05/ncol(male)), col="red3") +
  annotate("text",x=0,y=3.75,color="red3",label="Bonferroni",size=3)+
  geom_text_repel(aes(label=ifelse(pval<abs(log10(0.05/ncol(male))),Variable,"")),size=3) +
  labs(title=" Manhattan plot of the association with LTL",
       x="Variable", y=expression(paste("-","lo", g[10], "(p)", sep = ""))) +
  theme_classic() + 
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(size = 20)) +
  theme(legend.position="right")
