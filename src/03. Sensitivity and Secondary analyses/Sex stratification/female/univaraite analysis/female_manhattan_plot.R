female <- univariate_linear_table_female
female <- tibble::rownames_to_column(female, "Variable")

library(ggplot2)
library(ggrepel)

female$Group<-NA
female$Group[1]<-"Baseline Characteristics"
female$Group[2:66]<-"Work"
female$Group[67:82]<-"Education"
female$Group[83:85]<-"Disability"
female$Group[86:113]<-"Lifestyle"
female$Group[114:133]<-"Ethnicity"
female$Group[134:191]<-"Mental Health"
female$Group[192:299]<-"Physical Activity"
female$Group[300:329]<-"Environment"
female$Group[330:350]<-"Electronic Device Use"
female$Group[351:371]<-"Sleep"
female$Group[372:390]<-"Smoking"
female$Group[391:496]<-"Diet"
female$Group[497:518]<-"Alcohol"
female$Group[519:540]<-"Sun Exposure"
female$Group[541:566]<-"Early life"
female$Group[567:606]<-"Female Specific"

group.colors = c("#999999", "#CC0000" , "#B45F06","#BF9000" , "#F1C232", "#FFE599",
                 "#B6D7A8","#93C47D","#6AA84F", "#38761D", "#76A5AF",
                 "#45818E", "#0B5394", "#073763", "#8e7cc3", "#674ea7","#351c75","#741b47")


ggplot(data=female,aes(x=1:nrow(female), y= ifelse(coef > 0, log10(pval), -log10(pval)),
                     color= ifelse(-log10(pval) > -log10(0.05/ncol(female)),Group, "Not significant")))+
  geom_point()+
  scale_color_manual(values = group.colors, breaks=c("Not significant", "Baseline Characteristics","Work","Education","Disability",
                                "Lifestyle","Ethnicity","Mental Health","Physical Activity",
                                "Environment","Electronic Device Use","Sleep",
                                "Smoking","Diet","Alcohol","Sun Exposure","Early life",
                                "Female Specific"))+
  labs(color="Variable group")+
  geom_hline(yintercept = log10(0.05/ncol(female)), col="red3") +
  annotate("text",x=0,y=3.75,color="red3",label="Bonferroni",size=3) +
  geom_hline(yintercept = -log10(0.05/ncol(female)), col="red3") +
  annotate("text",x=0,y=3.75,color="red3",label="Bonferroni",size=3)+
  geom_text_repel(aes(label=ifelse(pval<abs(log10(0.05/ncol(female))),Variable,"")),size=3) +
  labs(title="Female-Specific Manhattan plot of the association with LTL",
       x="Variable", y=expression(paste("-","lo", g[10], "(p)", sep = ""))) +
  theme_classic() + 
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(size = 20)) +
  theme(legend.position="right")






