male <- univariate_linear_table_male  
male <- tibble::rownames_to_column(male, "Variable") 

library(ggplot2)
library(ggrepel)

male$Group<-NA
male$Group[1]<-"Baseline Characteristics"
male$Group[2:66]<-"Work"
male$Group[67:82]<-"Education"
male$Group[83:88]<-"Disability"
male$Group[89:116]<-"Lifestyle"
male$Group[117:136]<-"Ethnicity"
male$Group[137:200]<-"Mental Health"
male$Group[201:308]<-"Physical Activity"
male$Group[309:338]<-"Environment"
male$Group[339:359]<-"Electronic Device Use"
male$Group[360:380]<-"Sleep"
male$Group[381:399]<-"Smoking"
male$Group[400:505]<-"Diet"
male$Group[506:527]<-"Alcohol"
male$Group[528:549]<-"Sun Exposure"
male$Group[550:575]<-"Early life"
male$Group[576:586]<-"Male Specific" 


group.colors = c("#999999", "#CC0000" , "#B45F06","#BF9000" , "#F1C232", "#FFE599",
                 "#B6D7A8","#93C47D","#6AA84F", "#38761D", "#76A5AF",
                 "#45818E", "#0B5394", "#073763", "#8e7cc3", "#674ea7","#351c75","#741b47")


#manhatten plot
ggplot(data=male,aes(x=1:nrow(male), y= ifelse(coef > 0, log10(pval), -log10(pval)),
                       color= ifelse(-log10(pval) > -log10(0.05/ncol(male)),Group, "Not significant")))+
  geom_point()+
  scale_color_manual(values = group.colors, breaks=c("Not significant", "Baseline Characteristics","Work","Education","Disability",
                                                     "Lifestyle","Ethnicity","Mental Health","Physical Activity",
                                                     "Environment","Electronic Device Use","Sleep",
                                                     "Smoking","Diet","Alcohol","Sun Exposure","Early life",
                                                     "Male Specific"))+
  labs(color="Variable group")+
  geom_hline(yintercept = log10(0.05/ncol(male)), col="red3") +
  annotate("text",x=0,y=3.75,color="red3",label="Bonferroni",size=3) +
  geom_hline(yintercept = -log10(0.05/ncol(male)), col="red3") +
  annotate("text",x=0,y=3.75,color="red3",label="Bonferroni",size=3)+
  geom_text_repel(aes(label=ifelse(pval<abs(log10(0.05/ncol(male))),Variable,"")),size=3) +
  labs(title="Male-Specific Manhattan plot of the association with LTL",
       x="Variable", y=expression(paste("-","lo", g[10], "(p)", sep = ""))) +
  theme_classic() + 
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(size = 20)) +
  theme(legend.position="right")





