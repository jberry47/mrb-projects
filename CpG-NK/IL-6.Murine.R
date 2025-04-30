library(plyr)
library(ggplot2)
library(car)
library(reshape2)
library(scales)
library(lme4)
library(stringr)
library(grid)
library(gridExtra)
library(lsmeans)
library(ggbeeswarm)
library(ggpubr)

CpG <- read.csv("Line.plots.csv",header = T,stringsAsFactors = F)
CpG$Treatment <- ordered(CpG$Treatment,levels=c("CpG ODN 2395"))
head(CpG)

options(rlang_backtrace_on_error = "none")

CpG <- read.csv("IL-6ELISAMurineSplenocyte.csv",stringsAsFactors = F)
CpG$Treatment <- ordered(CpG$Treatment,levels=c("Unstimulated","CpG 100","CpG 500","CpG 1000"))
CpG$UC <- CpG$Average+1.96*CpG$SEM
CpG$LC <- CpG$Average-1.96*CpG$SEM
CpG$Group <- sapply(CpG$Treatment,function(i) if(i == "Unstimulated"){"A"}else{"B"})
CpG_melt <- melt(CpG[,1:4])

p <- ggplot(CpG,aes(Treatment,Average))+
  geom_bar(stat = "identity",aes(fill=Group),show.legend = F,color="black",size=1.25)+
#  geom_errorbar(aes(ymin=UC,ymax=LC),width=0.5,size=2,color="black")+
  geom_errorbar(aes(ymin=UC,ymax=LC),width=0.5,size=1,color="black")+
  scale_fill_manual(values=c("red","#A24747"))+
#  geom_point(size=4)+
#  geom_point(size=2.5,color="white")+
  geom_point(data=CpG_melt,aes(Treatment,value,group=variable),position = position_dodge(width = 0.5),size=4,color="black")+
  geom_point(data=CpG_melt,aes(Treatment,value,group=variable),position = position_dodge(width = 0.5),size=2.5,color="white")+
#  geom_line()+
#  scale_x_log10()+
  theme_light()+
  ylab("IL-6 (pg/mL)")+
  xlab("nM")+
  ggtitle("Amazing title")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 20),
        axis.title= element_text(size = 18))+
  theme(strip.background=element_rect(fill="gray50"),
        strip.text.x=element_text(size=14,color="white"),
        strip.text.y=element_text(size=14,color="white"))+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))
p
ggsave("IL-6.line.plot.png",plot=p,width=4,height = 4.5,dpi = 300)
