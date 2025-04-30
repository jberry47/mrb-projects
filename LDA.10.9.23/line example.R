library(ggplot2)

LDA <- read.csv("LDA.10.9.23.csv",header = T,stringsAsFactors = F)
head(LDA)


#*************************************************************************************************
# Data Visualization
#*************************************************************************************************
p <- ggplot(LDA,aes(Concentration,Value))+
  facet_wrap(~Name)+
  geom_line(aes(color=Treatment,group=Treatment),linewidth=1.25)+
  geom_point(aes(color=Treatment,size=Treatment))+
  theme_light()+
  scale_size_manual(values = rev(c(2.5,2,1.5)))+
  scale_x_continuous(label=scales::label_number_si())+
  ylab("Failure fraction (%)")+
  xlab("Cell number")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 13),
        plot.title = element_text(size = 20),
        axis.title= element_text(size = 18))+
  theme(strip.background=element_rect(fill="gray50",color="gray50"),
        strip.text.x=element_text(size=14,color="white"),
        strip.text.y=element_text(size=14,color="white"))
p
ggsave("LDA-100923.png",plot=p,width=11.2,height = 3.98,dpi = 300)          
