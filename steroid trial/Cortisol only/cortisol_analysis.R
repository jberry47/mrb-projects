library(ggplot2)

df <- read.csv("cortisol.data.file.csv",stringsAsFactors = FALSE)
df$Cortisol.Value_f <- sapply(df$Cortisol.Value,function(i) if(i == "<lloq"){27.6}else{as.numeric(i)})
df$Cohort <- ordered(df$Cohort,levels=c("5-Day Cohort","14-Day Cohort"))
df$ACTH <- ordered(df$ACTH, levels=c("pre-ACTH","post-ACTH"))
df$Steroid <- ordered(df$Steroid,levels=c("Pre-Steroid Tx","Post-Steroid Tx"))

df_ref <- data.frame(
  "ACTH"=c(rep("pre-ACTH",4),rep("post-ACTH",4)),
  "Steroid"=c(rep("Pre-Steroid Tx",2),rep("Post-Steroid Tx",2),rep("Pre-Steroid Tx",2),rep("Post-Steroid Tx",2)),
  "Cohort"=rep(c("5-Day Cohort","14-Day Cohort"),times=4),
  "Lower"=c(rep(58,4),rep(225,4)),
  "Upper"=c(rep(144,4),rep(425,4))
)
df_ref$Cortisol.Value_f <- 0
df_ref$Patient <- "P00"
df_ref$Cohort <- ordered(df_ref$Cohort,levels=c("5-Day Cohort","14-Day Cohort"))
df_ref$ACTH <- ordered(df_ref$ACTH, levels=c("pre-ACTH","post-ACTH"))
df_ref$Steroid <- ordered(df_ref$Steroid,levels=c("Pre-Steroid Tx","Post-Steroid Tx"))


my_theme <- theme(strip.background=element_rect(fill="gray50",color="gray20"),
                  strip.text.x=element_text(size=14,color="white"),
                  strip.text.y=element_text(size=14,color="white"))+
            theme(axis.title= element_text(size = 18))+
            theme(axis.text = element_text(size = 14))+
            theme(axis.ticks.length=unit(0.2,"cm"))

p <- ggplot(df,aes(ACTH,Cortisol.Value_f,group=Patient))+
  facet_grid(Cohort~Steroid,scales = "free_x")+
  scale_x_discrete(expand = c(0.25,0))+
  geom_rect(data=df_ref,aes(xmin=as.numeric(ACTH)-0.1,xmax=as.numeric(ACTH)+0.1,ymin=Lower,ymax=Upper),fill="#f8cae3",color="#f8cae3")+
  geom_line(color="gray80")+
  geom_point(aes(color=Patient))+
  theme_light()+
  xlab("")+
  ylab("Cortisol (nmol/L)")+
  my_theme+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")
p
ggsave("steroid_trial_cortisol.png",dpi=300,plot=p,width = 5.22,height=5.06)






