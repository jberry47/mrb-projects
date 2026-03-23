library(ggplot2)
library(ggpubr)

dat <- read.csv("steroid.data.file.csv", header = TRUE, stringsAsFactors = FALSE)

for(measure in colnames(dat)[!colnames(dat) %in% c("Patient","Day")]){
  df <- na.omit(dat[,c("Patient","Day",measure)])
  
  my_measure <- sym(measure)
  
  my_ylab <- sapply(my_measure,function(i){
    if(i == "Pre-ACTH.cortisol"){"Cortisol (nmol/L)"}
    else if(i == "Post-ACTH.cortisol"){"Cortisol (nmol/L)"}
    else if(i == "eACTH"){"eACTH (pg/mL)"}
    else if(i == "BUN"){"BUN (mg/dL)"}
    else if(i == "ALP"){"ALP (U/L)"}
    else if(i == "cALP"){"cALP (U/L)"}
    else if(i == "ALT"){"ALT (U/L)"}
    else if(i == ""){""}
    else{NA}
  })

  p <- ggplot(df, aes(x=Day,y=!!my_measure))+
    geom_line(aes(group=Patient),size=1.5,color="gray80")+
    geom_point(aes(color=Patient),size=3)+
    stat_compare_means(method = "t.test",comparisons = my_comparisons)+
    theme_light()+
    ylab(my_ylab)+
    xlab("")+
    scale_x_discrete(breaks=my_breaks)+
    theme(axis.text = element_text(size = 18),
      plot.title = element_text(size = 20),
      axis.title= element_text(size = 18),
      axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "right")
  p
  ggsave(paste0(my_measure,".png"),width = my_width,height = 4.82,dpi=300)
}
