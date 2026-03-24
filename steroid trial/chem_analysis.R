library(ggplot2)
library(ggpubr)

dat <- read.csv("chem.data.file.csv", header = TRUE, stringsAsFactors = FALSE)

not_nice_measures <- c("eACTH","BUN","ALP","cALP","ALT")

# Define patient groups
patient_groups <- list(
  list(patients = paste0("P", sprintf("%02d", 1:20)), days = c("Day 0", "Day 5")),
  list(patients = paste0("P", sprintf("%02d", 21:40)), days = c("Day 0", "Day 14"))
)

for(group in patient_groups){
  dat_filtered <- dat[dat$Patient %in% group$patients, ]
  
  for(measure in colnames(dat_filtered)[!colnames(dat_filtered) %in% c("Patient","Day")]){
    df <- na.omit(dat_filtered[,c("Patient","Day",measure)])
    
    my_measure <- sym(measure)
    
    my_ylab <- sapply(my_measure,function(i){
      if(i == "eACTH"){"eACTH (pg/mL)"}
      else if(i == "BUN"){"BUN (mg/dL)"}
      else if(i == "ALP"){"ALP (U/L)"}
      else if(i == "cALP"){"cALP (U/L)"}
      else if(i == "ALT"){"ALT (U/L)"}
      else{NA}
    })

    my_comparisons <- list(c(group$days[1], group$days[2]))
    my_width <- 4.12

    p <- ggplot(df, aes(x=Day,y=!!my_measure))
    
    # Add shaded region for eACTH detection limit
    if(measure == "eACTH"){
      p <- p + geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=5), 
                         fill="#f8cae3", alpha=0.5, inherit.aes=FALSE)
    }
    
    p <- p +
      geom_line(aes(group=Patient),size=1.5,color="gray80")+
      geom_point(aes(color=Patient),size=3)+
      stat_compare_means(method = "t.test",comparisons = my_comparisons)+
      theme_light()+
      ylab(my_ylab)+
      xlab("")+
      scale_x_discrete(breaks=group$days)+
      theme(axis.text = element_text(size = 18),
        plot.title = element_text(size = 20),
        axis.title= element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1))+
      theme(legend.position = "right")
    
    ggsave(paste0(measure,"_",paste(group$patients[c(1,length(group$patients))],collapse="-"),".png"),
           width = my_width,height = 4.82,dpi=300)
  }
}
