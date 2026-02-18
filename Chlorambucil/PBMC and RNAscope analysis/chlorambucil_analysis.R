library(ggplot2)
library(ggpubr)

dat <- read.csv("Compiled.data.file.csv", header = TRUE, stringsAsFactors = FALSE)

not_nice_measures <- c("X..CD146.","new_trait")

for(measure in colnames(dat)[!colnames(dat) %in% c("Patient","Day")]){
  df <- na.omit(dat[,c("Patient","Day",measure)])
  
  my_measure <- sym(measure)
  
  my_ylab <- sapply(my_measure,function(i){
    if(i == "Lymphocyte.count"){"Lymphocytes (cells/µL)"}
    else if(i == "X..T.cells"){"% T cells"}
    else if(i == "Abs..T.cells"){"Abs. T cells (cells/µL)"}
    else if(i == "X..Th.cells"){"% Th cells"}
    else if(i == "Abs..Th.cells"){"Abs. Th cells (cells/µL)"}
    else if(i == "X..CTLs"){"% CTLs"}
    else if(i == "Abs..CTLs"){"Abs. CTLs (cells/µL)"}
    else if(i == "X..Treg.cells"){"% Treg cells"}
    else if(i == "Abs..Treg.cells"){"Abs. Treg cells (cells/µL)"}
    else if(i == "X..Treg.CTL"){"% Treg:CTL"}
    else if(i == "Abs..Treg.CTL"){"Abs. Treg:CTL"}
    else if(i == "X..CTL.Treg"){"% CTL:Treg"}
    else if(i == "Abs..CTL.Treg"){"Abs. CTL:Treg"}
    else if(i == "X..CD146."){"% CD146+ cells"}
    else if(i == ""){""}
    else{NA}
  })

  if(measure %in% not_nice_measures){
    my_comparisons <- list(c("Day 0", "Day 42"))
    my_breaks <- c("Day 0", "Day 42")
    my_width <- 4.12
  }else{
    my_comparisons <- list(c("Day 0", "Day 21"), c("Day 0", "Day 42"), c("Day 21", "Day 42"))
    my_breaks <- c("Day 0","Day 21","Day 42")
    my_width <- 6.67
  }

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
