library(ggplot2)
library(patchwork)

whole_data <- read.csv("Test_Data.csv")

#-- No touchy from here down ---
whole_data$Parameter <- trimws(whole_data$Parameter)
whole_data$Parameter <- gsub("Baseline","Pre-Tx",whole_data$Parameter)

my_theme <- theme(strip.background=element_rect(fill="gray50",color="gray20"),
  strip.text.x=element_text(size=14,color="white"),
  strip.text.y=element_text(size=14,color="white"))+
  theme(axis.title= element_text(size = 18))+
  theme(axis.text = element_text(size = 14))+
  theme(axis.ticks.length=unit(0.2,"cm"))

for(what_patient in unique(whole_data$Patient_ID)){
  dir.create(what_patient, showWarnings = FALSE)
  
  dat <- whole_data[whole_data$Patient_ID == what_patient,]
  temp_df <- dat
  temp_df$Parameter <- ordered(temp_df$Parameter,levels=c("Pre-Tx","0.5h","1h","3h","6h","24h"))

  #-- Examination
  upper_limit <- dat$Value[dat$Condition == "Examination" & dat$Week == "Week 1"]*1.1
  lower_limit <- dat$Value[dat$Condition == "Examination" & dat$Week == "Week 1"]*0.9
  p_exam <- ggplot(data=dat[dat$Condition == "Examination",],aes(Week,Value))+
    geom_rect(aes(xmin=1,xmax=6,ymin=upper_limit,ymax=Inf),fill="#f7cbcb")+
    geom_rect(aes(xmin=1,xmax=6,ymin=-Inf,ymax=lower_limit),fill="#f7cbcb")+
    geom_line(aes(group=Patient_ID),color="#0a0573")+
    geom_point()+
    ylab("Weight (kg)")+
    xlab("")+
    theme_light()+
    my_theme+
    theme(axis.text.x = element_text(angle=45,hjust=1))
  ggsave(paste0(what_patient,"/weight-",what_patient,".png"),plot=p_exam,dpi=300,width=3.5,height = 2.89)
  
  #-- Temp
  p_temp1 <- ggplot(data=temp_df[temp_df$Condition == "Temp" & temp_df$Week %in% paste("Week",1:3),],aes(Parameter,Value))+
    facet_grid(~Week)+
    geom_rect(aes(xmin=1,xmax=6,ymin=103,ymax=Inf),fill="#f7cbcb")+
    geom_rect(aes(xmin=1,xmax=6,ymin=-Inf,ymax=100.5),fill="#f7cbcb")+
    geom_line(aes(group=Patient_ID),color="#0a0573")+
    geom_point()+
    ylab("Temperature (F)")+
    xlab("")+
    theme_light()+
    my_theme+
    theme(axis.text.x = element_text(angle=45,hjust=1))
  ggsave(paste0(what_patient,"/temp_tx-",what_patient,".png"),plot=p_temp1,dpi=300,width=7.35,height = 2.89)
  
  p_temp2 <- ggplot(data=temp_df[temp_df$Condition == "Temp" & temp_df$Parameter == "Pre-Tx",],aes(Week,Value))+
    geom_rect(aes(xmin=1,xmax=6,ymin=103,ymax=Inf),fill="#f7cbcb")+
    geom_rect(aes(xmin=1,xmax=6,ymin=-Inf,ymax=100.5),fill="#f7cbcb")+
    geom_line(aes(group=Patient_ID),color="#0a0573")+
    geom_point()+
    ylab("Temperature (F)")+
    xlab("")+
    theme_light()+
    my_theme+
    theme(axis.text.x = element_text(angle=45,hjust=1))
  ggsave(paste0(what_patient,"/temp_baseline-",what_patient,".png"),plot=p_temp2,dpi=300,width=3.5,height = 2.89)
  
  #-- TNFa
  p_tnfa <- ggplot(data=dat[dat$Condition == "TNFa" & dat$Week %in% paste("Week",1:3),],aes(Parameter,Value))+
    facet_grid(~Week)+
    geom_line(aes(group=Patient_ID),color="#0a0573")+
    geom_point()+
    ylab("TNFa (pg/mL)")+
    xlab("")+
    theme_light()+
    my_theme+
    theme(axis.text.x = element_text(angle=45,hjust=1))
  ggsave(paste0(what_patient,"/tnfa-",what_patient,".png"),plot=p_tnfa,dpi=300,width=7.35,height = 2.89)
    
  #-- CBC
  p_hct <- ggplot(data=dat[dat$Condition == "CBC" & dat$Parameter == "Hct",],aes(Week,Value))+
    geom_rect(aes(xmin=1,xmax=6,ymin=52,ymax=Inf),fill="#f7cbcb")+
    geom_rect(aes(xmin=1,xmax=6,ymin=-Inf,ymax=35),fill="#f7cbcb")+
    geom_line(aes(group=Patient_ID),color="#0a0573")+
    geom_point()+
    ylab("Hct (%)")+
    xlab("")+
    theme_light()+
    my_theme+
    theme(axis.text.x = element_text(angle=45,hjust=1))
  
  p_wbc <- ggplot(data=dat[dat$Condition == "CBC" & dat$Parameter == "WBC",],aes(Week,Value))+
    geom_rect(aes(xmin=1,xmax=6,ymin=17,ymax=Inf),fill="#f7cbcb")+
    geom_rect(aes(xmin=1,xmax=6,ymin=-Inf,ymax=6),fill="#f7cbcb")+
    geom_line(aes(group=Patient_ID),color="#0a0573")+
    geom_point()+
    ylab("WBC (k/uL)")+
    xlab("")+
    theme_light()+
    my_theme+
    theme(axis.text.x = element_text(angle=45,hjust=1))
  
  p_neut <- ggplot(data=dat[dat$Condition == "CBC" & dat$Parameter == "Neut.",],aes(Week,Value))+
    geom_rect(aes(xmin=1,xmax=6,ymin=11.5,ymax=Inf),fill="#f7cbcb")+
    geom_rect(aes(xmin=1,xmax=6,ymin=-Inf,ymax=3),fill="#f7cbcb")+
    geom_line(aes(group=Patient_ID),color="#0a0573")+
    geom_point()+
    ylab("Neut. (k/uL)")+
    xlab("")+
    theme_light()+
    my_theme+
    theme(axis.text.x = element_text(angle=45,hjust=1))
  
  p_lymph <- ggplot(data=dat[dat$Condition == "CBC" & dat$Parameter == "Lymph.",],aes(Week,Value))+
    geom_rect(aes(xmin=1,xmax=6,ymin=4.8,ymax=Inf),fill="#f7cbcb")+
    geom_rect(aes(xmin=1,xmax=6,ymin=-Inf,ymax=1),fill="#f7cbcb")+
    geom_line(aes(group=Patient_ID),color="#0a0573")+
    geom_point()+
    ylab("Lymph. (k/uL)")+
    xlab("")+
    theme_light()+
    my_theme+
    theme(axis.text.x = element_text(angle=45,hjust=1))
  
  p_plt <- ggplot(data=dat[dat$Condition == "CBC" & dat$Parameter == "Plt",],aes(Week,Value))+
    geom_rect(aes(xmin=1,xmax=6,ymin=700,ymax=Inf),fill="#f7cbcb")+
    geom_rect(aes(xmin=1,xmax=6,ymin=-Inf,ymax=200),fill="#f7cbcb")+
    geom_line(aes(group=Patient_ID),color="#0a0573")+
    geom_point()+
    ylab("Plt (k/uL)")+
    xlab("")+
    theme_light()+
    my_theme+
    theme(axis.text.x = element_text(angle=45,hjust=1))
  p <- (p_hct + p_plt + p_wbc + p_neut + p_lymph) + plot_layout(ncol = 3)
  ggsave(paste0(what_patient,"/cbc-",what_patient,".png"),plot=p,width=8.56,height=4.94,dpi=300)

  #-- Chem
  p_bun <- ggplot(data=dat[dat$Condition == "Chem" & dat$Parameter == "BUN",],aes(Week,Value))+
    geom_rect(aes(xmin=1,xmax=6,ymin=30,ymax=Inf),fill="#f7cbcb")+
    geom_rect(aes(xmin=1,xmax=6,ymin=-Inf,ymax=6),fill="#f7cbcb")+
    geom_line(aes(group=Patient_ID),color="#0a0573")+
    geom_point()+
    ylab("BUN (mg/dL)")+
    xlab("")+
    theme_light()+
    my_theme+
    theme(axis.text.x = element_text(angle=45,hjust=1))

  p_cre <- ggplot(data=dat[dat$Condition == "Chem" & dat$Parameter == "CRE",],aes(Week,Value))+
    geom_rect(aes(xmin=1,xmax=6,ymin=1.5,ymax=Inf),fill="#f7cbcb")+
    geom_rect(aes(xmin=1,xmax=6,ymin=-Inf,ymax=0.5),fill="#f7cbcb")+
    geom_line(aes(group=Patient_ID),color="#0a0573")+
    geom_point()+
    ylab("CRE (mg/dL)")+
    xlab("")+
    theme_light()+
    my_theme+
    theme(axis.text.x = element_text(angle=45,hjust=1))

  p_alt <- ggplot(data=dat[dat$Condition == "Chem" & dat$Parameter == "ALT",],aes(Week,Value))+
    geom_rect(aes(xmin=1,xmax=6,ymin=65,ymax=Inf),fill="#f7cbcb")+
    geom_rect(aes(xmin=1,xmax=6,ymin=-Inf,ymax=8),fill="#f7cbcb")+
    geom_line(aes(group=Patient_ID),color="#0a0573")+
    geom_point()+
    ylab("ALT (U/L)")+
    xlab("")+
    theme_light()+
    my_theme+
    theme(axis.text.x = element_text(angle=45,hjust=1))

  p_alp <- ggplot(data=dat[dat$Condition == "Chem" & dat$Parameter == "ALP",],aes(Week,Value))+
    geom_rect(aes(xmin=1,xmax=6,ymin=92,ymax=Inf),fill="#f7cbcb")+
    geom_rect(aes(xmin=1,xmax=6,ymin=-Inf,ymax=7),fill="#f7cbcb")+
    geom_line(aes(group=Patient_ID),color="#0a0573")+
    geom_point()+
    ylab("ALP (U/L)")+
    xlab("")+
    theme_light()+
    my_theme+
    theme(axis.text.x = element_text(angle=45,hjust=1))

  p_alb <- ggplot(data=dat[dat$Condition == "Chem" & dat$Parameter == "ALB",],aes(Week,Value))+
    geom_rect(aes(xmin=1,xmax=6,ymin=3.8,ymax=Inf),fill="#f7cbcb")+
    geom_rect(aes(xmin=1,xmax=6,ymin=-Inf,ymax=2.5),fill="#f7cbcb")+
    geom_line(aes(group=Patient_ID),color="#0a0573")+
    geom_point()+
    ylab("ALB (g/dL)")+
    xlab("")+
    theme_light()+
    my_theme+
    theme(axis.text.x = element_text(angle=45,hjust=1))
  p <- (p_bun + p_cre + p_alt + p_alp + p_alb) + plot_layout(ncol = 3)
  ggsave(paste0(what_patient,"/chem-",what_patient,".png"),plot=p,width=8.56,height=4.94,dpi=300)
}

  

