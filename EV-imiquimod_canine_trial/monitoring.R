library(ggplot2)
library(patchwork)

whole_data <- read.csv("Test_Data.csv")
what_patient <- "P01.XX"
dat <- whole_data[whole_data$Patient_ID == what_patient,]

my_theme <- theme(strip.background=element_rect(fill="gray50",color="gray20"),
  strip.text.x=element_text(size=14,color="white"),
  strip.text.y=element_text(size=14,color="white"))+
  theme(axis.title= element_text(size = 18))+
  theme(axis.text = element_text(size = 14))+
  theme(axis.ticks.length=unit(0.2,"cm"))

#-- Examination
p_exam <- ggplot(data=dat[dat$Condition == "Examination",],aes(Week,Value))+
  geom_line(aes(group=Patient_ID),color="#0a0573")+
  geom_point()+
  ylab("Weight (kg)")+
  theme_light()+
  my_theme
p_exam

#-- Temp
temp_df <- dat
temp_df$Parameter <- ordered(temp_df$Parameter,levels=c("Baseline","0.5h","1h","3h","6h","24h"))
p_temp1 <- ggplot(data=temp_df[temp_df$Condition == "Temp" & temp_df$Week %in% paste("Week",1:3),],aes(Parameter,Value))+
  facet_grid(~Week)+
  geom_line(aes(group=Patient_ID),color="#0a0573")+
  geom_point()+
  ylab("Temperature (F)")+
  xlab("")+
  theme_light()+
  my_theme

p_temp2 <- ggplot(data=temp_df[temp_df$Condition == "Temp" & temp_df$Parameter == "Baseline",],aes(Week,Value))+
  geom_line(aes(group=Patient_ID),color="#0a0573")+
  geom_point()+
  ylab("Temperature (F)")+
  xlab("")+
  theme_light()+
  my_theme

p <- p_temp1/p_temp2
p

#-- TNFa
p_tnfa <- ggplot(data=temp_df[temp_df$Condition == "TNFa" & temp_df$Week %in% paste("Week",1:3),],aes(Parameter,Value))+
  facet_grid(~Week)+
  geom_line(aes(group=Patient_ID),color="#0a0573")+
  geom_point()+
  ylab("TNFa (pg/mL)")+
  xlab("")+
  theme_light()+
  my_theme
p_tnfa

#-- CBC
p_cbc <- ggplot(data=dat[dat$Condition == "CBC",],aes(Week,Value))+
  facet_wrap(~Parameter,scales = "free_y")+
  geom_line(aes(group=Patient_ID),color="#0a0573")+
  geom_point()+
  ylab("Temperature (F)")+
  xlab("")+
  theme_light()+
  my_theme
p_cbc

