library(ggh4x)
library(ggplot2)
library(scales)
library(ggpubr)

the.data <- read.csv("POLYPLEX_moDCs.csv", head = TRUE)
the.data <- the.data[!duplicated(the.data),]
the.data$Condition <- ordered(the.data$Condition, levels = c("10:1", "7.5:1", "5:1", "2.5:1", "1:1", "LPF3000", "free mRNA", "Blank"))
the.data$Target.Name <- ordered(the.data$Target_Name, levels=c("Cells"))
the.data$Value <- as.numeric(the.data$Value)

# Reorder Group so "Polyplex" is on the left (first)
the.data$Group <- factor(the.data$Group, levels = c("Polyplex", setdiff(unique(the.data$Group), "Polyplex")))
my_comparisons <- list( c("free mRNA", "7.5:1"), c("LPF3000", "10:1"))
    
p <- ggplot(data=subset(the.data, Target_Name == "Cells"), aes(x=Condition, y=Value)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), width = 0.7, fill = "white", color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = position_dodge(.7)) +
  stat_compare_means(comparisons = my_comparisons,method = "t.test")+
  geom_point(aes(color = Condition), position = position_jitter(width = 0.2), size = 1.5) +
  scale_fill_manual(values = c("Blank" = "#03d82d", "free mRNA" = "#8f1c07a6", "LPF3000" = "#ec6504", "10:1" = "#3e3a3ad6", "7.5:1" = "#851387", "5:1" = "#ec052c", "2.5:1" = "#070ee6", "1:1" = "#f30487")) +
  scale_color_manual(values = c("Blank" = "#03d82d", "free mRNA" = "#8f1c07a6", "LPF3000" = "#ec6504", "10:1" = "#3e3a3ad6", "7.5:1" = "#851387", "5:1" = "#ec052c", "2.5:1" = "#070ee6", "1:1" = "#f30487")) +
  scale_y_continuous(oob = rescale_none) +
  ylab("% eGFP+ cells among live") +
  xlab("") +
  theme_light() +
  theme(axis.text = element_text(size = 14),
    plot.title = element_text(size = 20),
    axis.title= element_text(size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    strip.placement = "top",              
    strip.background = element_blank(),
    strip.text.x = element_text(size = 16, face = "bold", color = "black")
  )
p


ggsave("Polyplex_moDCs_20h_NEW_Take2.png",plot=p,width=4.1,height = 4.6,dpi = 300)
