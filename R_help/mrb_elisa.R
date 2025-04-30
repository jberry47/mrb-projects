##-- Setup
options(rlang_backtrace_on_error = "none")
pkgs <- c("ggplot2","scales","reshape2","plyr","brms","bayestestR","stringr","patchwork","plotly","DT","ggbeeswarm","ggpubr")
suppressMessages(invisible(sapply(pkgs, require, character.only = TRUE)))

library(bayesianUtils)

my_theme <- theme(strip.background=element_rect(fill="gray50",color="gray20"),
                  strip.text.x=element_text(size=14,color="white"),
                  strip.text.y=element_text(size=14,color="white"))+
            theme(axis.title= element_text(size = 18))+
            theme(axis.text = element_text(size = 14))+
            theme(axis.ticks.length=unit(0.2,"cm"))

##-- Read/format Data
dat <- read.csv("data/CitH3_ELISA_new.csv",stringsAsFactors = F)
dat$Group <- ordered(dat$Group, levels=c("Healthy","Cancer","Hospitalized.noncancer"),labels=c("Healthy","Cancer","Ill"))
head(dat)

##-- Proportion of data above LLOQ
props <- data.frame(do.call("rbind",lapply(split(dat,dat$Group),function(i){
  bayes_bernoulli(i$CitH3>0)$summary
})))
props$Group <- rownames(props)
props$Group <- ordered(props$Group, levels=c("Healthy","Cancer","Ill"),labels=c("Healthy","Cancer","Ill"))
rownames(props) <- NULL
dat$CitH3[dat$Group == "Healthy"] > 0

comps <- combn(unique(as.character(props$Group)),2)
df <- data.frame(t(apply(comps,MARGIN = 2,FUN=function(i){
  bayes_bernoulli(
    logical_vec1 = dat$CitH3[dat$Group == i[1]] > 0,
    logical_vec2 = dat$CitH3[dat$Group == i[2]] > 0)$summary
})))
df$group1 <- comps[1,]
df$group2 <- comps[2,]
df$y.position <- c(0.9,1,1.1)
df$p <- round(df$P.value,3)

p <- ggplot(data=props,aes(Group,HDE))+
  geom_bar(stat = "identity",fill="gray90",color="gray60",size=1)+
  geom_errorbar(aes(ymin=HDI_Low,ymax=HDI_High),width=0.5,size=1,color="gray30")+
  stat_pvalue_manual(data=df,label = "p")+
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1))+
  theme_light()+
  my_theme+
  ylab("Pr(Detecting H3Cit)")
p
ggsave("output/by_group_props.png",plot=p,dpi=300,width=4.7,height = 4.46)

##-- H3Cit by group
p <- ggplot(data=dat,aes(Group,CitH3))+
  geom_quasirandom(method = "smiley",width=0.25)+
  stat_compare_means(comparisons = list(c("Healthy","Cancer"),c("Healthy","Ill"),c("Cancer","Ill")))+
  theme_light()+
  my_theme
p
ggsave("output/by_group.png",plot=p,dpi=300,width=4.7,height = 4.46)

##-- H3Cit by Morph
sub <- dat[dat$Group != "Ill",]
sub$Morph <- ordered(sub$Morph,levels=c("None","Carcinoma","Round","Sarcoma","Miscellaneous"),labels=c("Healthy","Carcinoma","Round","Sarcoma","Miscellaneous"))
p <- ggplot(data=sub,aes(Morph,CitH3))+
  geom_quasirandom(method = "smiley",width=0.25)+
  stat_compare_means(ref.group = "Healthy")+
  theme_light()+
  my_theme
p
ggsave("output/morph_to_healthy.png",plot=p,dpi=300,width = 8.28,height = 4.46)

