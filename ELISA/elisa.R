##- Load libraries and make helper function
library(nplr)
library(ggplot2)

get_5PL_estimate <- function(mod,od_val){
  if(is.null(mod)){
    return(NA)
  }else{
    mod_ests <- getPar(mod)$params
    est_conc <- (mod_ests[3]-(1/mod_ests[4])*log10(((mod_ests[2]-mod_ests[1])/(od_val-mod_ests[1]))^(1/mod_ests[5])-1))
    return(as.numeric(est_conc))
  }
}

##-- Read in standard curve ODs and Conc, and sample ODs
dat <- read.csv("K9_IL-12_ELISA_7-31-25.csv")
samples <- read.csv("K9_IL-12_ELISA_7-31-25_samples.csv")

##-- Fit 5PL curve and estimate sample concentrations
mod <- suppressWarnings(suppressMessages(nplr(y=dat$ODC,x=dat$Conc,useLog = F,npars=5,method = "res",silent = T)))
samples$Concentration <- 2*sapply(samples$OD,function(i) get_5PL_estimate(mod,i))

##-- Make line of standard curve for plotting purposes
sd_curve <- na.omit(data.frame(
            "od"=seq(from=min(dat$ODC,na.rm = T),to=max(dat$ODC,na.rm = T),by=diff(range(dat$ODC))/1000),
            "conc"=sapply(seq(from=min(dat$ODC,na.rm = T),to=max(dat$ODC,na.rm = T),by=diff(range(dat$ODC))/1000),function(i) as.numeric(suppressWarnings(suppressMessages(get_5PL_estimate(mod,i)))))
))

##-- Extract model coefficients for plotting purposes
mod_ests <- round(getPar(mod)$params,2)
mod_lab <- paste0(
  "Bottom: ",mod_ests[1],"\n",
  "Top: ",mod_ests[2],"\n",
  "Xmid: ",mod_ests[3],"\n",
  "Slope: ",mod_ests[4],"\n",
  "Asymetric: ",mod_ests[5],"\n",
  "R2: ", round(mod@goodness$gof,4)
)
xpos <- max(min(sd_curve$conc,na.rm=T)+0.1*diff(range(sd_curve$conc,na.rm=T)),0.1*diff(range(sd_curve$conc,na.rm=T)))
ypos <- max(sd_curve$od,na.rm=T)-0.25*diff(range(sd_curve$od,na.rm=T))

##-- Plot of Standard Curve
p <- ggplot(sd_curve,aes(conc,od))+
  geom_line()+
  geom_point(data=dat,aes(Conc,ODC,color=Std),size=3)+
  theme_light()+
  labs(color="Standards")+
  ylab("OD")+
  xlab("Concentration (pg/mL)")+
  annotate("text",label=mod_lab,x=xpos,y=ypos)+
  scale_x_continuous(limits = c(0,1000))
p
ggsave("standard_curve.png",dpi=300,width = 6.8,height = 3.55)

##-- Plot of sample concentrations
p <- ggplot(samples,aes(as.character(Week),Concentration))+
  facet_grid(~Patient)+
  geom_bar(stat = "identity",aes(fill=as.character(Week)),position = position_dodge(width = 0.75),width=0.75)+
  geom_line(aes(group=Patient))+
  theme_light()+
  ylab("IL-12 (pg/mL)")+
  xlab("Week")+
  labs(color="Standards")+
  theme(legend.position = "none")
p
ggsave("sample_estimates.png",dpi=300,width = 10.1,height = 3.55)
