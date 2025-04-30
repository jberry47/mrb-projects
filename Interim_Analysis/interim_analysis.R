library(ggplot2)
library(bayesianUtils)

dat <- read.csv("data/interim_data.csv")
head(dat)
str(dat)

#-- Quick look at the denisty distributions
p <- ggplot(dat,aes(hrs_2nd_normal))+
  geom_density(aes(fill=treatment_group),alpha=0.5)
p

#-- Specify priors to use in interim analysis
my_priors <- list(m = c(mean(dat$hrs_2nd_normal[dat$treatment_group == "Y"]), mean(dat$hrs_2nd_normal[dat$treatment_group == "Z"])), n = c(1, 1), s2 = c(var(dat$hrs_2nd_normal[dat$treatment_group == "Y"]), var(dat$hrs_2nd_normal[dat$treatment_group == "Z"])))

#-- Interim analysis with the data at hand
res <- bayes_normal(
  sample1 = dat$hrs_2nd_normal[dat$treatment_group == "Y"],
  sample2 = dat$hrs_2nd_normal[dat$treatment_group == "Z"],
  priors = my_priors,
  rope_int = c(-24,24),
  plot = T
)
res

#-- Simulation study using the collectioned data to get reps of both samples up to 27 per arm
sim <- lapply(1:2000,function(i){
  sim_sample1 <- rnorm(14, mean=res$updates$m[1],sqrt(res$updates$s2[1]))
  sim_sample2 <- rnorm(7, mean=res$updates$m[2],sqrt(res$updates$s2[2]))
  
  sim1 <- bayes_normal(
    sample1 = sim_sample1,
    sample2 = sim_sample2,
    priors = res$updates,
    rope_int = c(-24,24)
  )
  sim1
})

#-- Probaility that the difference of the two groups are not clinically actionable (difference is within 24hr interval)
the_answer <- mean(as.numeric(unlist(lapply(sim,function(i) i$summary["ROPE_perc"]))))
the_answer

#-- Conclusions
# Using the currently observed data to estimate the distibutions of the two treatments as priors, we draw samples from them to increase the total replicates per arm to 27, and this is done 2,000 times. In each iteration of the simulation, we estimate the percent chance that the difference between treatments is with a 24hr window. We summarize each iteration's percent chance using arithmatic mean to show that the overall percent chance of a non-clinically actionable outcome is 94.7%, in otherwords the probability of a clinically actionable outcome is only ~5%. 
