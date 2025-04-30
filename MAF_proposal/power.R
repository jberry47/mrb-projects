library(ggplot2)

df <- data.frame("reps"=3:10,"power"=sapply(3:10,function(nreps){
  back <- sapply(1:100,function(i){
    group1 <- MASS::rnegbin(nreps, mu=5400000, theta=2*500000)
    group2 <- MASS::rnegbin(nreps, mu=100, theta=2*25)
    difference <- group1 - group2
    the_test <- suppressWarnings(summary(MASS::glm.nb(difference ~ 1,init.theta = 100000)))
    the_test$coefficients[4]
  })
  mean(back < 0.05)
}))

df <- data.frame("reps"=3:10,"power"=sapply(3:10,function(nreps){
  back <- sapply(1:10000,function(i){
    group1 <- rnorm(nreps, mean=5400000, sd=500000)
    group2 <- rnorm(nreps, mean=100, sd=25)
    difference <- group1 - group2
    t.test(difference, alternative="greater")$p.value
  })
  mean(back < 0.01)
}))

p <- ggplot(df, aes(x=reps, y=power)) +
  geom_point() +
  geom_line() +
  theme_light()
p
