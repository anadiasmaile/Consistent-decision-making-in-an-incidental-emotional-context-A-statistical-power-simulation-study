library(ggplot2)
source("utilities_effect_size.R")

power <- c()


for (effectsize in e_pos ) {
  N_sim <- 1000
  sig_count <- 0
  e_cont <- 0.1
  e_pos <- seq(0,0.1,0.001)
  e_neg <- 0.1

  for (sim in 1:N_sim) {
    result <- single_simulation(N = 150, e_cont, effectsize, e_neg)
    sig_count <- sig_count + result
  }
  power<- append(power, sig_count/N_sim)
}

#create dataframe
powerdf<- data.frame (e_pos,power)

#create plot
ggplot (powerdf, aes( x = e_pos, y = power)) + geom_point (size=1) + labs ( x = "Effect Size", y = "Statistical Power") + theme_classic() + scale_y_continuous(breaks = seq(0,1,0.2)) + scale_x_continuous(breaks= seq(0,0.1,0.01))
