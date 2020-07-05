library(ggplot2)
source("utilities_effect_size1.R")

power <- c()
e_pos <- seq(0,0.1,0.001)


for (effectsize in e_pos ) {
N_sim <- 1000
sig_count <- 0
e_cont <- 0.1
e_neg <- 0.1

  for (sim in 1:N_sim) {
    result <- single_simulation(N = 150, e_cont, effectsize, e_neg)
    sig_count <- sig_count + result
  }
  power<- append(power, sig_count/N_sim)
}

source("utilities_effect_size2.R")


effect_total <- c()
e_cont <- 0.1
e_pos <- seq(0,0.1,0.001)
e_neg <- 0.1

for (effectsize in e_pos ) {
  result <- single_simulation(N = 9999, e_cont, effectsize, e_neg)
  effect_total <- append(effect_total, result)
}


#create dataframe
powerdf<- data.frame (effect_total,power)

#create plot
ggplot (powerdf, aes( x = effect_total, y = power)) + geom_point (size=1) + labs ( x = "Effect Size", y = "Statistical Power") + theme_classic() + scale_y_continuous(breaks = seq(0,1,0.2)) + scale_x_continuous(breaks= seq(0,0.16,0.02))
