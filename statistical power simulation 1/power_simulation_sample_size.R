library(ggplot2)
source("utilities_sample_size.R")

sample_size <- seq(6,420,3)
power <- c()

for (samplesize in sample_size) {
N_sim <- 1000
sig_count <- 0
e_cont <- 0.1
e_pos <- 0.04
e_neg <- 0.1

for (sim in 1:N_sim) {
  result <- single_simulation(N = samplesize, e_cont, e_pos, e_neg)
  sig_count <- sig_count + result
}
power<- append(power, sig_count/N_sim)
}

#create dataframe
powerdf<- data.frame (sample_size,power)

#create plot
ggplot (powerdf, aes( x = sample_size, y = power)) + geom_point (size=1) + labs ( x = "Sample Size", y = "Statistical Power") + theme_classic() + scale_y_continuous(breaks = seq(0,1,0.2)) + scale_x_continuous(breaks= seq(0,420,30))
