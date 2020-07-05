library(ggplot2)
source("utilities_e_pos.R")

effect_total <- c()
e_cont <- 0.1
e_pos <- seq(0,0.1,0.001)
e_neg <- 0.1

for (effectsize in e_pos ) {
  result <- single_simulation(N = 9999, e_cont, effectsize, e_neg)
  effect_total <- append(effect_total, result)
}

#create dataframe
effect_total_df<- data.frame (e_pos,effect_total)

#create plot
ggplot (effect_total_df, aes( x = e_pos, y = effect_total)) + geom_point (size=1) + labs ( x = "e_pos", y = "effect size") + theme_classic() + scale_y_continuous(breaks = seq(0,0.2,0.02)) + scale_x_continuous(breaks= seq(0,0.1,0.01))
