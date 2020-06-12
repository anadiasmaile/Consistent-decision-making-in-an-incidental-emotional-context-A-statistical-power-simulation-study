library(ggplot2)
source("utilities_e_pos.R")

mean_total <- c()
e_cont <- 0.1
e_pos <- seq(0,0.1,0.001)
e_neg <- 0.1

for (effectsize in e_pos ) {
  result <- single_simulation(N = 150, e_cont, effectsize, e_neg)
  mean_total <- append(mean_total, result)
  }

#create dataframe
mean_posdf<- data.frame (e_pos,mean_total)

#create plot
ggplot (mean_posdf, aes( x = e_pos, y = mean_total)) + geom_point (size=1) + labs ( x = "e_pos", y = "mean_CCEI_total") + theme_classic() + scale_y_continuous(breaks = seq(0.7,0.9,0.02)) + scale_x_continuous(breaks= seq(0,0.1,0.01)) + geom_smooth(method = "lm", formula = y~x, se = FALSE)
