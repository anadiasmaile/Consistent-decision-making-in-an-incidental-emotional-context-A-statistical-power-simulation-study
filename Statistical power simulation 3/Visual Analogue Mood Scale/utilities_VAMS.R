library(truncnorm)
library(extraDistr)

#generate function for VAMS
gen_vams_data <- function(group, m_cont, m_pos, m_neg) {
  if (group == "cont") {
    return(rtruncnorm(1, 0, 100, m_cont, 5))
  }
  if (group == "pos") {
    return(rtruncnorm(1, 0, 100, m_pos, 5))
  }
  if (group == "neg") {
    return(rtruncnorm(1, 0, 100, m_neg, 5))
  }
}

single_simulation <- function(N, m_cont, m_pos, m_neg) {
  
  if(N%%3 != 0) stop("N must be divisible by 3.")
  
  #Generate N participants
  vp <- seq(1, N, 1)
  
  #Assign participants to three groups
  group <- rep(c("neg", "pos", "cont"), N/3)
  
  df <- data.frame(vp, group)
  
  df$VAMS <- apply(df, 1, function(x) gen_vams_data(x["group"], m_cont, m_pos, m_neg))
  
  sig <- summary(aov(VAMS ~ group,df)) [[1]] [["Pr(>F)"]] < 0.05
  ifelse(sig, return(1), return(0))
  
}