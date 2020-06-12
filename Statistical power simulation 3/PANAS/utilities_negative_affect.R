library(truncnorm)
library(extraDistr)

#generate function for PANAS
gen_PANAS_data <- function(group, m_cont, m_pos, m_neg) {
  if (group == "cont") {
    return(rtpois(1, m_cont, 10, 50))
  }
  if (group == "pos") {
    return(rtpois(1, m_pos, 10, 50))
  }
  if (group == "neg") {
    return(rtpois(1, m_neg, 10, 50))
  }
}

single_simulation <- function(N, m_cont, m_pos, m_neg) {
  
  if(N%%3 != 0) stop("N must be divisible by 3.")
  
  #Generate N participants
  vp <- seq(1, N, 1)
  
  #Assign participants to three groups
  group <- rep(c("neg", "pos", "cont"), N/3)
  
  df <- data.frame(vp, group)
  
  df$NAS <- apply(df, 1, function(x) gen_PANAS_data(x["group"],m_cont, m_pos, m_neg))
  
  sig <- summary(aov(NAS ~ group,df)) [[1]] [["Pr(>F)"]] < 0.05
  ifelse(sig, return(1), return(0))
  
}