library(truncnorm)
library(extraDistr)
library(Hmisc)
library(lsr)


#Function to generate consistency data based on group
gen_consist_data <- function(group, m_cont, m_pos, m_neg) {
  if (group == "cont") {
    return(rtruncnorm(1, 0, 1, m_cont, 0.1))
  }
  if (group == "pos") {
    return(rtruncnorm(1, 0, 1, m_pos, 0.1))
  }
  if (group == "neg") {
    return(rtruncnorm(1, 0, 1, m_neg, 0.1))
  }
}


#Function to calculate consist data overall
calc_consist_data <- function(x, e_cont, e_pos, e_neg) {
  group <- x["group"]
  min_consist <- as.numeric(min(c(x["CCEI_keep"], x["CCEI_loss"])))
  if (group == "cont") {
    return(min_consist - rtruncnorm(1, 0, 1, e_cont, 0.05))
  }
  if (group == "pos") {
    return(min_consist - rtruncnorm(1, 0, 1, e_pos, 0.05))
  }
  if (group == "neg") {
    return(min_consist - rtruncnorm(1, 0, 1, e_neg, 0.05))
  }
}


single_simulation <- function(N, e_cont, e_pos, e_neg) {

  if(N%%3 != 0) stop("N must be divisible by 3.")

  #Generate N participants
  vp <- seq(1, N, 1)

  #Assign participants to three groups
  group <- rep(c("neg", "pos", "cont"), N/3)

  df <- data.frame(vp, group)

  df$CCEI_keep <- apply(df, 1, function(x) gen_consist_data(x["group"], 0.95, 0.95, 0.95))
  df$CCEI_loss <- apply(df, 1, function(x) gen_consist_data(x["group"], 0.95, 0.95, 0.95))
  df$CCEI_total <- apply(df, 1, function(x) calc_consist_data(x, e_cont, e_pos, e_neg))

  anovaeffect <- aov (formula = CCEI_total ~ group, data = df)
  effectdf <- etaSquared(anovaeffect)
  return(effectdf[1,1])
}
