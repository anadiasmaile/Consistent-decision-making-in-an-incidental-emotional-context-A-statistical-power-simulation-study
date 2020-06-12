library(truncnorm)
library(extraDistr)
library(ggplot2)
library(Hmisc)
library(lsr)

#Generate 9999 participants
vp <- seq(1, 9999, 1)

#Assign participants to three groups
group <- rep(c("neg", "pos", "cont"), 3333)

df <- data.frame(vp, group)

#Generate Consistency data

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

#Generate consist data for keep frame based on group
m_cont <- 0.95
m_pos <- m_cont
m_neg <- m_cont

df$CCEI_keep <- apply(df, 1, function(x) gen_consist_data(x["group"], m_cont, m_pos, m_neg))

#Generate consist data for loss frame based on group
m_cont <- 0.95
m_pos <- m_cont
m_neg <- m_cont

df$CCEI_loss <- apply(df, 1, function(x) gen_consist_data(x["group"], m_cont, m_pos, m_neg))

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

#Generate consist data total
e_cont <- 0.1
e_pos <- 0.04
e_neg <- 0.1

df$CCEI_total <- apply(df, 1, function(x) calc_consist_data(x, e_cont, e_pos, e_neg))

#calculate ANOVA and effect size
anovaeffect <- aov (formula = CCEI_total ~ group, data = df)
summary (anovaeffect)
etaSquared (anovaeffect)
