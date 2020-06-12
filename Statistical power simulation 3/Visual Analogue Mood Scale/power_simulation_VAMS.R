source("utilities_VAMS.R")


N_sim <- 1000
sig_count <- 0
m_cont <- 60
m_pos <- 65
m_neg <- 55

for (sim in 1:N_sim) {
  result <- single_simulation(N = 150, m_cont, m_pos, m_neg)
  sig_count <- sig_count + result
}

power <- sig_count/N_sim