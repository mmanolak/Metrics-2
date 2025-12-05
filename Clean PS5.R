#Michael Manolakis 
#Econ 629 Econometrics 2 - CRN 79650
#Problem Set 5 - Clean Version (Minimal Comments)
#5 December 2025

#install.packages(c("tidyverse", "AER", "wooldridge"))

library(tidyverse)
library(AER)
#library(wooldridge)

# Defining Parameters
n_sims <- 50
n <- 500

# Container for Results
sim_results <- tibble(iteration = 1:n_sims, beta_hat_x1 = NA_real_, beta_hat_x2 = NA_real_, beta_hat_x3 = NA_real_, beta_iv_x2  = NA_real_, beta_iv_x3  = NA_real_, f_stat_iv_x2 = NA_real_, f_stat_iv_x3 = NA_real_)

for (i in 1:n_sims) {
  # DGP
  x <- rnorm(n = n, mean = 1, sd = 1)
  v <- rnorm(n = n, mean = 0, sd = 1)
  y <- 1 + 0.5 * x + v
  
  e1 <- rnorm(n = n, mean = 0, sd = sqrt(0.25))
  e2 <- rnorm(n = n, mean = 0, sd = sqrt(0.5))
  e3 <- rnorm(n = n, mean = 0, sd = sqrt(19))
  
  x1 <- x + e1
  x2 <- x + e2
  x3 <- x + e3
  
  df_iter <- tibble(y, x1, x2, x3)
  
  # OLS Regressions
  # Estimation and Storage
  ols_model1 <- lm(y ~ x1, data = df_iter)
  sim_results$beta_hat_x1[i] <- coef(ols_model1)[["x1"]]
  
  ols_model2 <- lm(y ~ x2, data = df_iter)
  sim_results$beta_hat_x2[i] <- coef(ols_model2)[["x2"]]
  
  ols_model3 <- lm(y ~ x3, data = df_iter)
  sim_results$beta_hat_x3[i] <- coef(ols_model3)[["x3"]]
  
  
  # IV Regressions
  # y on x^*_1 using x^*_2 as an instrument
  iv_model_x2 <- ivreg(y ~ x1 | x2, data = df_iter)
  sim_results$beta_iv_x2[i] <- coef(iv_model_x2)[["x1"]]
  
  # y on x^*_1 using x^*_3 as an instrument
  iv_model_x3 <- ivreg(y ~ x1 | x3, data = df_iter)
  sim_results$beta_iv_x3[i] <- coef(iv_model_x3)[["x1"]]
  
  # iv_model_x2 defined:
  iv_summary_x2 <- summary(iv_model_x2, diagnostics = TRUE)
  sim_results$f_stat_iv_x2[i] <- iv_summary_x2$diagnostics["Weak instruments", "statistic"]
  
  # iv_model_x3 defined:
  iv_summary_x3 <- summary(iv_model_x3, diagnostics = TRUE)
  sim_results$f_stat_iv_x3[i] <- iv_summary_x3$diagnostics["Weak instruments", "statistic"]
}

# LLN to estimate plim
mean(sim_results$beta_hat_x1)
mean(sim_results$beta_hat_x2)
mean(sim_results$beta_hat_x3)

# Some more averages
mean(sim_results$beta_iv_x2)
mean(sim_results$beta_iv_x3)
mean(sim_results$f_stat_iv_x2)
mean(sim_results$f_stat_iv_x3)

# Standard Deviation of the IV estimates to compare efficiency/variance
sd(sim_results$beta_iv_x2)
sd(sim_results$beta_iv_x3)


