#Michael Manolakis 
#Econ 629 Econometrics 2 - CRN 79650
#Problem Set 5
#5 December 2025

library(tidyverse)
#library(wooldridge)

# Consider the Following Data Generating Process (DGP):
#     y= 1 + 0.5x +v
# Where x approximates a Normal Distribution of 1,1 and v approximates a Normal distribution of 0,1. The econometrician does not observe x, but instead observes one (or two) of the following:
#     x^*_1 = x +e_1 where e_1 ~ N(0,0.25)
#     x^*_2 = x +e_2 where e_2 ~ N(0,0.5)
#     x^*_3 = x +e_3 where e_3 ~ N(0,19)
##### steps of work process
# 1. Simulate this DGP for N = 500 a total of 50 times in Mata.
# Monte Carlo Simulation
#set.seed(123)
#n <- 500

#x<-rnorm(n=n, mean=1, sd=1)
#v<-rnorm(n=n, mean=0, sd=1)
#y <- 1 + 0.5 * x + v

#e_1<-rnorm(n=n, mean=0, sd= sqrt(0.25))
#e_2<-rnorm(n=n, mean=0, sd= sqrt(0.5))
#e_3<-rnorm(n=n, mean=0, sd= sqrt(19))

# For the purpose of coding simplicity, let x^*_1=x_1, x^*_2=x_2, and x^*_3=x_3
#x_1<- x +e_1
#x_2<- x +e_2
#x_3<- x +e_3

######## Creating a simulation
# Defining Parameters
n_sims <- 50
n <- 500

# Container for Results
sim_results <- tibble(
  iteration = 1:n_sims,
  beta_hat_ols = NA_real_,
  beta_hat_iv = NA_real_
)

for (i in 1:n_sims) {
  
  # --- A. Data Generating Process ---
  x <- rnorm(n = n, mean = 1, sd = 1)
  v <- rnorm(n = n, mean = 0, sd = 1)
  y <- 1 + 0.5 * x + v
  
  e_1 <- rnorm(n = n, mean = 0, sd = sqrt(0.25))
  e_2 <- rnorm(n = n, mean = 0, sd = sqrt(0.5))
  e_3 <- rnorm(n = n, mean = 0, sd = sqrt(19))
  
  x_1 <- x + e_1
  x_2 <- x + e_2
  x_3 <- x + e_3
  
  # Create a temporary dataframe for this iteration
  df_iter <- tibble(y, x_1, x_2, x_3)
}




# 2. What are the theoretical biases from a regression of y onto x^*_1, x^*_2, and x^*_3?
# Attenuation Bias










# 3. How do the empirical biases compare to the theoretical biases?
# Relationship between plim and mean of simulated sampling distribution










# 4. Consider two IV regressions of y onto x^*_1. The first uses x^*_2 and the second uses x^*_3 as the IV. What is the mean of both IV estimates?
# IV as a solution to measurement error










# 5. What are the average first stage F-statistics from both sets of regressions?
# Weak Instruments











# 6. Using the two-way command, plot kernel density estimates of both simulated IV estimates in a single plot.
# 7. What can we conclude about the IV estimates that use x^*_3 as the instrument? Why does it matter?
# 6 & 7. Consequences of Weak Instruments










