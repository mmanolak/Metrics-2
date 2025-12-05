#Michael Manolakis 
#Econ 629 Econometrics 2 - CRN 79650
#Problem Set 5
#5 December 2025

library(tidyverse)
library(AER)
#library(wooldridge)

# Consider the Following Data Generating Process (DGP):
#     y= 1 + 0.5x +v
# Where x ~ N(1,1) and v ~ N(0,1). The econometrician does not observe x, but instead observes one (or two) of the following:
#     x^*_1 = x +e_1 where e_1 ~ N(0,0.25)
#     x^*_2 = x +e_2 where e_2 ~ N(0,0.5)
#     x^*_3 = x +e_3 where e_3 ~ N(0,19)



# 1. Simulate this DGP for N = 500 a total of 50 times in Mata. #################################################
##### steps of work process
#set.seed(123)
#n <- 500

#x<-rnorm(n=n, mean=1, sd=1)
#v<-rnorm(n=n, mean=0, sd=1)
#y <- 1 + 0.5 * x + v

#e_1<-rnorm(n=n, mean=0, sd= sqrt(0.25))
#e_2<-rnorm(n=n, mean=0, sd= sqrt(0.5))
#e_3<-rnorm(n=n, mean=0, sd= sqrt(19))

# For the purpose of coding simplicity, let x^*_1=x1, x^*_2=x2, and x^*_3=x3
#x1<- x +e_1
#x2<- x +e_2
#x3<- x +e_3

######## Creating a simulation
# Defining Parameters
n_sims <- 50
n <- 500

# Container for Results
sim_results <- tibble(iteration = 1:n_sims, beta_hat_ols1 = NA_real_, beta_hat_iv = NA_real_)

for (i in 1:n_sims) {
  
  # --- A. Data Generating Process ---
  x <- rnorm(n = n, mean = 1, sd = 1)
  v <- rnorm(n = n, mean = 0, sd = 1)
  y <- 1 + 0.5 * x + v
  
  e1 <- rnorm(n = n, mean = 0, sd = sqrt(0.25))
  e2 <- rnorm(n = n, mean = 0, sd = sqrt(0.5))
  e3 <- rnorm(n = n, mean = 0, sd = sqrt(19))
  
  x1 <- x + e1
  x2 <- x + e2
  x3 <- x + e3
  
  # Create a temporary dataframe for this iteration
  df_iter <- tibble(y, x1, x2, x3)
}



# 2. What are the theoretical biases from a regression of y onto x^*_1, x^*_2, and x^*_3? #########################
##### steps of work process

# 1. Perform OLS regression
ols_model1 <- lm(y ~ x1, data = df_iter)

# 2. Extract the coefficient for x^*_1
beta_x1 <- coef(ols_model1)[["x1"]]

# 3. Save to the i-th row of the results tibble
sim_results$beta_hat_ols[i] <- beta_x1

# Rinse repeat
#ols_model2 <- lm(y ~ x2, data = df_iter)
#beta_x2 <- coef(ols_model)[["x2"]]
#sim_results$beta_hat_ols[i] <- beta_x2

#ols_model3 <- lm(y ~ x3, data = df_iter)
#beta_x3 <- coef(ols_model)[["x3"]]
#sim_results$beta_hat_ols[i] <- beta_x3



# 3. How do the empirical biases compare to the theoretical biases?  #################################################
##### steps of work process

# It was a many step process, first we would need to recreate question 1 too then account for beta_hat_x3 and beta_hat_x2. From here we then would add in the information gathered from within the for loop in question 2 after the generation of the temporary dataframe. To be more specific on what was added after the for loop, I put in the ols_model1/2/3 along with the related sim_results for the beta_hat.F After the for loop I took the average for fun and resolve question 3. By this I mean that using the LLN from Ch.3 of Wooldridge and Amemiya, we can use this to average to approximate a probability limit.



# 4. Consider two IV regressions of y onto x^*_1. The first uses x^*_2 and the second uses x^*_3 as the IV. What is the mean of both IV estimates?
# IV as a solution to measurement error
##### steps of work process

# Estimate IV regression (y on x^*_1, instrumented by x^*_2)
iv_model <- ivreg(y ~ x1 | x2, data = df_iter)

# Extract the coefficient for x1
beta_iv_x1 <- coef(iv_model)[["x1"]]

# Saving Results
sim_results$beta_hat_iv[i] <- beta_iv_x1



# 5. What are the average first stage F-statistics from both sets of regressions? ###################################
# Weak Instruments
##### steps of work process

# Summary object creation w/ diagnostics enabled
# This calculates the Weak Instruments test (First-stage F-stat)
iv_summary <- summary(iv_model, diagnostics = TRUE)

# Extract F Stats
# Storing it in the 'diagnostics' matrix under the row "Weak instruments" and column "statistic"
f_stat <- iv_summary$diagnostics["Weak instruments", "statistic"]



##### 6 & 7. Consequences of Weak Instruments
# 6. Using the two-way command, plot kernel density estimates of both simulated IV estimates in a single plot.
##### steps of work process
# For this the work will mostly continue onto the Clean version as I did not compile both here as it kept getting too messy for me. However I will comment below what I added into the clean at this step to get the graphs to show up for question/part/step.

#sim_results %>%
#  select(beta_iv_x2, beta_iv_x3) %>%
#  pivot_longer(cols = everything(), names_to = "Instrument", values_to = "Estimate") %>%
#  ggplot(aes(x = Estimate, fill = Instrument)) +
#  geom_density(alpha = 0.5) +
#  labs(
#    title = "Kernel Density Estimates of IV Estimators",
#    x = "Beta Hat",
#    y = "Density"
#  ) + theme_minimal()



# 7. What can we conclude about the IV estimates that use x^*_3 as the instrument? Why does it matter?
##### steps of work process

## The Below is the same in this dirty version as it is in the clean version for comments/answers. I do not want to force you to read it twice.

# Concentration, mu squared, parameter will become more dominate as strength of the instruments grow. A high F-statistic suggests a strong instrument while a low F-stat suggests a weak one. Given that the data within the sim results, granted only showing the first several of the series without expanding it, shows that the F-Stat of IV_x^*_3 is weak, while the IV_x^*_2 is strong. While looking into the console after running the full script the relative scale that the with the use of the LLN, it shows that the mean of the F-stat of IV_x^*_2 will be much greater than that of the mean of F-stat IV_x^*_3. Comparing the mean, plim, of beta_x^*_2, is closer to the theoretical/true beta value of 0.5; while beta_x^*_3 will be off on a larger amount. For my examples I had gotten beta_x^*_2 is approximately 0.4912 while beta_x^*_3 is 0.5196. With my F-stat of IV_x^*_2 approximating 578.32 with the F-stat of IV_x^*_3 being 20.64.
# To add a point of clarity, while beta_x^*_2 is closer to the true/theory beta than beta_x^*_3, they are still both fairly close. However the large issue with this comes out from the fact that the variation within x^*_3 is quite large, and would make the performance of any single sample highly unreliable, but with a large enough size it should come back. But this would mean we would have to have a very large N, much greater than the 500 we are using here, something like over a million or more.








