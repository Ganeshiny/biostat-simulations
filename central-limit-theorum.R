#install.packages('ggplot2')
library(stats)
library(ggplot2)
library(dplyr)

# Generate a parent population
generate_parent_pop <- function(type_of_distribution, number_of_obs = 1000, mean = 1, std = 2, min = 0, max =1, rate = 0.5, 
                                size = 10 , prob = 0.5, lambda = 10, df = 3){
  if (type_of_distribution == 'normal'){
    df <- data.frame(X = rnorm(number_of_obs, mean = mean, sd = std))
  } else if (type_of_distribution == 'uniform') {
    df <- data.frame(X = runif(number_of_obs, min, max))
  } else if (type_of_distribution == 'exponential') {
    df <- data.frame(X = rexp(number_of_obs, rate))
  } else if (type_of_distribution == 'binomial') {
    df <- data.frame(X = rbinom(number_of_obs, size, prob))
  } else if (type_of_distribution == 'poisson') {
    df <- data.frame(X = rpois(number_of_obs, lambda))
  } else if (type_of_distribution == 'chisquare'){
    df <- data.frame(X = rchisq (number_of_obs, df))
  }
}

# Resampling: Number of samples and sample size
sampling_means <- function(number_of_samples, parent_pop_df, sample_size) {
  sampled_means <- numeric(number_of_samples)
  
  for (i in 1:number_of_samples) {
    sample_df <- sample_n(parent_pop_df, sample_size, replace = TRUE)
    sampled_means[i] <- mean(sample_df$X)
  }
  
  return(data.frame(X_mean = sampled_means))
}


## ====================================================

# Make a parent population with a desired distribution
df = generate_parent_pop(type_of_distribution = 'normal')
# Generate sampled means from the parent population
sampled_dist <- sampling_means(30, df, 150)
sampled_mean = mean(sampled_dist$X_mean); sampled_mean
sample_std = sd(sampled_dist$X_mean); sample_std


## ============ Generating the plots =================

# Plot the parent population distribution using a histogram with density curve
ggplot(df, aes(x = X)) + 
  geom_histogram(aes(y = ..density..), bins = 30, fill = 'blue', alpha = 0.5, color = 'black') + 
  geom_density(color = 'red', size = 1) + 
  ggtitle("Parent Population Distribution (Histogram and Density)") + 
  xlab("X") + 
  ylab("Density")

# Plot the sampling distribution using a histogram with density curve
ggplot(sampled_dist, aes(x = X_mean)) + 
  geom_histogram(aes(y = ..density..), bins = 30, fill = 'green', alpha = 0.5, color = 'black') + 
  geom_density(color = 'red', size = 1) + 
  ggtitle("Sampling Distribution of the Mean (Histogram and Density)") + 
  xlab("Sample Mean") + 
  ylab("Density")
