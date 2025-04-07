library(tidyverse)

#Part 1: Basic Simulation

sim.study <- rbinom(n=10000, size=1000, prob=0.39) #Random binom sample

data <- tibble(x = sim.study)
plot <- ggplot(data=data, aes(x = x)) +
  geom_histogram(
    aes(y = after_stat(density)), 
    bins = 30, 
    fill = "lightblue", 
    color = "blue",
    alpha = 0.5  # Add some transparency
  ) +
  geom_density(color = "red", linewidth = 1) +
  labs(
    title = bquote("Beta Distribution (α = " ~ .(alpha) ~ ", β = " ~ .(beta) ~ ")"),
    x = "Value",
    y = "Density"
  ) +
  theme_minimal()

#The graph is not skewed; the middle 95 percent of the graph as a range of 200

#Part 2: Perform Resampling

R <- 1000
resamples <- tibble(p.hat = numeric(R))
for( i in 1:R){
curr.resample <- sample(x = data$x,
                        size = nrow(data),
                        replace = T)
# compute the stat on the resample
resamples$p.hat[i] <- mean(curr.resample)
}

resample.plot <- ggplot(resamples, aes(x = p.hat)) + 
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.01, color = "grey", fill = "white") +
  geom_density(color = "red", linewidth = 1) +
  geom_hline(yintercept = 0) +
  labs(x = "p.hat", y = "Density")


#Part 3: simulation over n and p
stored_values <- tibble(
  n = numeric(),
  p = numeric(),
  p_hat = numeric(),
  moe = numeric()
)

# Parameter values
n_values <- seq(100, 3000, by = 10)
p_values <- seq(0.01, 0.99, by = 0.01)

# Loop through and collect stats
for (n in n_values) {
  for (p in p_values) {
    sim <- rbinom(n = 10000, size = n, prob = p) / n
    moe <- (quantile(sim, 0.975) - quantile(sim, 0.025)) / 2
    
    temp_df <- tibble(
      n = n,
      p = p,
      p_hat = mean(sim),
      moe = moe
    )
    
    stored_values <- rbind(stored_values, temp_df)
  }
}