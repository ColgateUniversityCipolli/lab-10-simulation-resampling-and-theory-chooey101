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
    title = bquote("Simulation Study using a Binomial Distribution"),
    x = "Value",
    y = "Density"
  ) +
  theme_minimal()

#The graph is not skewed; the middle 95 percent of the graph has a range of 200

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

simulation_plot <- ggplot(stored_values, aes(x = p, y = n, fill = moe)) +
  geom_raster() +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Margin of Error by Sample Size (n) and Probability (p)",
    x = "True Probability (p)",
    y = "Sample Size (n)",
    fill = "Margin of Error"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

#Part 4: actual margin of error calculation
grid <- expand.grid(n = n_values, p = p_values)

Wilson_moe <- function(n, p, z = qnorm(0.975)) {
  numerator <- z * sqrt((n * p * (1 - p) + (z^2 / 4)) / (n + z^2))
  return(numerator)
}


#apply to all data
grid$moe <- mapply(Wilson_moe, grid$n, grid$p)

# Convert to tibble
wilson_data <- as_tibble(grid)

wilson_plot <- ggplot(wilson_data, aes(x = p, y = n, fill = moe)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(
    title = "Wilson Margin of Error by Sample Size (n) and Probability (p)",
    x = "True Proportion (p)",
    y = "Sample Size (n)",
    fill = "MOE"
  ) +
  theme_minimal()
