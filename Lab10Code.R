library(tidyverse)

#Part 1: Basic Simulation

sim.study <- rbinom(n=1000, size=10000, prob=0.39) #Random binom sample

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