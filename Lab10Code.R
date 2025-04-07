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

ggplot(resamples) + 
  geom_histogram(aes(x=p.hat, y=after_stat(density)),
                 bins = 30, 
                 color="grey")+
  geom_hline(yintercept=0)+
  xlab("p.hat")+
  ylab("Density")
