library(ggplot2)
library(dplyr)

set.seed(1)

# number of simulations
n <- 200000

# Weibull parameters
shape <- runif(n,0.5,6)
scale <- runif(n,1,12)

# employment probabilities
p5 <- 1 - exp(-(5/scale)^shape)
p6 <- 1 - exp(-(6/scale)^shape)

# prior information (90% landed jobs by the 6 month mark)
prior_w <- dnorm(p6,0.90,0.04)

# current layoff at the 5 month mark
data_w <- dnorm(p5,0.3,0.04)

# posterior weights
w <- prior_w * data_w
w <- w / sum(w)

# time when 99% employed
t99 <- scale * (-log(0.01))^(1/shape)

posterior_df <- data.frame(t99)

# summary stats
median_t99 <- quantile(t99,0.5)
low_t99 <- quantile(t99,0.05)
high_t99 <- quantile(t99,0.95)

# ---------------------------------------------------
# Plot 1: Posterior distribution of time to 99%
# ---------------------------------------------------

ggplot(posterior_df, aes(x=t99)) +
  geom_histogram(bins=60, fill="steelblue", alpha=0.7) +
  #geom_vline(xintercept=median_t99, color="red", linewidth=1.2) +
  #geom_vline(xintercept=c(low_t99,high_t99), linetype="dashed") +
  labs(
    title="Posterior distribution: time to 99% employment",
    x="Months after layoff",
    y="Count"
  ) +
  theme_minimal()

# ---------------------------------------------------
# Generate posterior employment curves
# ---------------------------------------------------

t <- seq(0,18,0.2)

curve_matrix <- sapply(1:n,function(i){
  1 - exp(-(t/scale[i])^shape[i])
})

# compute credible bands
mean_curve <- rowSums(curve_matrix * w)

low_curve <- apply(curve_matrix,1,quantile,0.05)
high_curve <- apply(curve_matrix,1,quantile,0.95)

curve_df <- data.frame(
  month=t,
  mean=mean_curve,
  low=low_curve,
  high=high_curve
)

# ---------------------------------------------------
# Plot 2: Posterior employment 
# ---------------------------------------------------

ggplot(curve_df,aes(month,mean)) +
  geom_ribbon(aes(ymin=low,ymax=high),
              fill="skyblue",
              alpha=0.4) +
  geom_line(color="steelblue", linewidth=1.2) + 
  geom_vline(xintercept = median_t99,
                                                          linetype = "dashed",
                                                          linewidth = 1,
                                                          color = "red") +
  labs(
    title="Posterior employment trajectory",
    x="Months since layoff",
    y="Share employed"
  ) + scale_x_continuous(breaks = seq(0, 18, 1)) +
  labs(
    title = "Posterior employment trajectory",
    x = "Months since layoff",
    y = "Share employed"
  ) +
  ylim(0,1) +
  theme_minimal()