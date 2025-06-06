source("simulation/hw_tools.R")

# Function to estimate delta from data
estimate_delta <- function(data) {
  # Define log-likelihood function for the G1 distribution
  hw.llh <- function(delta, x, log = FALSE) {
    loglik <- sum(dG1(x, delta, log = TRUE))
    if(log) {
      return(loglik)
    } else {
      return(exp(loglik))
    }
  }
  
  # Use optimization to find delta that maximizes log-likelihood
  delta_est <- optimise(f = function(delta) -hw.llh(delta, log(data[data > 1]), log = TRUE), 
                        interval = c(0.5, 0.99), maximum = FALSE)$minimum
  
  return(delta_est)
}

# Function to prepare data for QQ plot (without plotting)
prepare_qqplot_data <- function(qqdata) {
  # Get dimensions
  n_sims <- length(qqdata)
  n <- length(qqdata[[1]])
  
  # Matrix to hold empirical data
  emp_matrix <- matrix(NA, nrow = n_sims, ncol = n)
  
  # Matrix to hold estimated deltas for each simulation
  deltas <- numeric(n_sims)
  
  # Matrix to hold theoretical quantiles based on estimated deltas
  theo_matrix <- matrix(NA, nrow = n_sims, ncol = n)
  
  # Process each simulation
  for (i in 1:n_sims) {
    # Extract empirical data from this simulation and sort the values
    emp_matrix[i, ] <- sort(qqdata[[i]])
    
    # Estimate delta for this simulation
    deltas[i] <- estimate_delta(emp_matrix[i, ])
    
    # Generate theoretical quantiles based on estimated delta
    p <- (1:n) / (n + 1)  # Standard plotting positions
    theo_matrix[i, ] <- exp(qG1(p, delta = deltas[i]))
  }
  
  # Calculate means and quantiles
  emp_means <- colMeans(emp_matrix)
  theo_means <- colMeans(theo_matrix)
  
  # Calculate confidence intervals for theoretical quantiles
  theo_lower <- apply(theo_matrix, 2, function(x) quantile(x, 0.025))
  theo_upper <- apply(theo_matrix, 2, function(x) quantile(x, 0.975))
  
  # Return all computed statistics
  return(list(
    deltas = deltas,
    mean_delta = mean(deltas),
    sd_delta = sd(deltas),
    ci_delta = quantile(deltas, c(0.025, 0.975)),
    theo_means = theo_means,
    emp_means = emp_means,
    theo_lower = theo_lower,
    theo_upper = theo_upper
  ))
}

# Create data frames for ggplot
create_plot_df <- function(data) {
  df <- data.frame(
    theo = data$theo_means,
    emp = data$emp_means,
    lower = data$theo_lower,
    upper = data$theo_upper
  )
  return(df)
}


# Prepare data for both datasets
load("E:/R/EEA/simulation/simulation1/simu1_0.3.Rdata")
load("E:/R/EEA/simulation/simulation1/simu1_0.7.Rdata")

qqdata_0.3_1 <- lapply(hw_delta_0.3, function(x) x[[1]][,1])
qqdata_0.3_2 <- lapply(hw_delta_0.3, function(x) x[[1]][,2])

qqdata_0.7_1 <- lapply(hw_delta_0.7, function(x) x[[1]][,1])
qqdata_0.7_2 <- lapply(hw_delta_0.7, function(x) x[[1]][,2])

data1 <- prepare_qqplot_data(qqdata_0.3_1)
data2 <- prepare_qqplot_data(qqdata_0.3_2)

data3 <- prepare_qqplot_data(qqdata_0.7_1)
data4 <- prepare_qqplot_data(qqdata_0.7_2)

df1 <- create_plot_df(data1)
df2 <- create_plot_df(data2)

df3 <- create_plot_df(data3)
df4 <- create_plot_df(data4)

# Determine common plot limits
all_values <- c(df1$theo, df1$emp, df1$lower, df1$upper,
                df2$theo, df2$emp, df2$lower, df2$upper)
all_values <- all_values[is.finite(all_values) & all_values > 0]
limits <- range(all_values) * c(0.9, 1.1)

all_values2 <- c(df3$theo, df3$emp, df3$lower, df3$upper,
                df4$theo, df4$emp, df4$lower, df4$upper)
all_values2 <- all_values2[is.finite(all_values2) & all_values2 > 0]
limits <- range(all_values2) * c(0.9, 1.1)

library(ggplot2)
p1 <- ggplot(df1, aes(x = emp, y = theo)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  geom_point(color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 0.8) +
  scale_x_continuous(limits = limits) +
  scale_y_continuous(limits = limits) +
  labs(
    title = "δ = 0.3 (1)",
    x = "Empirical Quantiles",
    y = "Estimated Distribution"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    panel.grid = element_line(color = "gray90"),
    aspect.ratio = 1  # Force square aspect ratio
  )

# Create ggplot for Dataset 2
p2 <- ggplot(df2, aes(x = emp, y = theo)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  geom_point(color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 0.8) +
  scale_x_continuous(limits = limits) +
  scale_y_continuous(limits = limits) +
  labs(
    title = "δ = 0.3 (2)",
    x = "Empirical Quantiles",
    y = "Estimated Distribution"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    panel.grid = element_line(color = "gray90"),
    aspect.ratio = 1  # Force square aspect ratio
  )


p3 <- ggplot(df3, aes(x = emp, y = theo)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  geom_point(color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 0.8) +
  scale_x_continuous(limits = limits) +
  scale_y_continuous(limits = limits) +
  labs(
    title = "δ = 0.7 (1)",
    x = "Empirical Quantiles",
    y = "Estimated Distribution"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    panel.grid = element_line(color = "gray90"),
    aspect.ratio = 1  # Force square aspect ratio
  )

# Create ggplot for Dataset 2
p4 <- ggplot(df4, aes(x = emp, y = theo)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  geom_point(color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 0.8) +
  scale_x_continuous(limits = limits) +
  scale_y_continuous(limits = limits) +
  labs(
    title = "δ = 0.7 (2)",
    x = "Empirical Quantiles",
    y = "Estimated Distribution"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    panel.grid = element_line(color = "gray90"),
    aspect.ratio = 1  # Force square aspect ratio
  )


library(gridExtra)
# Arrange plots side by side
qqplot <- grid.arrange(
  p1, p2, p3, p4,
  ncol = 2
)

ggsave("simulation/simulation1/simu1_qqplot.pdf", plot = qqplot, height = 6, width = 7)
