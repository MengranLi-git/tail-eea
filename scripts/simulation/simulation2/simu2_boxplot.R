library(ggpubr)

p1 <- 0.05
p2 <- 0.02
p3 <- 0.01


load("E:/R/EEA/simulation/simulation2/delta_0.3.Rdata")
delta_0.3 <- delta_0.3[which(sapply(delta_0.3, function(x) length(x) != 1))]

load("E:/R/EEA/simulation/simulation2/delta_0.4.Rdata")
delta_0.4 <- delta_0.4[which(sapply(delta_0.4, function(x) length(x) != 1))]

load("E:/R/EEA/simulation/simulation2/delta_0.6.Rdata")
delta_0.6 <- delta_0.6[which(sapply(delta_0.6, function(x) length(x) != 1))]

load("E:/R/EEA/simulation/simulation2/delta_0.7.Rdata")
delta_0.7 <- delta_0.7[which(sapply(delta_0.7, function(x) length(x) != 1))]

load("E:/R/EEA/simulation/simulation2/delta_0.8.Rdata")
delta_0.8 <- delta_0.8[which(sapply(delta_0.8, function(x) length(x) != 1))]


boxplot_0.05 <- data.frame(HW = c(sapply(delta_0.3, function(x) x[[1]][[1]][1]), 
                                    sapply(delta_0.4, function(x) x[[1]][[1]][1]),
                                    sapply(delta_0.6, function(x) x[[1]][[1]][1]),
                                    sapply(delta_0.7, function(x) x[[1]][[1]][1]),
                                    sapply(delta_0.8, function(x) x[[1]][[1]][1])),
                           mGPD = c(sapply(delta_0.3, function(x) x[[2]][[1]][1]), 
                                    sapply(delta_0.4, function(x) x[[2]][[1]][1]),
                                    sapply(delta_0.6, function(x) x[[2]][[1]][1]),
                                    sapply(delta_0.7, function(x) x[[2]][[1]][1]),
                                    sapply(delta_0.8, function(x) x[[2]][[1]][1])),
                           eFCM = c(sapply(delta_0.3, function(x) x[[3]][[1]][1]), 
                                    sapply(delta_0.4, function(x) x[[3]][[1]][1]),
                                    sapply(delta_0.6, function(x) x[[3]][[1]][1]),
                                    sapply(delta_0.7, function(x) x[[3]][[1]][1]),
                                    sapply(delta_0.8, function(x) x[[3]][[1]][1]))
                           )
boxplot_0.05 <- boxplot_0.05 - p1
boxplot_0.05$delta <- c(rep(0.3, each = 300), rep(c(0.4, 0.6, 0.7, 0.8), each = 1000))

boxplot_0.02 <- data.frame(HW = c(sapply(delta_0.3, function(x) x[[1]][[1]][2]), 
                                  sapply(delta_0.4, function(x) x[[1]][[1]][2]),
                                  sapply(delta_0.6, function(x) x[[1]][[1]][2]),
                                  sapply(delta_0.7, function(x) x[[1]][[1]][2]),
                                  sapply(delta_0.8, function(x) x[[1]][[1]][2])),
                           mGPD = c(sapply(delta_0.3, function(x) x[[2]][[1]][2]), 
                                    sapply(delta_0.4, function(x) x[[2]][[1]][2]),
                                    sapply(delta_0.6, function(x) x[[2]][[1]][2]),
                                    sapply(delta_0.7, function(x) x[[2]][[1]][2]),
                                    sapply(delta_0.8, function(x) x[[2]][[1]][2])),
                           eFCM = c(sapply(delta_0.3, function(x) x[[3]][[1]][2]), 
                                    sapply(delta_0.4, function(x) x[[3]][[1]][2]),
                                    sapply(delta_0.6, function(x) x[[3]][[1]][2]),
                                    sapply(delta_0.7, function(x) x[[3]][[1]][2]),
                                    sapply(delta_0.8, function(x) x[[3]][[1]][2]))
)
boxplot_0.02 <- boxplot_0.02 - p2
boxplot_0.02$delta <- c(rep(0.3, each = 300), rep(c(0.4, 0.6, 0.7, 0.8), each = 1000))


boxplot_0.01 <- data.frame(HW = c(sapply(delta_0.3, function(x) x[[1]][[1]][3]), 
                                  sapply(delta_0.4, function(x) x[[1]][[1]][3]),
                                  sapply(delta_0.6, function(x) x[[1]][[1]][3]),
                                  sapply(delta_0.7, function(x) x[[1]][[1]][3]),
                                  sapply(delta_0.8, function(x) x[[1]][[1]][3])),
                           mGPD = c(sapply(delta_0.3, function(x) x[[2]][[1]][3]), 
                                    sapply(delta_0.4, function(x) x[[2]][[1]][3]),
                                    sapply(delta_0.6, function(x) x[[2]][[1]][3]),
                                    sapply(delta_0.7, function(x) x[[2]][[1]][3]),
                                    sapply(delta_0.8, function(x) x[[2]][[1]][3])),
                           eFCM = c(sapply(delta_0.3, function(x) x[[3]][[1]][3]), 
                                    sapply(delta_0.4, function(x) x[[3]][[1]][3]),
                                    sapply(delta_0.6, function(x) x[[3]][[1]][3]),
                                    sapply(delta_0.7, function(x) x[[3]][[1]][3]),
                                    sapply(delta_0.8, function(x) x[[3]][[1]][3]))
)
boxplot_0.01 <- boxplot_0.01 - p3
boxplot_0.01$delta <- c(rep(0.3, each = 300), rep(c(0.4, 0.6, 0.7, 0.8), each = 1000))

combined_plot <- ggarrange(p1, p2, p3,
                           ncol = 3,
                           common.legend = TRUE,
                           legend = "bottom",
                           labels = c("a)", "b)", "c)"))

# Display the combined plot
combined_plot
ggsave("simulation/simulation2/simu2_boxplot.pdf", width = 16, height = 6)


make_plot <- function(data, p_title) {
  data %>%
    gather("Method", "Error", -delta) %>%
    filter(Method != "HW") %>%  # ✅ remove HW model
    ggplot(aes(x = factor(delta), y = Error, fill = Method)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      x = expression(delta),  # ✅ Greek letter delta
      y = "Bias",             # ✅ change y label
      title = p_title,
      fill = "Model"          # ✅ legend title
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),  # ✅ center title
      legend.title = element_text(size = 14),   # ✅ bigger legend title
      legend.text = element_text(size = 12)     # ✅ bigger legend text
    ) +
    scale_fill_manual(
      values = c("eFCM" = "#FF99B3", "mGPD" = "#4DA6FF"),
      labels = c("eFCM" = "eFCM", "mGPD" = "mGPD")
    )
}

# Create plots (sorting from low p to high p: p=0.01, 0.02, 0.05)
p1 <- make_plot(boxplot_0.01, "p = 0.01")
p2 <- make_plot(boxplot_0.02, "p = 0.02")
p3 <- make_plot(boxplot_0.05, "p = 0.05")

# Standardize y-axis limits
y_min <- min(c(layer_scales(p1)$y$range$range,
               layer_scales(p2)$y$range$range,
               layer_scales(p3)$y$range$range))
y_max <- max(c(layer_scales(p1)$y$range$range,
               layer_scales(p2)$y$range$range,
               layer_scales(p3)$y$range$range))

p1 <- p1 + ylim(y_min, y_max)
p2 <- p2 + ylim(y_min, y_max)
p3 <- p3 + ylim(y_min, y_max)

# Combine plots without labels
combined_plot <- ggarrange(p1, p2, p3,
                           ncol = 3,
                           common.legend = TRUE,
                           legend = "bottom")

# Display combined plot
combined_plot



library(dplyr)

# Function to compute RMSE per method per delta
compute_rmse <- function(df) {
  df %>%
    select(delta, mGPD, eFCM) %>%  # 只保留两个方法
    pivot_longer(cols = c("mGPD", "eFCM"), names_to = "Method", values_to = "Error") %>%
    group_by(delta, Method) %>%
    summarize(RMSE = sqrt(mean(Error^2, na.rm = TRUE)), .groups = "drop")
}

# Compute RMSE for each p
rmse_0.05 <- compute_rmse(boxplot_0.05)
rmse_0.02 <- compute_rmse(boxplot_0.02)
rmse_0.01 <- compute_rmse(boxplot_0.01)

# Combine all RMSEs into one table
rmse_all <- bind_rows(
  rmse_0.05 %>% mutate(p = 0.05),
  rmse_0.02 %>% mutate(p = 0.02),
  rmse_0.01 %>% mutate(p = 0.01)
)

# View result
print(rmse_all)

generate_row <- function(model, p, values, best) {
  row <- paste(
    model, ifelse(model == "eFCM", p, ""),
    paste(ifelse(best, paste0("\\textbf{", format(values, digits=4, nsmall=4), "}"),
                 format(values, digits=4, nsmall=4)), collapse = " & "),
    sep = " & "
  )
  return(paste0(row, " \\\\"))
}

generate_row()


