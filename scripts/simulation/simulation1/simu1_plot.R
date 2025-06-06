
load("E:/R/EEA/simulation/simulation1/simu1_0.3.Rdata")

## list(dat, wrongd, wrongm, pard, parm)

#### box plot ####

p1 = 0.05
p2 = 0.02
p3 = 0.01

wd <- sapply(hw_delta_0.3, function(x) x[[2]])
wm <- sapply(hw_delta_0.3, function(x) x[[3]])

err_0.3 <- rbind(wd - c(p1, p2, p3), wm - c(p1, p2, p3))
err_0.3 <- as.data.frame(t(err_0.3))
names(err_0.3) <- c(paste0("wd", 1:3), paste0("wm", 1:3))

library(tidyr)
library(ggplot2)
library(tidyverse)

# Reshape data from wide to long format
long_data_0.3 <- err_0.3 %>%
  pivot_longer(
    cols = c(paste0("wd", 1:3), paste0("wm", 1:3)),
    names_to = "measurement",
    values_to = "value"
  ) %>%
  mutate(
    type = substr(measurement, 2, 2),    # get 'd' or 'm'
    group = case_when(
      substr(measurement, 3, 3) == "1" ~ "p=0.05",
      substr(measurement, 3, 3) == "2" ~ "p=0.02",
      substr(measurement, 3, 3) == "3" ~ "p=0.01"
    ),
    # Make sure the groups are in the correct order
    group = factor(group, levels = c("p=0.05", "p=0.02", "p=0.01"))
  )


# Create boxplot
plot1 <- ggplot(long_data_0.3, aes(x = group, y = value, fill = type)) +
  geom_boxplot() +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  labs(
    x = "",
    y = "Bias",
    fill = "Type"
  ) +
  scale_fill_manual(
    values = c("d" = "#69b3a2", "m" = "#404080"),
    labels = c("d" = "WD and RM", "m" = "RD and WM")
  )


load("E:/R/EEA/simulation/simulation1/simu1_0.7.Rdata")

wd_0.7 <- sapply(hw_delta_0.7, function(x) x[[2]])
wm_0.7 <- sapply(hw_delta_0.7, function(x) x[[3]])

err_0.7 <- rbind(wd_0.7 - c(p1, p2, p3), wm_0.7 - c(p1, p2, p3))
err_0.7 <- as.data.frame(t(err_0.7))
names(err_0.7) <- c(paste0("wd", 1:3), paste0("wm", 1:3))

long_data_0.7 <- err_0.7 %>%
  pivot_longer(
    cols = c(paste0("wd", 1:3), paste0("wm", 1:3)),
    names_to = "measurement",
    values_to = "value"
  ) %>%
  mutate(
    type = substr(measurement, 2, 2),    # get 'd' or 'm'
    group = case_when(
      substr(measurement, 3, 3) == "1" ~ "p=0.05",
      substr(measurement, 3, 3) == "2" ~ "p=0.02",
      substr(measurement, 3, 3) == "3" ~ "p=0.01"
    ),
    # Make sure the groups are in the correct order
    group = factor(group, levels = c("p=0.05", "p=0.02", "p=0.01"))
  )

# Create boxplot
plot2 <- ggplot(long_data_0.7, aes(x = group, y = value, fill = type)) +
  geom_boxplot() +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  labs(
    x = "",
    y = "Bias",
    fill = "Type"
  ) +
  scale_fill_manual(
    values = c("d" = "#69b3a2", "m" = "#404080"),
    labels = c("d" = "WD and RM", "m" = "RD and DM")
  )

library(ggpubr)

# To make y-axis ranges the same, modify plot1 and plot2 before combining:
y_min <- min(c(layer_scales(plot1)$y$range$range, layer_scales(plot2)$y$range$range))
y_max <- max(c(layer_scales(plot1)$y$range$range, layer_scales(plot2)$y$range$range))

plot1 <- plot1 + ylim(y_min, y_max)
plot2 <- plot2 + ylim(y_min, y_max)

# --- Add titles manually above each plot ---
plot1 <- plot1 + ggtitle(expression(delta == 0.3))
plot2 <- plot2 + ggtitle(expression(delta == 0.7))

# Combine without labels, keep legend at bottom
combined_plot <- ggarrange(plot1, plot2,
                           ncol = 2,
                           common.legend = TRUE,
                           legend = "bottom")

# Display combined plot
combined_plot
ggsave("simulation/simulation1/simu1_boxplot.pdf", width = 8, height = 4)

#### qq plot ####

dG1 <- function(x,delta,log=FALSE){
  if(delta!=1/2){
    logres <- log( (1/(2*delta-1))*(exp(-x/delta) - exp(-x/(1-delta))) )
  } else{
    logres <- log(4)+log(x)-2*x
  }
  if(log){
    return(logres)
  } else{
    return(exp(logres))
  }
}

qG1 <- function(p,delta,log=FALSE){
  pmat <- p
  if(!is.matrix(p)){
    pmat <- matrix(p,nrow=1)
  }
  n <- nrow(pmat)
  D <- ncol(pmat)
  fun <- function(x,p,delta){
    return( pG1(exp(x),delta,log=TRUE,lower.tail=FALSE)-log(1-p) )
  }
  logres <- matrix(nrow=n,ncol=D)
  for (i in 1:n){
    for(j in 1:D){
      pi <- pmat[i,j]
      if(pi<=0){
        logres[i,j] <- -Inf
      } else if(pi>=1){
        logres[i,j] <- Inf
      } else{
        logres[i,j] <- uniroot(fun,interval=c(-5,5),p=pi,delta=delta,extendInt='yes')$root
      }
    }
  }
  if(!is.matrix(p)){
    logres <- as.vector(logres)
  }
  if(log){
    return(logres)
  } else{
    return(exp(logres))
  }
}

pG1 <- function(x,delta,log=FALSE,lower.tail=TRUE){
  if(delta!=1/2){
    if(lower.tail){
      logres <- log( 1-(delta/(2*delta-1))*exp(-x/delta) + ((1-delta)/(2*delta-1))*exp(-x/(1-delta)) )
    } else{
      logres <- log( (delta/(2*delta-1))*exp(-x/delta) - ((1-delta)/(2*delta-1))*exp(-x/(1-delta)) )
    }
  } else{
    if(lower.tail){
      logres <- log( 1-exp(-2*x)*(1+2*x) )
    } else{
      logres <- -2*x+log(1+2*x)
    }
  }
  if(log){
    return(logres)
  } else{
    return(exp(logres))
  }
}


hw.llh <- function(delta, x, log = FALSE){
  -2*sum(dG1(x, delta, log))
}



hw_qq <- function(x){
  hw_delta <- optimise(f = hw.llh, interval = c(0, 1), x = log(x[x>1]), log = TRUE)$minimum
  qq_data <- data.frame(
    hw = qG1((1:length(x)/(length(x) + 1)), hw_delta, log = FALSE),
    emp = sort(x)
  )
  return(qq_data)
}


qqdata_0.3 <- lapply(hw_delta_0.3, function(x) apply(x[[1]], 2, hw_qq))

qqdata_0.3_1 <- lapply(qqdata, function(x) x[[1]])
qqdata_0.3_2 <- lapply(qqdata, function(x) x[[2]])

qqdata_0.3_1 <- data.table::rbindlist(qqdata_0.3_1)
qqdata_0.3_1$group <- rep(1:1000, each = 1000)


qqdata_0.3_1

# Create Q-Q plot
ggplot(qqdata_0.3_1, aes(x = emp, y = hw, group = factor(group))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Q-Q Plot: HW vs Empirical",
       x = "HW Model Quantiles",
       y = "Empirical Quantiles") +
  theme_minimal()

#### chi plot ####

load("E:/R/EEA/simulation/simulation1/simu1_chi_0.3.Rdata")

u <- seq(0.5, 0.999, 0.001)
chi_emp <- lapply(chi_0.3, function(x) x[[1]])
chi_wd <- lapply(chi_0.3, function(x) x[[2]])
chi_wm <- lapply(chi_0.3, function(x) x[[3]])

chi_data <- data.frame(emp = unlist(chi_emp), 
                       wd = unlist(chi_wd),
                       wm = unlist(chi_wm),
                       u = rep(u, times = 1000), 
                       group = rep(1:1000, each = 500))


chi_mean <- chi_data %>%
  group_by(u) %>% 
  summarise(emp_mean = mean(emp, na.rm = TRUE), 
            emp_5 = quantile(emp, 0.05, na.rm = TRUE),
            emp_95 = quantile(emp, 0.95, na.rm = TRUE),
            wd_mean = mean(wd, na.rm = TRUE), 
            wd_5 = quantile(wd, 0.05, na.rm = TRUE),
            wd_95 = quantile(wd, 0.95, na.rm = TRUE),
            wm_mean = mean(wm, na.rm = TRUE), 
            wm_5 = quantile(wm, 0.05, na.rm = TRUE),
            wm_95 = quantile(wm, 0.95, na.rm = TRUE))

p1 <- ggplot(data = chi_mean, aes(x = u, y = emp_mean)) +
  # Empirical ribbon
  geom_ribbon(aes(ymin = emp_5, ymax = emp_95), fill = "gray", alpha = 0.6) +
  
  # MGPD ribbon and lines
     geom_ribbon(aes(ymin = wd_5, ymax = wd_95, fill = "WD and RM"), alpha = 0.4) +
     geom_line(aes(y = wd_5), color = "#4DA6FF") +
     geom_line(aes(y = wd_95), color = "#4DA6FF") +
  
  # FCM ribbon and lines
     geom_ribbon(aes(ymin = wm_5, ymax = wm_95, fill = "RD and WM"), alpha = 0.4) +
     geom_line(aes(y = wm_5), color = "#FF99B3") +
     geom_line(aes(y = wm_95), color = "#FF99B3") +
  
  # Mean lines
  geom_line(aes(x = u, y = wm_mean, color = "RD and WM"), size = 0.7) +
  geom_line(aes(x = u, y = wd_mean, color = "WD and RM"), size = 0.7) +
  geom_line(aes(x = u, y = emp_mean, color = "Empirical")) +
  
  # Color scales and theme
  scale_color_manual(name = "Method", 
                     values = c("Empirical" = "#000000", 
                                "WD and RM" = "#4566cd",    # Darker blue
                                "RD and WM" = "#e0543a")) +  # Deeper pink
  scale_fill_manual(values = c("WD and RM" = "#4DA6FF",     # Keep light blue for ribbon
                               "RD and WM" = "#FF99B3")) +   # Keep light pink for ribbon
  guides(fill = "none") +
  labs(y = "chi") +
  theme_bw()

load("E:/R/EEA/simulation/simulation1/simu1_chi_0.7.Rdata")

chi_emp <- lapply(chi_0.7, function(x) x[[1]])
chi_wd <- lapply(chi_0.7, function(x) x[[2]])
chi_wm <- lapply(chi_0.7, function(x) x[[3]])

chi_data_0.7 <- data.frame(emp = unlist(chi_emp), 
                       wd = unlist(chi_wd),
                       wm = unlist(chi_wm),
                       u = rep(u, times = 1000), 
                       group = rep(1:1000, each = 500))


chi_mean_0.7 <- chi_data_0.7 %>%
  group_by(u) %>% 
  summarise(emp_mean = mean(emp, na.rm = TRUE), 
            emp_5 = quantile(emp, 0.05, na.rm = TRUE),
            emp_95 = quantile(emp, 0.95, na.rm = TRUE),
            wd_mean = mean(wd, na.rm = TRUE), 
            wd_5 = quantile(wd, 0.05, na.rm = TRUE),
            wd_95 = quantile(wd, 0.95, na.rm = TRUE),
            wm_mean = mean(wm, na.rm = TRUE), 
            wm_5 = quantile(wm, 0.05, na.rm = TRUE),
            wm_95 = quantile(wm, 0.95, na.rm = TRUE))

p2 <- ggplot(data = chi_mean_0.7, aes(x = u, y = emp_mean)) +
  # Empirical ribbon
  geom_ribbon(aes(ymin = emp_5, ymax = emp_95), fill = "gray", alpha = 0.6) +
  
  # MGPD ribbon and lines
  geom_ribbon(aes(ymin = wd_5, ymax = wd_95, fill = "WD and RM"), alpha = 0.4) +
  geom_line(aes(y = wd_5), color = "#4DA6FF") +
  geom_line(aes(y = wd_95), color = "#4DA6FF") +
  
  # FCM ribbon and lines
  geom_ribbon(aes(ymin = wm_5, ymax = wm_95, fill = "RD and WM"), alpha = 0.4) +
  geom_line(aes(y = wm_5), color = "#FF99B3") +
  geom_line(aes(y = wm_95), color = "#FF99B3") +
  
  # Mean lines
  geom_line(aes(x = u, y = wm_mean, color = "RD and WM"), size = 0.7) +
  geom_line(aes(x = u, y = wd_mean, color = "WD and RM"), size = 0.7) +
  geom_line(aes(x = u, y = emp_mean, color = "Empirical")) +
  
  # Color scales and theme
  scale_color_manual(name = "Method", 
                     values = c("Empirical" = "#000000", 
                                "WD and RM" = "#4566cd",    # Darker blue
                                "RD and WM" = "#e0543a")) +  # Deeper pink
  scale_fill_manual(values = c("WD and RM" = "#4DA6FF",     # Keep light blue for ribbon
                               "RD and WM" = "#FF99B3")) +   # Keep light pink for ribbon
  guides(fill = "none") +
  labs(y = "chi") +
  theme_bw()

# Now combine the modified plots
combined_plot <- ggarrange(p1, p2,
                           ncol = 2,
                           common.legend = TRUE,
                           legend = "bottom",
                           labels = c("a)", "b)"))

# Display the combined plot
combined_plot
ggsave("simulation/simulation1/simu1_chiplot.pdf", width = 8, height = 4)

