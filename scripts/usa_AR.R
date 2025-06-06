
# define quantile
q5 <- function(x) quantile(x, probs = 0.05, na.rm = TRUE)
q95 <- function(x) quantile(x, probs = 0.95, na.rm = TRUE)


#### hw #####

n_grid <- 261
n_boot <- 300

hw_ar5_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
hw_ar50_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
hw_pn5_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
hw_pn50_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)

# load data
for (i in 1:n_boot) {
  file_path <- sprintf("E:/R/EEA/Philippe/ACCESS-CM2/result/hw/ar_hw/ar_hw%d.Rdata", i)
  if (!file.exists(file_path)) {
    message(sprintf("file missing: %s，replace with NA", file_path))
    next
  }
  
  load(file_path)  
  
  for (j in 1:n_grid) {
    val <- ar_hw[[j]]
    if (is.null(val)) {
      hw_ar5_mat[j, i] <- NA
      hw_ar50_mat[j, i] <- NA
      hw_pn5_mat[j, i] <- NA
      hw_pn50_mat[j, i] <- NA
    } else {
      hw_ar5_mat[j, i] <- ifelse(is.null(val[1]), NA, val[1])
      hw_ar50_mat[j, i] <- ifelse(is.null(val[2]), NA, val[2])
      hw_pn5_mat[j, i] <- ifelse(is.null(val[3]), NA, val[3])
      hw_pn50_mat[j, i] <- ifelse(is.null(val[4]), NA, val[4])
    }
  }
}



#### mgpd #####

mgpd_ar5_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
mgpd_ar50_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
mgpd_pn5_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
mgpd_pn50_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)

# load data
for (i in 1:n_boot) {
  file_path <- sprintf("E:/R/EEA/Philippe/ACCESS-CM2/result/mgpd/ar_mgpd/ar_mgpd%d.Rdata", i)
  
  if (!file.exists(file_path)) {
    message(sprintf("file missing: %s", file_path))
    next
  }
  
  load(file_path) 
  
  for (j in 1:n_grid) {
    val <- ar_mgpd[[j]]
    
    if (is.null(val) || length(val) < 3 || is.null(val[[3]])) {
      mgpd_ar5_mat[j, i] <- NA
      mgpd_ar50_mat[j, i] <- NA
      mgpd_pn5_mat[j, i] <- NA
      mgpd_pn50_mat[j, i] <- NA
    } else {
      v <- val[[3]] 
      mgpd_ar5_mat[j, i] <- ifelse(is.null(v[1]), NA, v[1])
      mgpd_ar50_mat[j, i] <- ifelse(is.null(v[2]), NA, v[2])
      mgpd_pn5_mat[j, i] <- ifelse(is.null(v[3]), NA, v[1])
      mgpd_pn50_mat[j, i] <- ifelse(is.null(v[4]), NA, v[2])
    }
  }
}
mgpd_ar5_mat = -mgpd_ar5_mat
mgpd_ar50_mat = -mgpd_ar50_mat
mgpd_pn5_mat = -mgpd_pn5_mat
mgpd_pn50_mat = -mgpd_pn50_mat

#### fcm #####

fcm_ar5_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
fcm_ar50_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
fcm_pn5_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
fcm_pn50_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)

# load data

for (i in 1:n_boot) {
  file_path <- sprintf("E:/R/EEA/Philippe/ACCESS-CM2/result/fcm/ar_fcm/arfcm%d.Rdata", i)
  
  if (!file.exists(file_path)) {
    message(sprintf("file missing: %s", file_path))
    next
  }
  
  load(file_path) 
  
  dat <- get("ar_fcm")
  
  for (j in 1:n_grid) {
    val <- dat[[j]]
    
    if (is.null(val) || inherits(val, "try-error")) {
      fcm_ar5_mat[j, i] <- NA
      fcm_ar50_mat[j, i] <- NA
      fcm_pn5_mat[j, i] <- NA
      fcm_pn50_mat[j, i] <- NA
    } else {
      fcm_ar5_mat[j, i]  <- ifelse(length(val) >= 1 && !is.null(val[1]), val[1], NA)
      fcm_ar50_mat[j, i] <- ifelse(length(val) >= 2 && !is.null(val[2]), val[2], NA)
      fcm_pn5_mat[j, i]  <- ifelse(length(val) >= 3 && !is.null(val[3]), val[3], NA)
      fcm_pn50_mat[j, i] <- ifelse(length(val) >= 4 && !is.null(val[4]), val[4], NA)
    }
  }
}

#### grid ####

# AR5

hw_mean_AR5 <- rowMeans(hw_ar5_mat, na.rm = TRUE)
hw_alpha_AR5 <- apply(hw_ar5_mat, 1, function(x) q95(x) - q5(x))

hw_mean_AR50 <- rowMeans(hw_ar50_mat, na.rm = TRUE)
hw_alpha_AR50 <- apply(hw_ar50_mat, 1, function(x) q95(x) - q5(x))

mgpd_mean_AR5 <- rowMeans(mgpd_ar5_mat, na.rm = TRUE)
mgpd_alpha_AR5 <- apply(mgpd_ar5_mat, 1, function(x) q95(x) - q5(x))

mgpd_mean_AR50 <- rowMeans(mgpd_ar50_mat, na.rm = TRUE)
mgpd_alpha_AR50 <- apply(mgpd_ar50_mat, 1, function(x) q95(x) - q5(x))


fcm_mean_AR5 <- rowMeans(fcm_ar5_mat, na.rm = TRUE)
fcm_alpha_AR5 <- apply(fcm_ar5_mat, 1, function(x) q95(x) - q5(x))

fcm_mean_AR50 <- rowMeans(fcm_ar50_mat, na.rm = TRUE)
fcm_alpha_AR50 <- apply(fcm_ar50_mat, 1, function(x) q95(x) - q5(x))

load("E:/R/EEA/Philippe/ACCESS-CM2/fdata.Rdata")

grid$hw_AR5_mean <- hw_mean_AR5
grid$hw_AR5_alpha <- hw_alpha_AR5
grid$hw_AR50_mean <- hw_mean_AR50
grid$hw_AR50_alpha <- hw_alpha_AR50

grid$fcm_AR5_mean <- fcm_mean_AR5
grid$fcm_AR5_alpha <- fcm_alpha_AR5
grid$fcm_AR50_mean <- fcm_mean_AR50
grid$fcm_AR50_alpha <- fcm_alpha_AR50

grid$mgpd_AR5_mean <- mgpd_mean_AR5
grid$mgpd_AR5_alpha <- mgpd_alpha_AR5
grid$mgpd_AR50_mean <- mgpd_mean_AR50
grid$mgpd_AR50_alpha <- mgpd_alpha_AR50


library(ggplot2)
library(dplyr)

usa_map <- map_data("state")

library(dplyr)
library(tidyr)

grid_long_AR5 <- grid %>%
  dplyr::select(lon, lat,
                fcm_AR5_mean, fcm_AR5_alpha,
                hw_AR5_mean, hw_AR5_alpha,
                mgpd_AR5_mean, mgpd_AR5_alpha) %>%
  pivot_longer(
    cols = -c(lon, lat),
    names_to = c("method", ".value"),
    names_pattern = "(fcm|hw|mgpd)_AR5_(mean|alpha)"
  ) %>%
  mutate(method = toupper(method))  

grid_long_AR50 <- grid %>%
  dplyr::select(lon, lat,
                fcm_AR50_mean, fcm_AR50_alpha,
                hw_AR50_mean, hw_AR50_alpha,
                mgpd_AR50_mean, mgpd_AR50_alpha) %>%
  pivot_longer(
    cols = -c(lon, lat),
    names_to = c("method", ".value"),
    names_pattern = "(fcm|hw|mgpd)_AR50_(mean|alpha)"
  ) %>%
  mutate(method = toupper(method))  



breaks <- c(-Inf, -0.01, 0.01,  Inf)
breaks2 = c(seq(0, 2, 0.2), Inf)

adjusted_margin <- margin(t = 1.5, r = 0.5, b = 1.5, l = 0.5, unit = "points")

alpha_theme <-  scale_alpha_continuous(range = c(0.25, 0.85))


color_manual <- scale_color_manual(values = c("1" = "#d73027","2" = "#f7f7f7",
                                              "3" = "#4575b4"),
                                   name = "AR", 
                                   labels = c("Negative",  "Zero", "Positive"))

grid_long_AR5$mean <- cut(grid_long_AR5$mean, 
                          breaks = breaks, 
                          labels = 1:(length(breaks) - 1))

grid_long_AR5$alpha <- cut(grid_long_AR5$alpha, 
                           breaks = breaks2, 
                           labels = 1:(length(breaks2) - 1))


grid_long_AR50$mean <- cut(grid_long_AR50$mean, 
                           breaks = breaks, 
                           labels = 1:(length(breaks) - 1))

grid_long_AR50$alpha <- cut(grid_long_AR50$alpha, 
                            breaks = breaks2, 
                            labels = 1:(length(breaks2) - 1))


grid_long_AR5$alpha <- as.numeric(grid_long_AR5$alpha)
grid_long_AR50$alpha <- as.numeric(grid_long_AR50$alpha)

# Update method labels for display
grid_long_AR5$method <- recode(grid_long_AR5$method,
                               "FCM" = "eFCM",
                               "HW" = "HW",
                               "MGPD" = "mGPD")

grid_long_AR50$method <- recode(grid_long_AR50$method,
                                "FCM" = "eFCM",
                                "HW" = "HW",
                                "MGPD" = "mGPD")


save(grid_long_AR50, file = "usa_AR.Rdata")
