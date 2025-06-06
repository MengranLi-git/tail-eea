
# define quantile
q5 <- function(x) quantile(x, probs = 0.05, na.rm = TRUE)
q95 <- function(x) quantile(x, probs = 0.95, na.rm = TRUE)


#### hw #####

n_grid <- 881
n_boot <- 300

hw_ar5_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
hw_ar50_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
hw_pn5_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
hw_pn50_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)

# load data and fill NA
for (i in 1:n_boot) {
  file_path <- sprintf("E:/R/EEA/Euro/hw/ar_hw/ar_hw%d.Rdata", i)
  if (!file.exists(file_path)) {
    message(sprintf("file missing: %s，replace with NA", file_path))
    next
  }
  
  load(file_path)  
  
  for (j in 1:n_grid) {
    val <- as.numeric(ar_hw[[j]])
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
  file_path <- sprintf("E:/R/EEA/Euro/mgpd/ar_mgpd/ar_mgpd%d.Rdata", i)
  
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



#### fcm #####

# initialize matrix
fcm_ar5_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
fcm_ar50_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
fcm_pn5_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)
fcm_pn50_mat <- matrix(NA, nrow = n_grid, ncol = n_boot)

# load data

for (i in 1:n_boot) {
  file_path <- sprintf("E:/R/EEA/Euro/fcm/ar_fcm/ar_fcm%d.Rdata", i)
  
  if (!file.exists(file_path)) {
    message(sprintf("file missing: %s", file_path))
    next
  }
  
  load(file_path)  # load data
  
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


ar_mgpd5 <- pmin(pmax(-mgpd_ar5_mat, -1), 1)
ar_mgpd50 <- pmin(pmax(-mgpd_ar50_mat, -1), 1)
# Create the data frame
map_AR_MGPD = data.frame(
  lon = grid[, 'lon'],
  lat = grid[, 'lat'],
  AR_5 = apply(ar_mgpd5, 1, mean, na.rm = TRUE),
  AR_50 = apply(ar_mgpd50, 1, mean, na.rm = TRUE),
  AR_5_alpha = (apply(ar_mgpd5, 1, quantile, 0.975, na.rm = TRUE) - apply(ar_mgpd5, 1, quantile, 0.025, na.rm = TRUE)),
  AR_50_alpha = (apply(ar_mgpd50, 1, quantile, 0.975, na.rm = TRUE) - apply(ar_mgpd50, 1, quantile, 0.025, na.rm = TRUE))
)

ar_fcm5 <- pmin(pmax(fcm_ar5_mat, -1), 1)
ar_fcm50 <- pmin(pmax(fcm_ar50_mat, -1), 1)
# Create the data frame
map_AR_eFCM = data.frame(
  lon = grid[, 'lon'],
  lat = grid[, 'lat'],
  AR_5 = apply(ar_fcm5, 1, mean, na.rm = TRUE),
  AR_50 = apply(ar_fcm50, 1, mean, na.rm = TRUE),
  AR_5_alpha = (apply(ar_fcm5, 1, quantile, 0.975, na.rm = TRUE) - apply(ar_fcm5, 1, quantile, 0.025, na.rm = TRUE)),
  AR_50_alpha = (apply(ar_fcm50, 1, quantile, 0.975, na.rm = TRUE) - apply(ar_fcm50, 1, quantile, 0.025, na.rm = TRUE))
)

ar_fcm5 <- pmin(pmax(fcm_ar5_mat, -1), 1)
ar_fcm50 <- pmin(pmax(fcm_ar50_mat, -1), 1)
# Create the data frame
map_AR_eFCM = data.frame(
  lon = grid[, 'lon'],
  lat = grid[, 'lat'],
  AR_5 = apply(ar_fcm5, 1, mean, na.rm = TRUE),
  AR_50 = apply(ar_fcm50, 1, mean, na.rm = TRUE),
  AR_5_alpha = (apply(ar_fcm5, 1, quantile, 0.975, na.rm = TRUE) - apply(ar_fcm5, 1, quantile, 0.025, na.rm = TRUE)),
  AR_50_alpha = (apply(ar_fcm50, 1, quantile, 0.975, na.rm = TRUE) - apply(ar_fcm50, 1, quantile, 0.025, na.rm = TRUE))
)

save.image("script/Euro_AR.Rdata")