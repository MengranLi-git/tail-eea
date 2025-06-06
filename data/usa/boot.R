
library(eFCM)
library(parallel)

load("/home/pgrad2/2592713l/EEA/usa/fdata.Rdata")
load("/home/pgrad2/2592713l/EEA/usa/id.Rdata")

factual <- fdata(X0, coord = coord,grid =grid[, -1], neigh= neigh)

fit_cf <- mclapply(1:length(neigh), function(x) fcm(i, cf, hessian = FALSE, theta0 = c(2, 100)), mc.cores = 30)
fit_f <- mclapply(1:length(neigh), function(x) fcm(i, factual, hessian = FALSE, theta0 = c(2, 100)), mc.cores = 30)

save(fit_cf, fit_f, file = "/home/pgrad2/2592713l/EEA/usa/point_estimation.Rdata")
                  

for(i in 1:300){
  counterfactual <- fdata(X0[which(block %in% id[[i]]), ], coord = coord, grid = grid, neigh = neigh)
  factual <- fdata(X1[which(block %in% id[[i]]), ], coord = coord, grid = grid, neigh = neigh)
  
  fit_cf <- mclapply(1:length(neigh), function(x) fcm(i, counterfactual, hessian = FALSE, theta0 = c(2, 100)), mc.cores = 30)
  save(fit_cf, file = paste0("/home/pgrad2/2592713l/EEA/usa/fit_cf/fit_cf", i, ".Rdata"))
  
  fit_f <- mclapply(1:length(neigh), function(x) fcm(i, factual, hessian = FALSE, theta0 = c(2, 100)), mc.cores = 30)
  save(fit_f, file = paste0("/home/pgrad2/2592713l/EEA/usa/fit_f/fit_f", i, ".Rdata"))
  
  rm(fit_cf, fit_f, counterfactual,factual)
  print(i)
}

