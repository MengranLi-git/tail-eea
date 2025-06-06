
library(parallel)
library(SpatialADAI)


Echi = function(dat, u = 0.5){
  dt <- dat
  ranks <- apply(dt, 2, rank)
  k = nrow(ranks) * (1 - u)
  chiu = 2 - tailDepFun::stdfEmp(ranks, k)
  names(chiu) = NULL
  return(chiu)
}

chi_hd <- function(x){
  X <- hw_delta_0.3[[x]][[1]]
  parwd <- hw_delta_0.3[[x]][[4]]
  parwm <- hw_delta_0.3[[x]][[5]]
  
  u <- seq(0.5, 0.999, 0.001)
  
  chi_emp <- sapply(u, Echi, dat = X)
  chi_wd <- chiu2(u, parwd[1], parwd[2], model = "IEVL")
  chi_wm <- chiu2(u, parwm[1], parwm[2], model = "Gauss")
  
  print(x)
  return(list(emp = chi_emp,
              wd = chi_wd,
              wm = chi_wm))
}


load("simulation/simulation1/simu1_0.3.Rdata")
chi_0.3 <- mclapply(1:1000, function(x) chi_hd(x), mc.cores = 20)
save(chi_0.3, file = "simulation/simulation1/simu1_chi_0.3.Rdata")

load("simulation/simulation1/simu1_0.7.Rdata")
chi_0.7 <- mclapply(1:1000, function(x) chi_hd(x), mc.cores = 20)
save(chi_0.7, file = "simulation/simulation1/simu1_chi_0.7.Rdata")