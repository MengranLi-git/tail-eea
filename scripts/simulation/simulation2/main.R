
library(mvtnorm)
library(eFCM)
library(SpatialADAI)
library(cluster)
library(tailDepFun) 
library(maps) 
library(SpatialExtremes)
library(parallel)
library(fields)

source("simulation/mgpd.R") 


#  delta = 0.3
#  delta = 0.4
#  delta = 0.7
#  delta = 0.7
#  delta = 0.8

delta = 0.5

n <- 1000
thresh <- 0.9
p1 <- 0.05
p2 <- 0.02
p3 <- 0.01

#for(i in 1:n){
#  set.seed(4956*i)
#  source("simulation/simulation2/threemodel.R")
#  
#  hwlist <- list(hwp, parhw)
#  mgpdlist <- list(mgpdp, parmgpd)
#  fcmlist <- list(fcmp, parfcm)
#  dat <- X
#  chilist <- list(chi_emp, chi_hw, chi_mgpd, chi_fcm)
#  
#  print(i)
#  save(coplist, mgpdlist, fcmlist, dat, chilist, file = "E:/R/EEA/simulation/chi_delta=0.7.Rdata")
#}

three <- function(x){
  set.seed(4956*x)
  source("simulation/simulation2/threemodel.R")
  hwlist <- list(hwp, parhw)
  mgpdlist <- list(mgpdp, parmgpd)
  fcmlist <- list(fcmp, parfcm)
  dat <- X
  chilist <- list(chi_emp, chi_hw, chi_mgpd, chi_fcm)
  print(x)
  return(list(hwlist, mgpdlist,fcmlist,  dat, chilist)) 
}


#delta_0.3 <- mclapply(1:n, function(x) three(x), mc.cores = 20)
#save(delta_0.3, file = "simulation/simulation2/delta_0.3.Rdata")
#print(0.3)

delta = 0.4
delta_0.4 <- mclapply(1:n, function(x) three(x), mc.cores = 20)
save(delta_0.4, file = "simulation/simulation2/delta_0.4.Rdata")
print(0.4)


delta = 0.6
delta_0.6 <- lapply(1:n, function(x) three(x))
#save(delta_0.6, file = "simulation/simulation2/delta_0.6.Rdata")
#print(0.6)
#
#delta = 0.7
#delta_0.7 <- mclapply(1:n, function(x) three(x), mc.cores = 20)
#save(delta_0.7, file = "simulation/simulation2/delta_0.7.Rdata")
#print(0.7)
#
#
delta = 0.8
delta_0.8 <- mclapply(1:100, function(x) three(x), mc.cores = 20)
save(delta_0.8, file = "simulation/simulation2/delta_0.8.Rdata")
print(0.8)
