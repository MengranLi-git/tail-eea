
library(SpatialADAI)
library(mvtnorm)
library(mvPot)
library(fields)
library(evd)
library(EnvStats)

source("E:/R/EEA/simulation/mgpd.R")
source("E:/R/EEA/simulation/fcmtools.R")


pd <- list()
pm <- list()
parad <- list()
param <- list()

n <- 1000

thresh <- 0.9
p1 <- 0.05
p2 <- 0.02
p3 <- 0.01

theta <- 0.8
delta <- 0.7
# delta <- 0.3


for(i in 1:1000){
  set.seed(2345*i)
  source("E:/R/EEA/simulation/cop.R")
  pd[[i]] <- wrongd
  pm[[i]] <- wrongm
  parad[[i]] <- pard
  param[[i]] <- parm
  
  save(pd, pm,parad, param,  file = "E:/R/EEA/simulation/cop_0.7.Rdata")
  print(i)
}




