
Echi = function(dat, u = 0.5){
  dt <- dat
  ranks <- apply(dt, 2, rank)
  k = nrow(ranks) * (1 - u)
  chiu = 2 - stdfEmp(ranks, k)
  names(chiu) = 'Echi'
  return(chiu)
}
########################## generate simulation ##########################

# gaussian process

#the first parameter is delta (weight parameter), the two other are for the correlation function in W
par0 <- c(delta, 2, 1) 

coord <- cbind(runif(2,0,5),runif(2,0,5))

# power exponential model
sigma0 <- exp(-(as.matrix(dist(coord))/par0[2])^par0[3])
theta = sigma0[1, 2]
X <- rC2(n, delta, theta, model = "Gauss", scale = "x")

# empirical return level

v1 = quantile(apply(X, 1, mean), 1-p1)
v2 = quantile(apply(X, 1, mean), 1-p2)
v3 = quantile(apply(X, 1, mean), 1-p3)

#########################################################################

########################## hw model  ###################################

# take uniform scale
datU <- apply(X, 2, rank)/(n + 1)

# initial value
par <- c(delta, sigma0[1, 2])

# model fitting

hwfit <- tryCatch(fit.cop.2dim(datU, thresh, par, cens.type = "max", model = "Gauss", optim = TRUE), 
                  error = function(e) {
  message("Error encountered: ", e$message)
})

if(length(hwfit) != 0){
# compute the probability by simulation
    parhw <- hwfit$mle

    simurg <- rC2(1e05, delta = parhw[1], theta = parhw[2], model = "Gauss", scale = "x")

## probability
    p1. <- length(which(apply(simurg, 1, mean, na.rm = TRUE) > v1))/nrow(simurg)
    p2. <- length(which(apply(simurg, 1, mean, na.rm = TRUE) > v2))/nrow(simurg)
    p3. <- length(which(apply(simurg, 1, mean, na.rm = TRUE) > v3))/nrow(simurg)

    hwp <- c(p1., p2., p3.)
    u <- seq(0.5, 0.999, 0.001)
    chi_hw <- chiu2(u, parhw[1], parhw[2], model = "Gauss")
}else{
    hwp <- NA
    parhw <- NA
    chi_hw <- NA
}
 


#########################################################################

############################ MGPD  ######################################

mgpdfit <- fitPNmulti(X, K = 2, N = 5, thres = thresh, v = v1, wopt = FALSE, CI = FALSE, nsim = 5e+05)
p1. <- mgpdfit$p0[1]

mgpdfit <- fitPNmulti(X, K = 2, N = 5, thres = thresh, v = v2, wopt = FALSE, CI = FALSE, nsim = 5e+05)
p2. <- mgpdfit$p0[1]

mgpdfit <- fitPNmulti(X, K = 2, N = 5, thres = thresh, v = v3, wopt = FALSE, CI = FALSE, nsim = 5e+05)
p3. <- mgpdfit$p0[1]

mgpdp <- c(p1., p2., p3.)

sigma = mgpdfit$mar$sigma
gamma = mgpdfit$mar$gamma

u0 <- apply(X, 2, function(i) quantile(i, thresh))
datam0 <- selectExc(X, u0)
mgpdexp <- fit.MGPD.RevExpT(x=datam0, u=rep(0,2), std=F, dep.scale.fix=F,
                         marg.scale.ind = c(1,2),
                         marg.shape.ind = c(1,1),
                         marg.scale.start = sigma,
                         marg.shape.start = mean(gamma))

parmgpd = mgpdexp$mle
parmgpd[7] = parmgpd[6]

#########################################################################

############################ FCM  #######################################

# model fitting

init <- c(2, 200)
cf <- fdata(data = X, coord = coord, grid = coord, neigh = list(c(1,2)))

fcmfit <- fcm(1, cf, theta0 = init)

parfcm <- fcmfit$par


# generate simulation by EEA


  par = fcmfit$par
  M <- Matern(dist(coord), range = par[2], smoothness = 0.5)
  Z <- rmnorm(1e5, varcov = matrix(c(1, M, M, 1), ncol = 2))
  V <- rexp(1e5, rate = par[1])
  sim2 <- Z + V
  
  
  p1. <- length(which(apply(sim2, 1, mean) > qfcm(1-p1, par[1])))/nrow(sim2)
  p2. <- length(which(apply(sim2, 1, mean) > qfcm(1-p2, par[1])))/nrow(sim2)
  p3. <- length(which(apply(sim2, 1, mean) > qfcm(1-p3, par[1])))/nrow(sim2)

fcmp <- c(p1., p2., p3.)
rm(p1., p2., p3.)

## chi dependent structure

u <- seq(0.5, 0.999, 0.001)

### HW model

chi_emp <- sapply(u, Echi, dat = X)
chi_mgpd <- simuchi(parmgpd, u, 1e5)
chi_fcm <- sapply(u, eFCM::chi, h = unlist(rdist.earth(coord)), lbda = parfcm[1], delta = parfcm[2], nu = 0.5)



