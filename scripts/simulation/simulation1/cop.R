


dat <- rC2(n, delta = delta,theta = theta,model = "Gauss", scale = "x")

X = dat[, 1]
Y = dat[, 2]

# empirical r

# fit the correct dependence and correct margins

u.np = function(x) {
  rank(x, ties.method = "random", na.last = "keep")/(length(x[!is.na(x)]) + 1)
}

Xu <- u.np(X)
Yu <- u.np(Y)
datU <- cbind(Xu, Yu)

v1 = quantile(apply(dat, 1, mean), 1-p1)
v2 = quantile(apply(dat, 1, mean), 1-p2)
v3 = quantile(apply(dat, 1, mean), 1-p3)

# fit the incorrect dependence and correct margins
skip_to_next <- FALSE

tryCatch({fit1 <- fit.cop.2dim(datU = datU, thresh = thresh, init.val = c(0.1, theta), cens.type = "max", model = "IEVL", optim = TRUE)}, error = function(e) { skip_to_next <<- TRUE})
if(skip_to_next) { next } 
# compute the probability by simulation
simurg <- rC2(1e5, delta = unlist(fit1$mle[1]), theta = unlist(fit1$mle[2]), model = "IEVL", scale = "x")

## probability
pd1 <- length(which(apply(simurg, 1, mean, na.rm = TRUE) > v1))/nrow(simurg)
pd2 <- length(which(apply(simurg, 1, mean, na.rm = TRUE) > v2))/nrow(simurg)
pd3 <- length(which(apply(simurg, 1, mean, na.rm = TRUE) > v3))/nrow(simurg)

wrongd <- c(pd1, pd2, pd3)

pard <- fit1$mle

# correct strcture, wrong margins

# Transform X_star back to Pareto using inverse CDF

X_star <- pexp(X, 1)
Y_star <- pexp(Y, 1)

X_dot <- qpareto(X_star, delta)
Y_dot <- qpareto(Y_star, delta)

X. <- u.np(X_dot)
Y. <- u.np(Y_dot)

dat. <- cbind(X., Y.)

fit2 <- fit.cop.2dim(datU = dat., thresh = thresh, init.val = c(delta, theta), cens.type = "max", model = "Gauss", optim = TRUE)

simurg <- rC2(1e5, delta = unlist(fit2$mle[1]), theta = unlist(fit2$mle[2]), model = "Gauss", scale = "x")

## probability
pm1 <- length(which(apply(simurg, 1, mean, na.rm = TRUE) > v1))/nrow(simurg)
pm2 <- length(which(apply(simurg, 1, mean, na.rm = TRUE) > v2))/nrow(simurg)
pm3 <- length(which(apply(simurg, 1, mean, na.rm = TRUE) > v3))/nrow(simurg)

wrongm <- c(pm1, pm2, pm3)

parm <- fit2$mle
