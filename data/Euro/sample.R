library(FCM)

set.seed(1)
data = cf$data
n = nrow(data)

B <- 1000
ind <- lapply(1:B, function(x) sample(1:n, n, TRUE)) 
d = nrow(cf$grid)

fit <- list()
for(i in 1:B){
  cf$data <- data[, ind[[i]]]
  fit[[i]] = lapply(1:d, function(x) fcm(x, cf, progress = FALSE))
  save(fit, file = "fit_fcm_counterfactual.Rdata")
}

