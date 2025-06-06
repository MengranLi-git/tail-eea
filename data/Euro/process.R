prEuropeCounterFactual <- readRDS("data/prEuropeCounterFactual.RDS")
WinterClock <- readRDS("data/WinterClock.RDS")
LonLat <- readRDS("data/LonLat.RDS")

index = rep(1:ceiling(length(WinterClock)/7), each = 7)[1:length(WinterClock)]

datamat = apply(prEuropeCounterFactual, 2, function(x) tapply(x, index, max))

# column is each station
datamat = as.data.frame(datamat)
grid = as.data.frame(LonLat)
id = 1:nrow(grid)
grid = cbind(id, grid)
coord = LonLat

save(datamat, grid, coord, file = 'data/CounterFactual.Rdata')

load('data/neigh.Rdata')
grid = grid[center, ]

cf <- fdata(datamat, coord, grid[, -1])

save(cf, file = 'data/cf_CounterFactual.Rdata')



##################################################################

prEuropeFactual <- readRDS("data/prEuropeFactual.RDS")
WinterClock <- readRDS("data/WinterClock.RDS")
LonLat <- readRDS("data/LonLat.RDS")

index = rep(1:ceiling(length(WinterClock)/7), each = 7)[1:length(WinterClock)]

datamat = apply(prEuropeFactual, 2, function(x) tapply(x, index, max))

# column is each station
datamat = as.data.frame(datamat)
grid = as.data.frame(LonLat)
id = 1:nrow(grid)
grid = cbind(id, grid)
coord = LonLat

save(datamat, grid, coord, file = 'data/Factual.Rdata')

load('data/neigh.Rdata')
grid = grid[center, ]
cf <- fdata(datamat, coord, grid = grid[, -1], neigh)

save(cf, file = 'data/cf_Factual.Rdata')
