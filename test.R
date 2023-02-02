source('fbp.R')
do_test <- function() {
  lat <- 50
  lon <- -90
  wx <- get_weather(lat, lon, init=list(ffmc=90, dmc=50, dc=300, percent_cured=100))
  time <- min(wx$DATETIME) + hours(10)
  sim_env <- createSimulationEnvironment(lat, lon)
  pt <- sim_env$origin
  landscape <- sim_env$landscape
  start_fire(landscape, lat, lon, time)
  spread(landscape, wx)
}
do_test()
