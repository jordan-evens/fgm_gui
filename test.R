source('fbp.R')
do_test <- function() {
  lat <- 50
  lon <- -90
  wx <- get_weather(lat, lon, init=list(ffmc=90, dmc=50, dc=300, percent_cured=100))
  time <- min(wx$DATETIME) + hours(10)
  sim_env <- getSimulationEnvironment(lat, lon)
  pt <- sim_env$origin
  landscape <- sim_env$landscape
  new_sim_env <- start_fire(sim_env, lat, lon, time)
  stopifnot(new_sim_env == sim_env)
  spread(sim_env, wx)
}
do_test()
