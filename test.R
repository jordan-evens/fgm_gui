source('fbp.R')
do_test <- function() {
  lat <- 50
  lon <- -90
  wx <- get_weather(lat, lon, init=list(ffmc=90, dmc=50, dc=300, percent_cured=100))
  time <- min(wx$DATETIME) + hours(10)
  landscape_all <- createLandscape(lat, lon)
  pt <- landscape_all$origin
  landscape <- landscape_all$data
  start_fire(landscape, lat, lon, time)
  spread(landscape, wx)
}
do_test()
