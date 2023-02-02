library('data.table')
library('lutz')

dir_root <- getwd()
setwd(paste0(dir_root, '/cffdrs-ng'))
source('make_minmax.r')
source('make_hourly.r')
source('NG_FWI.r')
source('old_cffdrs.r')
setwd(dir_root)

get_weather <- function(lat, lon, init=list(ffmc=85, dmc=15, dc=6, percent_cured=100)) {
  wx <- data.table(read.csv('./cffdrs-ng/test_hffmc.csv'))
  wx$lat <- lat
  wx$long <- lon
  # HACK: make random wind directions that seem reasonable
  set.seed(lat)
  wx$wd <- as.integer(runif(1, 0, 360)[[1]] + cumsum(rnorm(nrow(wx), mean=0, sd=20))) %% 360
  names(wx) <- toupper(names(wx))
  # move precip to end
  cols <- c(setdiff(names(wx), c('PREC')), 'PREC')
  wx <- wx[, ..cols]
  timezone <- lutz::tz_lookup_coords(lat, lon)
  day1 <- wx[1,]
  date_start <- make_date(day1$YR, day1$MON, day1$DAY)
  tz <- tz_offset(date_start, timezone)$utc_offset_h
  # HACK: remove all rain
  wx$PREC <- 0
  # HACK: halve RH so things actually happen
  wx$RH <- wx$RH / 2
  wx <- hFWI(wx, tz, ffmc_old=init$ffmc, dmc_old=init$dmc, dc_old=init$dc, percent_cured=init$percent_cured)
  # HACK: try to minimize memory usage
  gc()
  wx[, DATETIME := make_datetime(YR, MON, DAY, HR, 0, tz=timezone)]
  wx <- wx[, -c('LAT', 'LONG', 'YR', 'MON', 'DAY', 'HR', 'MIN_RH', 'SUNLIGHT_HOURS')]
  cols <- c('DATETIME', setdiff(names(wx), c('DATETIME')))
  wx <- wx[, ..cols]
  COLS_SOLAR <- c('SUNRISE', 'SUNSET', 'SOLRAD', 'SOLPROP')
  cols <- c(setdiff(names(wx), COLS_SOLAR), COLS_SOLAR)
  wx <- wx[, ..cols]
  return(wx)
}
