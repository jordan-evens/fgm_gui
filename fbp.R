source('common.R')
source('weather.R')
library(sf)

NUM_CELLS <- 200
RAD_180 <- 180 / pi
MIN_ROS <- 0.1

# simulation variables
BASE_DATA <- ensure_data()

rad2deg <- function(theta) {
  return(theta * RAD_180)
}

deg2rad <- function(theta) {
  return(theta / RAD_180)
}

get_point <- function(landscape, lat, lon) {
  stopifnot(!is.null(lat))
  stopifnot(!is.null(lon))
  pt <- st_as_sf(data.frame(longitude=lon, latitude=lat), coords=c('longitude', 'latitude'), crs=PROJ_DEFAULT)
  print('In get_point()')
  pt_proj <- st_transform(pt, st_crs(landscape))
  return(pt_proj)
}

round_pt_precision <- function(lat, lon) {
  fbp_orig <- BASE_DATA$TIF_FBP
  pt <- st_as_sf(data.frame(latitude=lat, longitude=lon), coords=c('longitude', 'latitude'), crs=PROJ_DEFAULT)
  pt_proj <- st_transform(pt, st_crs(fbp_orig))
  pt_rounded <- st_transform(pt_proj, PROJ_DEFAULT)
  xy <- st_coordinates(pt_rounded)[1,]
  lon_rounded <- xy[1]
  lat_rounded <- xy[2]
  print(sprintf('Rounded (%f, %f) to (%f, %f)', lon, lat, lon_rounded, lat_rounded))
  return(list(lon=lon_rounded, lat=lat_rounded))
}

is_unburnable <- function(fueltype) {
  r <- copy(fueltype)
  names(r) <- 'unburnable'
  values(r) <- values(r) %in% BASE_DATA$NON_FUEL_VALUES
  return(r)
}

getSimulationEnvironment <- function(lat, lon, sim_env=NULL) {
  tif_fbp <- BASE_DATA$TIF_FBP
  if (!check_in_bounds(tif_fbp, lat, lon)) {
    print("Out of bounds")
    return()
  }
  # HACK: don't create landscape if in same cell as we are already
  pt <- get_point(tif_fbp, lat, lon)
  stopifnot(!is.null(pt))
  tif_fbp_cell <- cellFromXY(tif_fbp, st_coordinates(pt))
  pt_origin <- st_as_sf(data.frame(latitude=lat, longitude=lon), coords=c('longitude', 'latitude'), crs=PROJ_DEFAULT)
  if (!is.null(sim_env)  && sim_env$origin_cell == tif_fbp_cell) {
    print('keeping existing landscape')
    # clear burnt cells
    values(sim_env$landscape$burnt) <- 0
    # don't need to reset this if we don't edit it, but look into that later
    sim_env$landscape$unburnable <- is_unburnable(sim_env$landscape$fueltype)
    sim_env$origin <- pt_origin
    return(sim_env)
  }
  print('In getSimulationEnvironment()')
  pt_proj <- st_transform(pt_origin, st_crs(tif_fbp))
  b <- st_bbox(pt_proj)
  dist <- NUM_CELLS * res(tif_fbp)
  box <- ext(c(b$xmin - dist[1] / 2, b$xmax + dist[1] / 2, b$ymin - dist[2] / 2, b$ymax + dist[2] / 2))
  clipped <- tryCatch(crop(tif_fbp, box), error=function(e) { NULL })
  if (is.null(clipped)) {
    warning("Nothing in clipped area")
    return()
  }
  band <- names(clipped)[[1]]
  if (!all(is.nan(minmax(clipped[[band]])))) {
    tif_elev <- get_elevation(clipped)
    # HACK: try to minimize memory usage
    gc()
    tif_slope_percent <- as.integer(tan(terrain(tif_elev, v='slope', unit='radians')) * 100)
    tif_aspect_degrees <- as.integer(terrain(tif_elev, v='aspect', unit='degrees'))
    # HACK: convert for leaflet
    fueltype <- raster::raster(clipped)
    burnt <- copy(fueltype)
    names(burnt) <- 'burnt'
    values(burnt) <- 0
    unburnable <- is_unburnable(fueltype)
    # NOTE: use integer for everything because that should be precise enough
    landscape <- as.integer(raster::stack(list(fueltype=fueltype,
                                               elevation=tif_elev,
                                               slope=tif_slope_percent,
                                               aspect=tif_aspect_degrees,
                                               burnt=burnt,
                                               unburnable=unburnable)))
    sim_env <- (list(origin=pt_origin,
                     origin_cell=tif_fbp_cell,
                     landscape=landscape))
    return(sim_env)
  }
  return()
}

getCells <- function(landscape, pts) {
  print('In getCells()')
  print(pts)
  print('st_transform()')
  pts_proj <- st_transform(pts, st_crs(landscape))
  print(pts_proj)
  # cell <- as.list(extract(landscape, pt)[1,])
  # HACK: convert to SpatRaster to get cell number for now
  cell <- extract(rast(landscape), pts_proj, cells=TRUE)
  print(cell)
  if (any(is.na(cell))) {
    print('NA values extracted for pts')
    # return()
  }
  df <- as.data.table(cell)
  names(df) <- toupper(names(df))
  df$FUELTYPE <- to_fuel_abbreviation(BASE_DATA$FCT_NAMES_FBP(df$FUELTYPE))
  print('returning cells')
  cell <- unique(df$CELL)
  cell <- as.data.table(cbind(cell, rowColFromCell(landscape, cell)))
  names(cell) <- toupper(names(cell))
  df <- df[cell, on=c('CELL')]
  return(df)
}
calcFBP <- function(pt_cells, wx, time, lat, lon) {
  if (is.null(pt_cells)) {
    print('calcFBP called with NULL cell')
    return()
  }
  # only calculate unique values
  CELL_ATTRIBUTES <- c('FUELTYPE', 'ELEVATION', 'SLOPE', 'ASPECT')
  cells_unique <- unique(pt_cells[, ..CELL_ATTRIBUTES])
  # cells_unique <- cells_unique[!is.na(FUELTYPE),]
  wx_time <- wx[DATETIME == floor_date(time, unit='hour'),]
  stopifnot(0 < nrow(wx_time))
  df_fwi <- wx_time[, as.list(cells_unique), by=names(wx_time)]
  df_fwi$LAT <- lat
  df_fwi$LONG <- lon
  df_fwi$DJ <- lubridate::yday(time)
  names(df_fwi) <- toupper(names(df_fwi))
  df_fbp <- data.table(cffdrs::fbp(df_fwi, output='ALL'))
  df <- cbind(df_fbp, cells_unique)
  # exclude columns from wx_time that are already in df
  keep_cols <- setdiff(names(wx_time), intersect(names(wx_time), names(df)))
  wx_keep <- wx_time[, ..keep_cols]
  df <- df[, as.list(wx_keep), by=names(df)]
  df$DATETIME <- time
  names(df) <- toupper(names(df))
  # HACK: remove ID because fbp() adds it and it doesn't match point ids
  df <- df[, -c('ID')]
  # join back to original cells so they can be used per-cell outside this
  by_cell <- unique(pt_cells[, -c('ID')])
  df <- df[by_cell, , on=CELL_ATTRIBUTES]
  return(df)
}
calc_offsets <- function(fbp) {
  has_no_slope <- 0 == fbp$SLOPE
  b_semi <- ifelse(has_no_slope, 0, cos(atan(fbp$SLOPE / 100.0)))
  slope_radians <- deg2rad(fbp$ASPECT)
  raz <- deg2rad(fbp$WD + 180)
  # do check once and make function just return 1.0 if no slope
  correction_factor <- ifelse(has_no_slope,
                              function (theta) { return(1.0) },
                              function (theta) {
                                # figure out how far the ground distance is in map distance horizontally
                                angle_unrotated <- theta - slope_radians
                                # CHECK: if we're going directly across the slope then horizontal distance is same as spread distance
                                if (rad2deg(angle_unrotated) %in% c(270, 90)) {
                                  return(1.0)
                                }
                                tan_u <- tan(angle_unrotated);
                                y <- b_semi / sqrt(b_semi * tan_u * (b_semi * tan_u) + 1.0)
                                x <- y * tan_u
                                # CHECK: Pretty sure you can't spread farther horizontally than the spread distance, regardless of angle?
                                return(min(1.0, sqrt(x * x + y * y)))
                              })
  # add_offset <- function(direction, ros) {
  #   # this was from when we were tracking distance within cells, but if we're just using absolute coordinates then don't do this
  #   # spreading, so figure out offset from current point
  #   ros_cell <- ros / cell_size
  #   return(c(ros_cell[1] * sin(direction), ros_cell[2] * cos(direction)))
  # }
  calc_offset <- function(direction, ros) {
    return(list(X=ros * sin(direction), Y=ros *cos(direction)))
  }
  head_ros <- fbp$ROS
  back_ros <- fbp$BROS
  a <- (head_ros + back_ros) / 2.0
  c <- a - back_ros
  flank_ros <- a / fbp$LB
  a_sq <- a * a
  flank_ros_sq <- flank_ros * flank_ros
  a_sq_sub_c_sq <- a_sq - (c * c)
  ac <- a * c
  calculate_ros <- function(theta) {
    cos_t <- cos(theta)
    cos_t_sq <- cos_t * cos_t
    f_sq_cos_t_sq <- flank_ros_sq * cos_t_sq
    sin_t <- sin(theta)
    sin_t_sq <- sin_t * sin_t
    return(abs((a * ((flank_ros * cos_t * sqrt(f_sq_cos_t_sq + a_sq_sub_c_sq * sin_t_sq) - ac * sin_t_sq) / (f_sq_cos_t_sq + a_sq * sin_t_sq)) + c) / cos_t))
  }
  calc_offsets_at_theta <- function(angle_radians, ros_flat=NULL) {
    angle_radians <- angle_radians
    if (is.null(ros_flat)) {
      ros_flat <- calculate_ros(angle_radians)
    }
    if (ros_flat < MIN_ROS) {
      return()
    }
    direction_cw <- raz + angle_radians
    direction_ccw <- raz - angle_radians
    # spread is symmetrical across the center axis, but needs to be adjusted if on a slope
    results <- (rbind(calc_offset(direction_cw, ros_flat * correction_factor(direction_cw)),
                      calc_offset(direction_ccw, ros_flat * correction_factor(direction_ccw))))
    print(results)
    return(results)
  }
  print(sprintf('head_ros = %f', head_ros))
  offsets <- list()
  print(sprintf('added <- calc_offsets_at_theta(%f, %f)', raz, head_ros))
  added <- calc_offsets_at_theta(raz, head_ros)
  print(added)
  if (is.null(added)) {
    print('No spread')
    return()
  }
  offsets <- rbind(offsets, added)
  STEP <- 10
  i <- STEP;
  while (!is.null(added) && i < 90) {
    print(sprintf('added for %d', i - STEP))
    added <- calc_offsets_at_theta(deg2rad(i))
    offsets <- rbind(offsets, added)
    i <- i + STEP
  }
  if (!is.null(added)) {
    print(sprintf('added for %d', i - STEP))
    added <- calc_offsets_at_theta(deg2rad(90), flank_ros * sqrt(a_sq_sub_c_sq) / a)
    offsets <- rbind(offsets, added)
    i <- 90 + STEP
    while (!is.null(added) && i < 180) {
      print(sprintf('added for %d', i - STEP))
      added <- calc_offsets_at_theta(deg2rad(i))
      offsets <- rbind(offsets, added)
      i <- i + STEP
    }
    if (!is.null(added)) {
      print(sprintf('added for %d', i - STEP))
      # only use back ros if every other angle is spreading since this should be lowest
      if (back_ros >= MIN_ROS) {
        print(sprintf('added for %d', 180))
        direction <- deg2rad(RAD_180 + raz)
        offsets <- rbind(offsets, calc_offset(direction, back_ros * correction_factor(direction)))
      }
    }
  }
  return(offsets)
}

reconcile_burnt <- function(sim_env) {
  pts <- sim_env$points
  if (!is.null(pts)) {
    print(sprintf('Marking cells as burnt based on %d points', nrow(pts)))
    cells_new <- getCells(sim_env$landscape, pts)
    cell_ids <- unique(cells_new$CELL)
    print(sprintf('Expecting at least %d burnt cells', nrow(cell_ids)))
    values(sim_env$landscape$burnt)[cell_ids] <- 1
    print(sprintf('Now have %d burnt cells', sum(values(sim_env$landscape$burnt))))
    values(sim_env$landscape$unburnable)[cell_ids] <- 1
    n <- as.data.table(adjacent(sim_env$landscape$unburnable, cells=cell_ids, pairs=TRUE, directions=8))
    n <- cbind(n, as.data.table(list(unburnable=values(sim_env$landscape$unburnable)[n[, to]])))
    num_adjacent <- as.data.table(n %>% group_by(from) %>% summarize(n=sum(unburnable)))
    surrounded_ids <- num_adjacent[n == 8, from]
    # for any cell that is surrounded by unburnable cells, remove points within
    out_pt_ids <- cells_new[CELL %in% surrounded_ids, ID]
    print(sprintf('Removing %d points in surrounded cells', length(out_pt_ids)))
    keep_pts <- pts[setdiff(1:nrow(pts), out_pt_ids), ]
    print(sprintf('There are %d points remaining', nrow(keep_pts)))
    sim_env$points <- keep_pts
  } else {
    print('No points after condense')
  }
  return(sim_env)
}

spread <- function(sim_env, wx) {
  print('spread()')
  print(sim_env)
  print(wx)
  if (is.null(sim_env$points)) {
    print("No points to spread")
    return()
  }
  points_new <- NULL
  landscape <- sim_env$landscape
  cell_size <- res(landscape)
  pt_cells <- list()
  cell_fbps <- list()
  pt_cells <- getCells(landscape, sim_env$points)
  print('>>>> pt_cells')
  print(pt_cells)
  print('<<<< pt_cells')
  if (is.null(pt_cells)) {
    print('*************** Could not get cells ***************')
    return()
  }
  print('>>>> cell_fbps')
  cell_fbps <- calcFBP(pt_cells, wx, sim_env$time, sim_env$lat, sim_env$lon)
  print('<<<< cell_fbps')
  if (is.null(cell_fbps)) {
    print('Could not calculate FBP')
    return()
  }
  print(cell_fbps)
  # group points by cell
  stopifnot(nrow(unique(pt_cells$CELL)) == nrow(cell_fbps))
  make_points_by_cell <- function(for_cells) {
    pts_by_cell <- list()
    for (cell_id in unique(for_cells$CELL)) {
      ids <- for_cells[CELL == cell_id, ID]
      # HACK: use as.character so it doesn't populate all the unused numerical indices with NULL
      pts_by_cell[[as.character(cell_id)]] <- as.vector(ids)
    }
    return(pts_by_cell)
  }
  pts_by_cell <- make_points_by_cell(pt_cells)
  # don't go longer that it takes to go half a cell
  duration = 0.5 * cell_size / max(cell_fbps$ROS, na.rm=TRUE)
  max_time <- floor_date(sim_env$time + hours(1), unit='hour')
  time_until_hour <- as.integer(difftime(max_time, sim_env$time, units='secs')) / 60
  duration <- min(duration, time_until_hour)
  print(duration)
  stopifnot(!is.na(duration))
  for (cell_id in unique(pt_cells$CELL)) {
    fbp <- cell_fbps[CELL == as.integer(cell_id),]
    # I think this only happens on edge of raster?
    # if (!(is.na(fbp$SLOPE) || is.na(fbp$ASPECT))) {
    if (!any(is.na(fbp))) {
      print('Calculating offsets')
      offsets <- calc_offsets(fbp)
      print(offsets)
      if (!is.null(offsets)) {
        # HACK: forget about smouldering in place for now - if no spread then it goes out
        HACK_DIST <- 1
        dists <- list(X=unlist(offsets[,1]) * duration * HACK_DIST, Y=unlist(offsets[,2]) * duration * HACK_DIST)
        print(dists)
        print('Getting xy')
        for (i in pts_by_cell[[as.character(cell_id)]]) {
          pt <- sim_env$points[i,]
          xy <- st_coordinates(pt)
          # print('Applying offsets')
          xy_with_offsets <- list(X=xy[1] + dists$X, Y=xy[2] + dists$Y)
          # print(xy_with_offsets)
          xy_pts <- st_as_sf(data.frame(xy_with_offsets), coords=c('X', 'Y'), crs=st_crs(landscape))
          points_new <- rbind(points_new, xy_pts)
        }
      }
    }
  }
  print(sprintf('nrow(points_new) == %d)', nrow(points_new)))
  print(points_new)
  condense_points <- function(points_new) {
    if (is.null(points_new)) {
      return()
    }
    # now we want to aggregate the points by cell
    # (m)
    HULL_DIST <- 10
    # hulled_pts <- NULL
    cells_new <- getCells(landscape, points_new)
    # throw out points that aren't in cells
    cells_new <- cells_new[!is.nan(CELL),]
    # throw out non-fuel points
    cells_new <- cells_new[!(FUELTYPE %in% BASE_DATA$NON_FUELS),]
    # mark all cells that have points as burnt
    points_new <- points_new[cells_new$ID,]
    points_new$CELL <- cells_new$CELL
    by_cell <- points_new %>%
      group_by(CELL) %>%
      summarise(geometry=st_combine(geometry)) %>%
      st_convex_hull() %>%
      st_simplify(preserveTopology=TRUE, dTolerance=HULL_DIST)
    by_cell$CELL <- NULL
    # not sure why this keeps making different geometries, but try this
    points <- NULL
    for (i in 1:nrow(by_cell)) {
      points <- rbind(points, st_cast(by_cell[i,], 'POINT'))
    }
    # # how does this happen and what does it mean?
    # linestrings <- by_cell[lapply(by_cell$geometry, function(s) { class(s)[[2]] }) == 'LINESTRING', ]
    # points <- by_cell[lapply(by_cell$geometry, function(s) { class(s)[[2]] }) == 'POINT', ]
    # points <- rbind(points, st_cast(linestrings, 'POINT'))
    hulled_pts <- st_as_sf(as.data.frame(points))
    st_crs(hulled_pts) <- st_crs(points_new)
    return(hulled_pts)
  }
  points_new <- condense_points(points_new)
  print('Incrementing time')
  # HACK: add a seconds so it should be enough to get into next hour if close
  sim_env$time <- sim_env$time + seconds(as.integer(duration * 60) + 1)
  print(sim_env$time)
  sim_env$points <- points_new
  sim_env <- reconcile_burnt(sim_env)
  return(sim_env)
}
start_fire <- function(sim_env, lat, lon, time) {
  print("***************** start_fire() *****************")
  print(sim_env)
  # HACK: use same lat/lon for fbp calculations for now
  sim_env$lat <- lat
  sim_env$lon <- lon
  sim_env$points <- get_point(sim_env$landscape, lat, lon)
  print("sim_env after fire started:")
  print(sim_env)
  stopifnot(!is.null(sim_env$points))
  sim_env$time <- time
  print(sprintf('Starting fire at (%f, %f) at time %s', lat, lon, time))
  values(sim_env$landscape$burnt) <- 0
  cell <- getCells(sim_env$landscape, sim_env$points)
  values(sim_env$landscape$burnt)[unique(cell$CELL)] <- 1
  sim_env$started <- list(lat=lat, lon=lon, time=time)
  return(sim_env)
}
# start_fire(landscape, lat, lon, time)
