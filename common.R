library('terra')
library('sf')
library('sp')
library('dplyr')
library('leaflet')
library('XML')
library('data.table')
# required for elevatr
library('progress')
library('elevatr')

# from example at https://github.com/rstudio/leaflet/blob/main/inst/examples/icons.R#L25-L40
# use point symbols from base R graphics as icons
pchIcons <- function(pch = 0:14, width = 30, height = 30, ...) {
  n <- length(pch)
  files <- character(n)
  # create a sequence of png images
  for (i in seq_len(n)) {
    f <- tempfile(fileext = ".png")
    png(f, width = width, height = height, bg = "transparent")
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = pch[i], cex = min(width, height) / 8, ...)
    dev.off()
    files[i] <- f
  }
  files
}

# as per https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution
ground_resolution <- function(latitude, z) {
  (cos(latitude * pi / 180) * 2 * pi * 6378137) / (256 * 2 ** z)
}

determine_zoom <- function(r) {
  center <- crds(r)[nrow(r) * ncol(r) / 2,]
  pt <- st_as_sf(data.frame(x=center[1], y=center[2]), coords=c('x', 'y'), crs=crs(r))
  pt_proj <- st_transform(pt, 'WGS84')
  xy <- st_coordinates(pt_proj)
  latitude <- xy[2]
  longitude <- xy[1]
  target <- min(res(r))
  resolutions <- ground_resolution(latitude, 0:15)
  # find ratios between r's resolution and zoom level pixels
  ratios <- resolutions / target
  # HACK: pick a the zoom level closest to r's resolution
  z <- which(as.integer(ratios) == 0)[[1]]
  # -1 because zoom is 0 indexed and lists are 1 indexed
  return(z - 1)
}

get_elevation <- function(r) {
  stopifnot(inherits(r, 'SpatRaster'))
  locations <- raster::raster(r)
  # # zoom in a couple levels level so hopefully resample is closer to being right
  # z <- determine_zoom(r) + 2
  # just make it fast for now
  z <- determine_zoom(r)
  e <- get_elev_raster(locations, z)
  return(resample(e, locations))
}

ensure_data <- function(dir_data='./data_input/') {
  dir_download <- paste0(dir_data, 'download/')
  dir_extracted <- paste0(dir_data, 'extracted/')
  dir_created <- './data/'
  # canada = 'https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip'
  canada <- 'http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip'
  fbp <- 'https://cwfis.cfs.nrcan.gc.ca/downloads/fuels/development/Canadian_Forest_FBP_Fuel_Types/Canadian_Forest_FBP_Fuel_Types_v20191114.zip'
  dir.create(dir_download, showWarnings=FALSE, recursive=TRUE)
  dir.create(dir_extracted, showWarnings=FALSE, recursive=TRUE)
  dir.create(dir_created, showWarnings=FALSE, recursive=TRUE)
  ensure_file <- function(url) {
    f <- paste0(dir_download, basename(url))
    if (!file.exists(f)) {
      download.file(url, f)
    }
    dir_out <- paste0(dir_extracted, tools::file_path_sans_ext(basename(url)))
    if (!dir.exists(dir_out)) {
      unzip(f, exdir=dir_out)
    }
    return(dir_out)
  }
  file_simple_canada <- paste0(dir_created, 'canada_simple.shp')
  if (!file.exists(file_simple_canada)) {
    file_canada <- list.files(ensure_file(canada),
                              pattern='*.shp',
                              full.names=TRUE)[[1]]
    if (!file.exists(file_simple_canada)) {
      shp_canada <- st_read(file_canada) %>%
        sf::st_transform('EPSG:3347')
      flag_orig <- sf_use_s2()
      sf_use_s2(FALSE)
      shp_canada <- st_simplify(shp_canada, dTolerance=1000) %>%
        sf::st_transform('+proj=longlat +datum=WGS84')
      sf_use_s2(flag_orig)
      st_write(shp_canada, file_simple_canada)
    }
  }
  shp_canada <- st_read(file_simple_canada)
  file_proj_fbp <- paste0(dir_created, 'fbp_proj.tif')
  if (!file.exists(file_proj_fbp)) {
    file_fbp <- list.files(ensure_file(fbp),
                           pattern='FBP_FuelLayer.tif',
                           full.names=TRUE,
                           recursive=TRUE)[[1]]
    if (!file.exists(file_proj_fbp)) {
      tif_fbp <- rast(file_fbp)
      # NOTE: super slow - look for another way
      proj_fbp <- projectRasterForLeaflet(tif_fbp, method='ngb')
      writeRaster(proj_fbp, file_proj_fbp, overwrite=TRUE)
    }
  }
  proj_fbp <- rast(file_proj_fbp)
  file_agg_fbp <- paste0(dir_created, 'fbp_agg.tif')
  if (!file.exists(file_agg_fbp)) {
    agg_fbp <- aggregate(proj_fbp, fact=10, fun='modal')
    writeRaster(agg_fbp, file_agg_fbp, overwrite=TRUE)
  }
  tif_fbp_agg <- rast(file_agg_fbp)
  file_style_fbp <- paste0(dir_created, 'fbp_style.xml')
  if (!file.exists(file_style_fbp)) {
    file.copy(list.files(dir_extracted,
                         pattern='QGIS_Burn_p3_style.qml',
                         full.names=TRUE,
                         recursive=TRUE)[[1]],
              file_style_fbp)
  }
  fbp_style <- XML::xmlParse(file_style_fbp)
  fbp_colours <- xmlElementsByTagName(xmlRoot(fbp_style), 'paletteEntry', recursive=TRUE)
  names(fbp_colours) <- NULL
  fct_fbp_lookup <- function (field) {
    fct_colours <- function(x) {
      r <- list()
      r[as.character(xmlGetAttr(x, 'value'))] <- xmlGetAttr(x, field)
      return(r)
    }
    fbp_pairs <- unlist(lapply(fbp_colours, fct_colours))
    return(function(v) { fbp_pairs[as.character(v)] })
  }
  fct_palette <- fct_fbp_lookup('color')
  fct_name <- fct_fbp_lookup('label')
  return(list(SHP_CANADA=shp_canada,
              TIF_FBP=proj_fbp,
              TIF_FBP_AGG=tif_fbp_agg,
              COLOURS_FBP=fct_palette,
              NAMES_FBP=fct_name))
}

check_in_bounds <- function(r, lat, lon) {
  return(tryCatch({
    print(sprintf('lat: %s, lon: %s', lat, lon))
    pt <- st_as_sf(data.frame(latitude=lat, longitude=lon), coords=c('longitude', 'latitude'), crs='WGS84')
    print(pt)
    pt_proj <- st_transform(pt, crs(r))
    return(!isTRUE(is.na(extract(r, pt_proj)[names(r)[[1]]])))
  },
  error=function(e) { FALSE }))
}

VERSION_HASH <- NULL
get_version <- function() {
  if (!is.null(VERSION_HASH)) {
    return(VERSION_HASH)
  }
  return(tryCatch({
    # # HACK: call this directly because importing it seems to cause errors
    # tried using git2r, but doesn't work on shinyapps.io
    # v <- git2r::last_commit()$sha
    # want some kind of hash so we can know if things are the same even if the modified times aren't
    code <- paste(sapply(sort(list.files(pattern='*.R$')), function (f) { paste(readLines(f), collapse='\n') }), collapse='\n')
    v <- digest::digest(code, algo='md5')
    v <- paste0('v_', substr(v, 1, 7))
    # # check if files are modified and indicate if so
    # if (0 < sum(sapply(git2r::status(), length))) {
    #   v <- paste0(v, '+')
    # }
    # # just do last modified time
    # v <- paste0('Last modified ', max(file.info(list.files())$mtime))
    VERSION_HASH <<- v
    return(v)
  }, error=function (e) { '' }))
}
