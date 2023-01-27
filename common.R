library('terra')
library('sf')
library('sp')
library('dplyr')
library('leaflet')
library('XML')
library('data.table')

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


ensure_data <- function(dir_data='./data_input/') {
  dir_download <- paste0(dir_data, 'download/')
  dir_extracted <- paste0(dir_data, 'extracted/')
  dir_created <- './data/'
  # canada = 'https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip'
  canada <- 'http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip'
  fbp <- 'https://cwfis.cfs.nrcan.gc.ca/downloads/fuels/development/Canadian_Forest_FBP_Fuel_Types/Canadian_Forest_FBP_Fuel_Types_v20191114.zip'
  dir.create(dir_download, showWarnings=FALSE, recursive=TRUE)
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
  fct_colours <- function(x) {
    r <- list()
    r[as.character(xmlGetAttr(x, 'value'))] <- xmlGetAttr(x, 'color')
    return(r)
  }
  fbp_pairs <- unlist(lapply(fbp_colours, fct_colours))
  fct_palette <- function(v) { fbp_pairs[as.character(v)] }
  return(list(SHP_CANADA=shp_canada,
              TIF_FBP=proj_fbp,
              TIF_FBP_AGG=tif_fbp_agg,
              COLOURS_FBP=fct_palette))
}

check_in_bounds <- function(r, lat, lon) {
  return(tryCatch({
    print(sprintf('lat: %s, lon: %s', lat, lon))
    pt <- st_as_sf(data.frame(latitude=lat, longitude=lon), coords=c('longitude', 'latitude'), crs='WGS84')
    print(pt)
    pt_proj <- st_transform(pt, crs(r))
    return(!is.na(extract(r, pt_proj)[names(r)[[1]]]))
  },
  error=function(e) { FALSE }))
}
