library('terra')
library('sf')
library('dplyr')
library('leaflet')
library('XML')
# requireNamespace('rgdal')

ensure_data <- function(dir_data='./data/') {
  dir_download <- paste0(dir_data, 'download/')
  dir_extracted <- paste0(dir_data, 'extracted/')
  dir_created <- paste0(dir_data, 'created/')
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
  file_canada <- list.files(ensure_file(canada),
                            pattern='*.shp',
                            full.names=TRUE)[[1]]
  file_simple_canada <- paste0(dir_created, 'canada_simple.shp')
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
  shp_canada <- st_read(file_simple_canada)
  file_fbp <- list.files(ensure_file(fbp),
                         pattern='FBP_FuelLayer.tif',
                         full.names=TRUE,
                         recursive=TRUE)[[1]]
  file_proj_fbp <- paste0(dir_created, 'fbp_proj.tif')
  if (!file.exists(file_proj_fbp)) {
    tif_fbp <- rast(file_fbp)
    # NOTE: super slow - look for another way
    proj_fbp <- projectRasterForLeaflet(tif_fbp, method='ngb')
    writeRaster(proj_fbp, file_proj_fbp, overwrite=TRUE)
  }
  proj_fbp <- rast(file_proj_fbp)
  file_agg_fbp <- paste0(dir_created, 'fbp_agg.tif')
  if (!file.exists(file_agg_fbp)) {
    agg_fbp <- aggregate(proj_fbp, fact=10, fun='modal')
    writeRaster(agg_fbp, file_agg_fbp, overwrite=TRUE)
  }
  tif_fbp_agg <- rast(file_agg_fbp)
  fbp_style <- XML::xmlParse(list.files(dir_extracted,
                                        pattern='QGIS_Burn_p3_style.qml',
                                        full.names=TRUE,
                                        recursive=TRUE)[[1]])
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


server <- function(input, output, session) {
  data <- ensure_data()
  shp_canada <- data$SHP_CANADA
  tif_fbp <- data$TIF_FBP_AGG
  tif_fbp <- raster::raster(tif_fbp)
  output$Map <- renderLeaflet({
    bbox <- as.vector(st_bbox(shp_canada))
    colours_fbp <- data$COLOURS_FBP
    # # NOTE: apparently leaflet addRasterImage() only works with EPSG:3857
    m <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       group="Default Maptile",
                       options = providerTileOptions(noWrap=TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group="Satellite Maptile") %>%
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
      addRasterImage(x=tif_fbp,
                     project=FALSE,
                     colors=colours_fbp,
                     opacity=0.5,
                     group='FBP') %>%
      addLayersControl(
        baseGroups = c("Default Maptile", "Satellite Maptile"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addPolygons(data=shp_canada,
                  col="black",
                  weight = 1,
                  fill=FALSE,
                  label=shp_canada$PREABBR)
  })
}
