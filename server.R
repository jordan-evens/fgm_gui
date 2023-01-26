source('common.R')
library('lutz')
library('DT')

NUM_CELLS <- 200


server <- function(input, output, session) {
    data <- ensure_data()
    shp_canada <- data$SHP_CANADA
    tif_fbp <- data$TIF_FBP_AGG
    tif_fbp <- raster::raster(tif_fbp)
    colours_fbp <- data$COLOURS_FBP
    m <- minmax(data$TIF_FBP)
    # HACK: don't figure out how to subset a data.frame right now
    df_colours <- data.table(data.frame(c(value=list(m[1]:m[2]), color=lapply(list(m[1]:m[2]), data$COLOURS_FBP))))
    # df_colours <- df_colours[!is.na(color),]
    df_colours[is.na(color),]$color <- 'magenta'
    df_colours <- data.frame(df_colours)
    output$map <- renderLeaflet({
    bbox <- as.vector(st_bbox(shp_canada))
    # # NOTE: apparently leaflet addRasterImage() only works with EPSG:3857
    m <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       group="Default Maptile",
                       options = providerTileOptions(noWrap=TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group="Satellite Maptile") %>%
      addRasterImage(x=tif_fbp,
                     project=FALSE,
                     colors=colours_fbp,
                     opacity=0.5,
                     layerId='FBP',
                     group='FBP') %>%
      addLayersControl(
        baseGroups = c("Default Maptile", "Satellite Maptile"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addPolygons(data=shp_canada,
                  col="black",
                  weight = 1,
                  fill=FALSE,
                  label=shp_canada$PREABBR) %>%
    # fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      setView((bbox[1] + bbox[3]) / 2, (bbox[2] + bbox[4]) / 2, zoom=3)
  })
  handleClick <- function(event) {
    # event <- list(lat=48.67645, lng=-88.6908)
    leafletProxy("map") %>% clearPopups()
    # print(event)
    if (is.null(event)) {
      return()
    }
    # print('Handling click')
    lat <- event$lat
    lon <- event$lng
    pt <- st_as_sf(data.frame(latitude=lat, longitude=lon), coords=c('longitude', 'latitude'), crs='WGS84')
    fbp_orig <- data$TIF_FBP
    pt_proj <- st_transform(pt, crs(fbp_orig))
    b <- st_bbox(pt_proj)
    b_orig <- st_bbox(fbp_orig)
    dist <- NUM_CELLS * ((b_orig$xmax - b_orig$xmin) / ncol(fbp_orig))
    box <- ext(c(b$xmin - dist / 2, b$xmax + dist / 2, b$ymin - dist / 2, b$ymax + dist / 2))
    # clipped <- crop(fbp_orig, box, mask=TRUE)
    clipped <- crop(fbp_orig, box)
    # HACK: for some reason it's indexing on the row index of the unique values and not the actual value
    df <- df_colours[df_colours$value %in% as.vector(unique(clipped)[[1]]),]
    band <- names(clipped)[[1]]
    # names(clipped) <- 'value'
    isolate({
      print(input$map_bounds)
      print(event$id)
      print(event$lat)
      print(event$lng)
    })
    session$userData$pt_originj <- pt
    session$userData$pt_origin_proj <- pt_proj
    session$userData$clipped <- clipped
    icon_origin <- makeIcon(iconUrl=pchIcons(13, 40, 40, col="black", lwd = 2)[[1]],
                            iconAnchorX=20,
                            iconAnchorY=20)
    # print(clipped)
    if (!all(is.nan(minmax(clipped[[band]])))) {
      shinyjs::show('div_map_zoom')
      bbox <- as.vector(st_bbox(st_transform(st_as_sf(as.polygons(ext(clipped), crs=crs(clipped))), crs(pt))))
      output$map_zoom <- renderLeaflet({
        m <- leaflet() %>%
          addRasterImage(x=clipped,
                         project=FALSE,
                         colors=colours_fbp,
                         opacity=0.5,
                         layerId='FBP',
                         group='FBP') %>%
          fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
          addMarkers(data=pt,
                     layerId='origin',
                     icon=icon_origin)
      })
      leafletProxy("map") %>%
        addMarkers(data=pt,
                   layerId='origin',
                   icon=icon_origin)
    }
    wx <- data.table(read.csv('./cffdrs-ng/test_hffmc.csv'))
    wx$lat <- lat
    wx$long <- lon
    timezone <- lutz::tz_lookup_coords(lat, lon)
    init <- wx[1,]
    date_start <- make_date(init$yr, init$mon, init$day)
    tz <- tz_offset(date_start, timezone)$utc_offset_h
    wx <- hFWI(wx, tz)
    weather <- copy(wx)
    col_precision <- list(lat=3, long=3)
    for (col in names(weather)) {
      if (is.numeric(weather[[col]])) {
        precision <- ifelse(col %in% names(col_precision), col_precision[[col]], 1)
        weather[[col]] <- round(weather[[col]], precision)
      }
    }
    output$weather <- DT::renderDT(weather,
                                   options=list(dom='t', scrollX=TRUE),
                                   rownames=FALSE)
    return(event)
  }
  observe({
    event <- input$map_click
    handleClick(event)
  })
  observe({
    event <- input$map_zoom_click
    handleClick(event)
  })
}
