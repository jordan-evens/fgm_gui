source('common.R')

NUM_CELLS <- 200


server <- function(input, output, session) {
    data <- ensure_data()
    shp_canada <- data$SHP_CANADA
    tif_fbp <- data$TIF_FBP_AGG
    tif_fbp <- raster::raster(tif_fbp)
    m <- minmax(data$TIF_FBP)
    # HACK: don't figure out how to subset a data.frame right now
    df_colours <- data.table(data.frame(c(value=list(m[1]:m[2]), color=lapply(list(m[1]:m[2]), data$COLOURS_FBP))))
    # df_colours <- df_colours[!is.na(color),]
    df_colours[is.na(color),]$color <- 'magenta'
    df_colours <- data.frame(df_colours)
    output$map <- renderLeaflet({
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
                  label=shp_canada$PREABBR)
  })
  observe({
    # event <- list(lat=48.67645, lng=-88.6908)
    leafletProxy("map") %>% clearPopups()
    event <- input$map_click
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
      print(event$id)
      print(event$lat)
      print(event$lng)
    })
    session$userData$pt_originj <- pt
    session$userData$pt_origin_proj <- pt_proj
    session$userData$clipped <- clipped
    # print(clipped)
    if (!all(is.nan(minmax(clipped[[band]])))) {
      shinyjs::show('div_map_zoom')
      output$map_zoom <- renderPlot({
        plot(clipped, band, col=df, type='classes')
        plot(pt_proj, pch=13, cex=4, col='black', lwd=1.5, add=TRUE)
      })
      leafletProxy("map") %>%
        addMarkers(data=pt,
                   layerId='origin',
                   icon=makeIcon(iconUrl=pchIcons(13, 40, 40, col="black", lwd = 2)[[1]],
                                 iconAnchorX=20,
                                 iconAnchorY=20))
              
    }
    return(event)
  })
}
