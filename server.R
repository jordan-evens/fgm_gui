source('common.R')
source('weather.R')
library('DT')
library('shinyvalidate')

NUM_CELLS <- 200
NUM_DATATABLE_ROWS <- 10

server <- function(input, output, session) {
  data <- ensure_data()
  lat_in_bounds <- function(lat) { return(check_in_bounds(data$TIF_FBP, lat, as.numeric(input$longitude))) }
  lon_in_bounds <- function(lon) { return(check_in_bounds(data$TIF_FBP, as.numeric(input$latitude), lon)) }
  shp_canada <- data$SHP_CANADA
  tif_fbp <- data$TIF_FBP_AGG
  tif_fbp <- raster::raster(tif_fbp)
  colours_fbp <- data$COLOURS_FBP
  m <- minmax(data$TIF_FBP)
  bbox <- as.vector(st_bbox(shp_canada))
  output$map <- renderLeaflet({
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
    if (is.null(event)) {
      return()
    }
    lat <- as.numeric(event$lat)
    lon <- as.numeric(event$lng)
    if ((!is.null(session$userData$latitude) &&
         !is.null(session$userData$longitude) &&
         (isTRUE(all.equal(session$userData$latitude, lat)) && isTRUE(all.equal(session$userData$longitude, lon))))) {
      return()
    }
    if (!check_in_bounds(data$TIF_FBP, lat, lon)) {
      return()
    }
    pt <- st_as_sf(data.frame(latitude=lat, longitude=lon), coords=c('longitude', 'latitude'), crs='WGS84')
    fbp_orig <- data$TIF_FBP
    pt_proj <- st_transform(pt, crs(fbp_orig))
    b <- st_bbox(pt_proj)
    b_orig <- st_bbox(fbp_orig)
    dist <- NUM_CELLS * ((b_orig$xmax - b_orig$xmin) / ncol(fbp_orig))
    box <- ext(c(b$xmin - dist / 2, b$xmax + dist / 2, b$ymin - dist / 2, b$ymax + dist / 2))
    clipped <- tryCatch(crop(fbp_orig, box), error=function(e) { NULL })
    if (is.null(clipped)) {
      return()
    }
    band <- names(clipped)[[1]]
    icon_origin <- makeIcon(iconUrl=pchIcons(13, 40, 40, col="black", lwd = 2)[[1]],
                            iconAnchorX=20,
                            iconAnchorY=20)
    if (!all(is.nan(minmax(clipped[[band]])))) {
      session$userData$pt_originj <- pt
      session$userData$pt_origin_proj <- pt_proj
      session$userData$clipped <- clipped
      session$userData$latitude <- lat
      session$userData$longitude <- lon
      wx <- get_weather(lat, lon)
      session$userData$wx <- wx
      weather <- copy(wx)
      col_precision <- list(LAT=3, LONG=3)
      for (col in names(weather)) {
        if (is.numeric(weather[[col]])) {
          precision <- ifelse(col %in% names(col_precision), col_precision[[col]], 1)
          weather[[col]] <- round(weather[[col]], precision)
        }
      }
      num_pages <- nrow(weather) / NUM_DATATABLE_ROWS
      session$userData$num_pages <- num_pages
      updateSelectInput(inputId='page', choices=(1:num_pages), selected=1)
      # HACK: can't figure out how to use format string in renderDT
      weather$DATETIME <- format(weather$DATETIME, '%Y-%m-%d %H:%M %Z')
      output$weather <- DT::renderDT(
        weather,
        callback = JS(c(
          # HACK: doesn't seem to work if done right away
          "setTimeout(function() { table.columns.adjust().draw('page'); }, 10);",
          "$('#page').on('change', function(){",
          "  table.page(parseInt($('#page').val()) - 1).draw('page');",
          "});"
        )),
        options=list(
          dom='t',
          autoWidth=TRUE,
          columnDefs=list(list(targets=0, width='9.5em')),
          scrollX=TRUE,
          pageLength=NUM_DATATABLE_ROWS
        ),
        server=FALSE,
        rownames=FALSE
      )
      updateTextInput(session, 'latitude', value=lat)
      updateTextInput(session, 'longitude', value=lon)
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
  
  fakeClick <- function() {
    if (is.na(suppressWarnings(as.numeric(input$latitude))) ||
        is.na(suppressWarnings(as.numeric(input$longitude)))) {
      return()
    }
    event <- list(id=NULL, lat=as.numeric(input$latitude), lng=as.numeric(input$longitude))
    if (!(event$lat <= 90 && event$lat >= -90 && event$lng >= -360 && event$lng <= 360)) {
      return()
    }
    handleClick(event)
  }
  
  observeEvent(input$latitude, { fakeClick() })
  observeEvent(input$longitude, { fakeClick() })
  session$userData$latitude <- NULL
  session$userData$longitude <- NULL
  
  iv <- InputValidator$new()
  iv$add_rule('latitude',
              compose_rules(
                sv_required(),
                sv_numeric(),
                sv_between(-90, 90),
                ~ if (!lat_in_bounds(.)) 'Coordinates must be within FBP raster'
              )
  )
  iv$add_rule('longitude',
              compose_rules(
                sv_required(),
                sv_numeric(),
                sv_between(-360, 360),
                ~ if (!lon_in_bounds(.)) 'Coordinates must be within FBP raster'
              )
  )
  iv$enable()
  
  observeEvent(input$prev_page, {
    updateSelectInput(session, 'page', selected=as.numeric(input$page) - 1)
  })
  observeEvent(input$next_page, {
    updateSelectInput(session, 'page', selected=as.numeric(input$page) + 1)
  })
  observeEvent(input$page, {
    page <- tryCatch(as.numeric(input$page), error=function(e) { NULL })
    shinyjs::toggleState('prev_page', (!is.null(page) && page > 1))
    shinyjs::toggleState('next_page', (!is.null(page) && page < session$userData$num_pages))
    shinyjs::delay(100, { shinyjs::show('div_info') })
  })
}
