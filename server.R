source('common.R')
source('weather.R')
source('fbp.R')
library('DT')
library('shinyvalidate')

NUM_DATATABLE_ROWS <- 10

server <- function(input, output, session) {
  makeRamp <- function(colours, value_min, value_max, breaks) {
    bins <- (value_max - value_min) / breaks
    return(colorBin(colorRampPalette(colours)(bins), domain=c(value_min, value_max), bins=bins))
  }
  # Just use world values (m) for now
  COLOURS_ELEV <- makeRamp(c('white', 'black'), -500, 9000, 100)
  COLOURS_SLOPE <- makeRamp(c('white', 'red'), 0, 300, 10)
  COLOURS_ASPECT <- makeRamp(c('white', 'red', 'black', 'blue', 'white'), 0, 360, 10)
  updateSimulationTimeSlider <- function(value=NULL, wx=NULL) {
    if (is.na(tryCatch(as.numeric(input$duration), error={ NA }))) {
      return()
    }
    if (is.null(wx)) {
      wx <- session$userData$wx
      if (is.null(wx)) {
        return()
      }
    }
    if (is.null(value)) {
      value <- as_datetime(input$startTime, tz=tz(wx$DATETIME))
      if (is.null(value)) {
        return()
      }
    }
    value_min <- min(wx$DATETIME)
    value_max <- max(wx$DATETIME) - hours(ceiling(input$duration / 60))
    value <- max(min(value, value_max), value_min)
    timezone <- timezone <- tz_offset(value, tz(value))$utc_offset_h * 60
    updateSliderInput(session=session,
                      'startTime',
                      value=value,
                      min=value_min,
                      max=value_max,
                      timezone=timezone)
    session$userData$startTime <- value
    updateFBPOriginTable(startTime=value)
  }
  updateFBPOriginTable <- function(startTime=NULL, wx=NULL) {
    print('updateFBPOriginTable()')
    if (is.null(wx)) {
      wx <- session$userData$wx
      if (is.null(wx)) {
        print('No weather so not updating fbp')
        return()
      }
    }
    if (is.null(startTime)) {
      # startTime <- as_datetime(input$startTime, tz=tz(wx$DATETIME))
      startTime <- session$userData$startTime
      if (is.null(startTime)) {
        print('No startTime, so not updating fbp')
        return()
      }
    }
    # lat <- as.numeric(input$latitude)
    # lon <- as.numeric(input$longitude)
    lat <- session$userData$latitude
    lon <- session$userData$longitude
    landscape <- session$userData$landscape
    if (is.null(lat) || is.null(lon) || is.null(landscape)) {
      print('No lat/long or landscape, so not updating fbp')
      return()
    }
    print('Getting point')
    pt <- get_point(landscape, lat, lon)
    if (is.null(pt)) {
      print('Could not get point, so not  updating fbp')
      return()
    }
    print('Getting cell')
    cell <- getCells(landscape, pt)
    if (is.null(cell)) {
      print('Could not get cell, so not  updating fbp')
      return()
    }
    print('Calculating fbp')
    df <- calcFBP(cell, wx, startTime, lat, lon)
    print(df)
    if (is.null(df)) {
      return()
    }
    col_precision <- list(LAT=3, LONG=3)
    for (col in names(df)) {
      if (is.numeric(df[[col]])) {
        precision <- ifelse(col %in% names(col_precision), col_precision[[col]], 1)
        df[[col]] <- round(df[[col]], precision)
      }
    }
    # HACK: can't figure out how to use format string in renderDT
    df$DATETIME <- format(df$DATETIME, '%Y-%m-%d %H:%M %Z')
    START_COLS <- c('DATETIME', 'FUELTYPE', 'SLOPE', 'ASPECT')
    cols <- c(START_COLS, setdiff(names(df), START_COLS))
    df <- df[, ..cols]
    output$fbp_origin <- DT::renderDT(
      df,
      options=list(
        dom='t',
        autoWidth=TRUE,
        columnDefs=list(list(targets=0, width='9.5em')),
        scrollX=TRUE,
        paging=FALSE
      ),
      server=FALSE,
      rownames=FALSE
    )
  }
  updatePoints <- function(pts) {
    print(sprintf('Called updatePoints() with %d points', nrow(pts)))
    # print(pts)
    if (!is.null(pts)) {
      # print('st_transform()')
      pts_proj <- st_transform(pts, PROJ_DEFAULT)
      # print(pts_proj)
      # pts_sp <- as(pts_proj, 'Spatial')
      # pts_map <- pts_sp
      # pts_map <- st_combine(pts_proj)
      pts_map <- pts_proj
      # pts_map <- as.data.table(as.data.frame(st_coordinates(pts_proj)))
      # print(pts_map)
      # pts_map <- pts_map[!is.na(X),]
      # if (is.null(pts_map)) {
      #   return()
      # }
      # names(pts_map) <- c('Longitude', 'Latitude')
      # pts_map$ID <- 1:nrow(pts_map)
      # # is data.table breaking it?
      # pts_map <- as.data.frame(pts_map)
      # # HACK: seems like it only draw one point if using 'data=pts_proj'
      # xy <- st_coordinates(pts_proj)
      add_points <- function(map_id) {
        # print(sprintf('add to %s', map_id))
        m <- leafletProxy(map_id) %>%
          # removeMarker('burning') %>%
          clearGroup('active') %>%
          # # addMarkers(
          addCircles(
            data=pts_map,
            color='red',
            radius=10,
            weight=10,
            opacity=1,
            fillOpacity=1,
            # lng=pts_map$Longitude,
            # lat=pts_map$Latitude,
            # data=pts_map,
            # pts_map
            # lng=as.vector(pts_map$X),
            # lat=as.vector(pts_map$Y),
            # data=pts_map,
            # lng=~Longitude,
            # lat=~Latitude,
            # lng=xy[,1],
            # lat=xy[,2],
            # icon=icon_burning,
            group='active')
        # clearMarkers()
        # for (i in 1:nrow(pts_map)) {
        #   addCircles(m,
        #              lng=pts_map$Longitude[i],
        #              lat=pts_map$Latitude[i],
        #              color='red',
        #              radius=10,
        #              weight=10,
        #              opacity=1,
        #              fillOpacity=1,
        #              layerId=sprintf('burning%03d', i))
        # }
      }
      add_points('map')
      add_points('map_zoom')
      # print('Updating simTime')
      updateTextInput(session, 'simTime', value=CUR_TIME)
      output$points <- DT::renderDT(
        as.data.frame(st_coordinates(pts_proj)),
        options=list(
          dom='t',
          # pageLength=nrow(pts_proj),
          autoWidth=TRUE,
          scrollX=TRUE,
          # HACK: maybe this works?
          scrollY=TRUE,
          paging=FALSE
        ),
        server=FALSE,
        rownames=FALSE
      )
    }
    # print('Done updatePoints()')
    # print(class(pts))
  }
  updateWeather <- function(wx) {
    if (is.null(wx)) {
      return()
    }
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
    print('Got weather')
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
    print('Rendered weather')
  }
  lat_in_bounds <- function(lat) { return(check_in_bounds(BASE_DATA$TIF_FBP, lat, as.numeric(input$longitude))) }
  lon_in_bounds <- function(lon) { return(check_in_bounds(BASE_DATA$TIF_FBP, as.numeric(input$latitude), lon)) }
  icon_origin <- makeIcon(iconUrl=pchIcons(13, 40, 40, col="black", lwd = 2)[[1]],
                          iconAnchorX=20,
                          iconAnchorY=20)
  icon_burning <- makeIcon(iconUrl=pchIcons(16, 10, 10, col="red", lwd = 2)[[1]],
                          iconAnchorX=5,
                          iconAnchorY=5)
  shp_canada <- BASE_DATA$SHP_CANADA
  tif_fbp <- BASE_DATA$TIF_FBP_AGG
  tif_fbp <- raster::raster(tif_fbp)
  colours_fbp <- BASE_DATA$COLOURS_FBP
  m <- minmax(BASE_DATA$TIF_FBP)
  bbox <- as.vector(st_bbox(shp_canada))
  output$map <- renderLeaflet({
    df <- data.frame(lng=-100:-80, lat=45:65)
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
        baseGroups=c('Default Maptile', 'Satellite Maptile'),
        overlayGroups=c('FBP'),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addPolygons(data=shp_canada,
                  col="black",
                  weight = 1,
                  fill=FALSE,
                  label=shp_canada$PREABBR) %>%
      # addCircles(
      #   color='red',
      #   radius=10,
      #   weight=10,
      #   opacity=1,
      #   fillOpacity=1,
      #   data=df,
      #   lng=~lng,
      #   lat=~lat,
      #   group='burning') %>%
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
    # # HACK: convert to raster crs and back so that we round to the same precision
    # # (trying to get raster to pick correct cell for position)
    # latlong_rounded <- round_pt_precision(lat, lon)
    # # # is this even doing anything?
    # # stopifnot(lon == as.double(latlong_rounded$lon))
    # # stopifnot(lat == as.double(latlong_rounded$lat))
    # lon <- as.double(latlong_rounded$lon)
    # lat <- as.double(latlong_rounded$lat)
    landscape_all <- createLandscape(lat, lon)
    session$userData$latitude <- lat
    session$userData$longitude <- lon
    wx <- get_weather(lat, lon)
    session$userData$wx <- wx
    updateWeather(wx)
    print('Got landscape')
    print(landscape_all)
    print('Checking landscape')
    if (!is.null(landscape_all) && !is.null(landscape_all$data)) {
      pt <- landscape_all$origin
      landscape <- landscape_all$data
      session$userData$landscape <- landscape
      updateTextInput(session, 'latitude', value=lat)
      updateTextInput(session, 'longitude', value=lon)
      shinyjs::show('div_map_zoom')
      bbox <- as.vector(st_bbox(st_transform(st_as_sf(as.polygons(ext(landscape), crs=as.character(crs(landscape)))), PROJ_DEFAULT)))
      print('Got bbox')
      print(landscape)
      print(landscape$elevation)
      print(landscape$slope)
      print(landscape$aspect)
      print(landscape$fueltype)
      print(bbox)
      print(pt)
      print('Drawing zoomed map')
      output$map_zoom <- renderLeaflet({
        m <- leaflet() %>%
          addRasterImage(x=landscape$elevation,
                         project=FALSE,
                         colors=COLOURS_ELEV,
                         opacity=1,
                         layerId='Elevation',
                         group='Elevation') %>%
          addRasterImage(x=landscape$slope,
                         project=FALSE,
                         colors=COLOURS_SLOPE,
                         opacity=1,
                         layerId='Slope',
                         group='Slope') %>%
          addRasterImage(x=landscape$aspect,
                         project=FALSE,
                         colors=COLOURS_ASPECT,
                         opacity=1,
                         layerId='Aspect',
                         group='Aspect') %>%
          addRasterImage(x=landscape$fueltype,
                         project=FALSE,
                         colors=colours_fbp,
                         opacity=0.5,
                         layerId='FBP',
                         group='FBP') %>%
          addLayersControl(
            baseGroups=c('Elevation', 'Slope', 'Aspect'),
            overlayGroups=c('FBP'),
            options = layersControlOptions(collapsed = TRUE)
          ) %>%
          fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
          addMarkers(data=pt,
                     layerId='origin',
                     icon=icon_origin)
      })
      print('Draw main map')
      leafletProxy("map") %>%
        addMarkers(data=pt,
                   layerId='origin',
                   icon=icon_origin)
      print('Showing wx')
      print(wx)
      print('Update slider')
      startTime <- min(wx$DATETIME) + hours(10)
      updateSimulationTimeSlider(startTime)
    }
    print('Done initial load')
    # HACK: try to minimize memory usage
    gc()
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
  iv$add_rule('duration',
              compose_rules(
                sv_required(),
                sv_numeric()
              ))
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
    shinyjs::delay(100, {
      shinyjs::show('div_info')
      shinyjs::show('div_sim')
    })
  })
  observeEvent(input$duration, {
    updateSimulationTimeSlider()
  })
  observeEvent(input$startTime, {
    updateFBPOriginTable()
  })
  observeEvent(input$do_reset, {
    landscape <- session$userData$landscape
    lat <- session$userData$latitude
    lon <- session$userData$longitude
    time <- session$userData$startTime
    updatePoints(start_fire(landscape, lat, lon, time))
  })
  observeEvent(input$do_step, {
    landscape <- session$userData$landscape
    wx <- session$userData$wx
    updatePoints(spread(landscape, wx))
  })
  session$onSessionEnded(stopApp)
}
