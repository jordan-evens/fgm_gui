source('common.R')
source('weather.R')
source('fbp.R')
# library('rgeos')
library('DT')
library('shinyvalidate')

NUM_DATATABLE_ROWS <- 10
DEFAULT_OPACITY <- 0.5

server <- function(input, output, session) {
  makeRamp <- function(colours, value_min, value_max, breaks) {
    bins <- (value_max - value_min) / breaks
    return(colorBin(colorRampPalette(colours)(bins), domain=c(value_min, value_max), bins=bins))
  }
  # Just use world values (m) for now
  COLOURS_ELEV <- makeRamp(c('white', 'black'), -500, 9000, 100)
  COLOURS_SLOPE <- makeRamp(c('white', 'red'), 0, 300, 10)
  COLOURS_ASPECT <- makeRamp(c('white', 'red', 'black', 'blue', 'white'), 0, 360, 10)
  COLOURS_BURNT <- makeRamp(c('white', 'black'), 0, 2, 1)
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
  getPerimeter <- function(sim_env=NULL){
    if (is.null(sim_env)) {
      sim_env <- session$userData$sim_env
      if (is.null(sim_env)) {
        return()
      }
    }
    burnt <- raster::rasterToPolygons(sim_env$landscape$burnt, fun=function(x) {x > 0}, digits=1, dissolve=TRUE)
    if (is.null(burnt)) {
      return(NULL)
    }
    perim <- st_as_sf(burnt)
    # HACK: missing crs for some reason
    st_crs(perim) <- st_crs(sim_env$landscape)
    perim <- st_transform(perim, PROJ_DEFAULT)
    return(perim)
  }
  addPerimeter <- function(m, sim_env=NULL) {
    perim <- getPerimeter()
    if (is.null(perim)) {
      print('No perimeter to add')
      return(m)
    }
    print('Adding perim')
    return(m %>%
             clearGroup('Perimeter') %>%
             addPolygons(data=perim,
                         group='Perimeter',
                         opacity=1,
                         weight=5,
                         # fill=FALSE,
                         color='black'))

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
    landscape <- session$userData$sim_env$landscape
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
    print("formatting date")
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
  updatePoints <- function(sim_env) {
    pts <- sim_env$points
    landscape <- sim_env$landscape
    print(sprintf('Called updatePoints() with %d points', nrow(pts)))
    print(sprintf('Updating burnt raster to have %d burnt cells', sum(values(landscape$burnt))))
    draw_perimeter <- function(map_id) {
      m <- leafletProxy(map_id) %>%
        addPerimeter() %>%
        removeImage('Burnt') %>%
        addRasterImage(x=landscape$burnt,
                       project=FALSE,
                       colors=COLOURS_BURNT,
                       opacity=DEFAULT_OPACITY,
                       layerId='Burnt',
                       group='Burnt')
    }
    draw_perimeter('map')
    draw_perimeter('map_zoom')
    clear_points <- function(map_id) {
      print('clearing active points')
      m <- leafletProxy(map_id) %>%
        clearGroup('active')
    }
    clear_points('map')
    clear_points('map_zoom')
    df <- as.data.frame(NULL)
    if (!is.null(pts)) {
      pts_map <- st_transform(pts, PROJ_DEFAULT)
      add_points <- function(map_id) {
        print('drawing active points')
        m <- leafletProxy(map_id) %>%
          addCircles(
            data=pts_map,
            color='red',
            radius=10,
            weight=10,
            opacity=1,
            fillOpacity=1,
            group='active')
      }
      add_points('map')
      add_points('map_zoom')
      df <- as.data.frame(st_coordinates(pts_map))
    }
    print('Updating simTime')
    updateTextInput(session, 'simTime', value=sim_env$time)
    output$points <- DT::renderDT(
      df,
      options=list(
        dom='t',
        pageLength=nrow(df),
        autoWidth=TRUE,
        scrollX=TRUE,
        # HACK: maybe this works?
        scrollY=TRUE,
        paging=FALSE
      ),
      server=FALSE,
      rownames=FALSE
    )
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
  colours_fbp <- BASE_DATA$FCT_COLOURS_FBP
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
                     opacity=DEFAULT_OPACITY,
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
    sim_env <- getSimulationEnvironment(lat, lon, session$userData$sim_env)
    is_same_env <- !is.null(session$userData$sim_env) && session$userData$sim_env$origin_cell == sim_env$origin_cell
    session$userData$latitude <- lat
    session$userData$longitude <- lon
    wx <- get_weather(lat, lon)
    session$userData$wx <- wx
    updateWeather(wx)
    print('Got sim_env')
    print(sim_env)
    print('Checking landscape')
    if (!is.null(sim_env) && !is.null(sim_env$landscape)) {
      startTime <- min(wx$DATETIME) + hours(10)
      sim_env$startTime <- startTime
      pt <- sim_env$origin
      landscape <- sim_env$landscape
      session$userData$sim_env <- sim_env
      stopifnot(!is.null(session$userData$sim_env))
      updateTextInput(session, 'latitude', value=lat)
      updateTextInput(session, 'longitude', value=lon)
      shinyjs::show('div_map_zoom')
      print('Get bbox')
      bbox <- as.vector(st_bbox(st_transform(st_as_sf(as.polygons(ext(landscape), crs=as.character(crs(landscape)))), PROJ_DEFAULT)))
      print('Drawing zoomed map')
      if (!is_same_env) {
        # # moved, so reset simulation
        # if (!is.null(session$userData$sim_env$started)) {
        #   rm(session$userData$sim_env$started)
        # }
        fct_start_fire(sim_env=sim_env, lat=lat, lon=lon, time=startTime)
        pt <- sim_env$origin
        landscape <- sim_env$landscape
        print('Adding new landscape to map_zoom')
        output$map_zoom <- renderLeaflet({
          # no point in rendering again if environment didn't change
          print(sprintf('Rendering: Updating burnt raster to have %d burnt cells', sum(values(landscape$burnt))))
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
                           opacity=DEFAULT_OPACITY,
                           layerId='FBP',
                           group='FBP') %>%
            addRasterImage(x=landscape$burnt,
                           project=FALSE,
                           colors=COLOURS_BURNT,
                           opacity=DEFAULT_OPACITY,
                           layerId='Burnt',
                           group='Burnt') %>%
            addLayersControl(
              baseGroups=c('Elevation', 'Slope', 'Aspect'),
              overlayGroups=c('FBP', 'Burnt'),
              options = layersControlOptions(collapsed = TRUE)
            ) %>%
            fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
            addMarkers(data=pt,
                       layerId='origin',
                       icon=icon_origin)
        })
      } else {
        # HACK: seems like leafletProxy doesn't work if we just set it above with new env
        print('Add markers to map_zoom')
        print(sprintf('Updating burnt raster to have %d burnt cells', sum(values(landscape$burnt))))
        leafletProxy('map_zoom') %>%
          addPerimeter() %>%
          clearGroup('active') %>%
          removeImage('Burnt') %>%
          addRasterImage(x=landscape$burnt,
                         project=FALSE,
                         colors=COLOURS_BURNT,
                         opacity=DEFAULT_OPACITY,
                         layerId='Burnt',
                         group='Burnt') %>%
          addMarkers(data=pt,
                     layerId='origin',
                     icon=icon_origin)
      }
      print('Add markers to main map')
      leafletProxy("map") %>%
        addMarkers(data=pt,
                   layerId='origin',
                   icon=icon_origin)
      print('Showing wx')
      print(wx)
      print('Update slider')
      updateSimulationTimeSlider(startTime)
      # print("Update points")
      # updatePoints(session$userData$sim_env)
      # output$points <- DT::renderDT(NULL)
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
  fct_start_fire <- function(sim_env=session$userData$sim_env,
                             lat=session$userData$latitude,
                             lon=session$userData$longitude,
                             time=session$userData$startTime) {
    session$userData$sim_env <- start_fire(sim_env, lat, lon, time)

  }
  fct_reset <- function() {
    fct_start_fire()
    print(sprintf('After start_fire(), have %d burnt cells', sum(values(session$userData$sim_env$landscape$burnt))))
    updatePoints(session$userData$sim_env)
  }
  observeEvent(input$do_reset, {
    fct_reset()
  })
  observeEvent(input$do_step, {
    print("do_step")
    wx <- session$userData$wx
    print(wx)
    if (is.null(session$userData$sim_env$started)) {
      # sim_env <- start_fire(sim_env,
      #                       as.numeric(input$latitude),
      #                       as.numeric(input$longitude),
      #                       sim_env$startTime)
      # stopifnot(!is.null(sim_env$points))
      fct_start_fire()
    }
    # print("Printing user data")
    # print(session$userData)
    # stopifnot(!is.null(session$userData$sim_env$points))
    session$userData$sim_env <- spread(session$userData$sim_env, wx)
    stopifnot(!is.null(session$userData$sim_env))
    print(session$userData$sim_env)
    print("Done user data")
    print(sprintf('After spread(), have %d burnt cells', sum(values(session$userData$sim_env$landscape$burnt))))
    updatePoints(session$userData$sim_env)
  })
  session$onSessionEnded(stopApp)
}
