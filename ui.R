library(shiny)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(rgdal)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)

ui<-fillPage(
    fluidPage(
    tags$head(HTML("<title>FGM Playground</title>")),
    useShinyjs(),
    br(),
    h1("Fire Growth Modelling Playground"),
    br(),br(),
    column(8,
    withSpinner(leafletOutput("map", height='100vh'))
    ),
    column(4,
    hidden(div(id='div_map_zoom',
    withSpinner(leafletOutput('map_zoom'))
    ))
    )
    )
  )
