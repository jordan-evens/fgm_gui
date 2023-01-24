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
    withSpinner(leafletOutput("Map", height='100vh'))
    )
  )
