source('weather.R')

library(shiny)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(rgdal)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)

DEFAULT_DURATION <- 60
DEFAULT_LATITUDE <- 50
DEFAULT_LONGITUDE <- -96

wx <- get_weather(DEFAULT_LATITUDE, DEFAULT_LONGITUDE)

ui <- fillPage(
  useShinyjs(),
  fluidPage(
    tags$head(
      HTML("<title>FGM Playground</title>"),
      tags$style(
        '.alignRight { float: right; }',
        '.alignLeft { float: left; }',
        'th, td { white-space: nowrap; overflow: hidden; }'
      )),
    useShinyjs(),
    fluidRow(
      column(8, withSpinner(leafletOutput("map"))),
      column(4,
             hidden(div(id='div_map_zoom',
                        withSpinner(leafletOutput('map_zoom'))
             ))
      )
    ),
    fluidRow(
      column(8,
             hidden(div(id='div_sim',
                        style='padding: 10px;',
                        h4('Simulation'),
                        fluidRow(
                          numericInput('duration', label='Duration (min)', value=DEFAULT_DURATION, step=1, min=1, max=3600),
                          sliderInput('startTime',
                                      'Start time',
                                      value=min(wx$DATETIME) + hours(10),
                                      min=min(wx$DATETIME),
                                      max=max(wx$DATETIME) - hours(ceiling(DEFAULT_DURATION / 60)),
                                      step=hours(1))
                        )))
      ),
      column(4,
             hidden(div(id='div_info',
                        fluidRow(
                          column(6, numericInput('latitude', label='latitude', value=DEFAULT_LATITUDE)),
                          column(6, numericInput('longitude', label='longitude', value=DEFAULT_LONGITUDE))
                        ),
                        column(12,
                               style='border: 1px solid #DDDDDD;',
                               fluidRow(
                                 h4('Weather:', style='float: left; padding-left: 10px;'),
                                 div(
                                   style='float: right; padding-right: 10px; padding-top: 2px;',
                                   column(3, actionButton('prev_page', '<', class='alignRight'), style='padding-right: 0px;'),
                                   column(6, selectInput('page', label=NULL, choices=list(), width='100%'), style='padding-left: 2px; padding-right: 2px;'),
                                   column(3, actionButton('next_page', '>', class='alignLeft'), style='padding-left: 0px;')
                                 )
                               ),
                               withSpinner(DT::DTOutput("weather"))
                        )
             ))
      )
    )
  )
)
