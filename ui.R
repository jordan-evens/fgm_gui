source('common.R')
source('weather.R')

library(shiny)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)

DEFAULT_DURATION <- 60
# DEFAULT_LATITUDE <- 50
# DEFAULT_LONGITUDE <- -96
DEFAULT_LATITUDE <- 50.5
DEFAULT_LONGITUDE <- -89
# DEFAULT_LATITUDE <- 51.2931978
# DEFAULT_LONGITUDE <- -117.3210066
wx <- get_weather(DEFAULT_LATITUDE, DEFAULT_LONGITUDE)
timezone <- tz_offset(wx$DATETIME[[1]], tz(wx$DATETIME))$utc_offset_h * 60

ui <- fillPage(
  useShinyjs(),
  fluidPage(
    tags$head(
      HTML("<title>FGM Playground</title>"),
      tags$style(
        '.alignRight { float: right; }',
        '.alignLeft { float: left; }',
        '.versionFooter { position: fixed; float: left; bottom: 0.25em; left: 0.5em; font-size: .75em; color: #CCCCCC}',
        'th, td { white-space: nowrap; overflow: hidden; }'
      )),
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
                                      step=hours(1),
                                      timezone=timezone)
                        ),
                        withSpinner(DT::DTOutput('fbp_origin')),
                        fluidRow(
                          disabled(textInput('simTime', 'Simulation Time')),
                          actionButton('do_reset', 'Reset'),
                          actionButton('do_step', 'Step')
                        ),
                        fluidRow(
                          withSpinner(DT::DTOutput('points'))
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
    ),
    div(id='version',
        class='versionFooter',
        get_version())
  )
)
