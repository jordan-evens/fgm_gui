library(shiny)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(rgdal)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)

ui <- fillPage(
  useShinyjs(),
  fluidPage(
    tags$head(
      HTML("<title>FGM Playground</title>"),
      tags$style(
        '.alignRight { float: right; }',
        '.alignLeft { float: left; }'
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
      column(8, div()),
      column(4,
             hidden(div(id='div_info',
                        fluidRow(
                          column(6, numericInput('latitude', label='latitude', value=50)),
                          column(6, numericInput('longitude', label='longitude', value=-96))
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
                               DT::DTOutput("weather")
                        )
             ))
      )
    )
  )
)
