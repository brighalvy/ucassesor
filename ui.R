library(tidyverse)
library(dplyr)
library(shinydashboard)
library(shiny)

options(shiny.maxRequestSize = 300*1024^2)

dashboardPage( skin = "blue",
               dashboardHeader(title = "Utah County Assesment"),
               dashboardSidebar(
                 sidebarMenu(
                   menuItem("Update Data", tabName = "dataupdate", icon = icon("align-left", class = NULL, lib = "font-awesome")),
                   menuItem("Ratio Statistics", tabName = "stats", icon = icon("align-left", class = NULL, lib = "font-awesome")),
                   menuItem("Models", tabName = "model", icon = icon("align-left", class = NULL, lib = "font-awesome")),
                   menuItem("Outliers", tabName = "outliers", icon = icon("align-left", class = NULL, lib = "font-awesome")),
                   menuItem("Percent Change", tabName = "pct_change", icon = icon("align-left", class = NULL, lib = "font-awesome")),
                   menuItem("Forecast", tabName = "forecast", icon = icon("align-left", class = NULL, lib = "font-awesome"))
                 )
               ),
               dashboardBody(
                 tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '
                 ))),
                 tabItems(
                   tabItem("dataupdate",
                           shiny::fluidPage(
                             shiny::titlePanel("Input new data"),
                             shiny::mainPanel(
                               shiny::fileInput(inputId = "new_file",
                                                label = "New CSV File"
                               ),
                               shiny::numericInput(inputId = "pyear",
                                                   label = "Value Year",
                                                   value = 0
                               ),
                               shiny::actionButton("run", "Update"),
                               br(),
                               shiny::downloadButton("download", "Download Predictions")
                             )
                           )),
                   tabItem("stats",
                           shiny::fluidRow(
                             shiny::titlePanel("Ratio Stats"),
                             shiny::column(6,
                               shiny::h3("Overall Ratio Stats"),
                               shiny::tableOutput("ratio_stats"),
                               br(),
                               shiny::numericInput(inputId = "br",
                                                   label = "Number of Breaks in Histogram",
                                                   value = 75),
                               shiny::sliderInput("xlim", 
                                                  label = "Range", 
                                                  min = 0,
                                                  max = 2,
                                                  step = .1,
                                                  value = c(.5,1.5)),
                               br(),
                               shiny::plotOutput("ratio_hist"),
                               br()
                             ),
                             shiny::column(5,
                               shiny::h3("Ratio by Value Area"),
                               shiny::tableOutput("table_nbhgrp"),
                               
                             ),
                            shiny::column(3, 
                                          shiny::h3("Ratio by District"),
                                          shiny::tableOutput("table_dist")
                                          )
                           )
                   ),
                     tabItem("model",
                             shiny::fluidPage(
                               shiny::titlePanel("Models"),
                               shiny::sidebarPanel(
                                 shiny::h5("Districts Combined for Modeling:"),
                                 br(),
                                 shiny::tableOutput("new_dist")
                               ),
                               shiny::mainPanel(
                                 shiny::selectInput(inputId = "dist", label = "Modeling District", choices = NULL),
                                 br(),
                                 shiny::tableOutput("coefs"),
                                 br(),
                                 shiny::h6("*Note: The model is predicting the Square Root of the Improvement value so the effects are on the square root.")
                               )
                             )),
                   tabItem("outliers",
                           shiny::fluidPage(
                             shiny::titlePanel("Outliers"),
                             shiny::sidebarPanel(shiny::sliderInput("range",
                                                                    label = "Limits for Outliers",
                                                                    min = 0,
                                                                    max = 2,
                                                                    step = .01,
                                                                    value = c(.72,1.2)),
                                                 shiny::tableOutput("outliercount")),
                             shiny::mainPanel(
                               shiny::titlePanel("Parcel Info"),
                               shiny::tableOutput("outlier_table")
                             )
                           )),
                   tabItem("pct_change",
                           shiny::fluidPage(
                             shiny::titlePanel("Percent Change"),
                             shiny::sidebarPanel(shiny::numericInput(inputId = "brks",
                                                                     label = "Number of Breaks in Histogram",
                                                                     value = 1000),
                                                 shiny::sliderInput("xlim2", 
                                                                    label = "Range", 
                                                                    min = -1,
                                                                    max = 5,
                                                                    step = .1,
                                                                    value = c(0,1))),
                             shiny::mainPanel(
                               shiny::h3("Overal Pct Change"),
                              shiny::tableOutput("pct_change_data"),
                              br(),
                              shiny::plotOutput("pct_hist"),
                              br(),
                              shiny::h3("Percent Change by Value Area"),
                              shiny::tableOutput("value_pct"),
                              br(),
                              shiny::selectInput("selectVA", label = "Select Value Area Histogram", choices = NULL),
                              br(),
                              shiny::plotOutput("va_hist"),
                              br(),
                              shiny::h3("Percent Change by District"),
                              shiny::tableOutput("dist_pct"))
                           )),
                   tabItem("forecast",
                           shiny::fluidPage(
                             
                             shiny::mainPanel(
                               shiny::plotOutput("forecast")
                             )
                           ))
                 )
               )
)