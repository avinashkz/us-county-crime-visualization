#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(markdown)
library(DT)
library(plotly)

navbarPage("US Crime Visualization",
           tabPanel("States",
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"),
                        
                        fluidRow(
                          column(10, plotlyOutput("geoPlot"))#,
                          #column(2)
                        ),
                        br(),
                        fluidRow(
                          column(3
                                 
                                 # Extra options for user.
                                 # checkboxGroupInput("checkGroup", 
                                 #                    h3("Extra Options", id = "myh3"), 
                                 #                    choices = c("Remove Legend" = 1),
                                 #                    selected = c())
                          ),
                          column(9 )
                        ),
                    
                        br(),
                        br(),
                        leafletOutput("countyplot")
           ),
           tabPanel("Table",
                    DT::dataTableOutput("mytable")
           ),
           navbarMenu("More",
                      tabPanel("Others",
                               #https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height/36471739#36471739
                               tags$style(type = "text/css", "#alternateplot {height: calc(60vh - 80px) !important;}"),
                               absolutePanel(top = 80, width = "49%",
                                             leafletOutput("alternateplot", width = "100%")),
                               absolutePanel(bottom = 0, left = 20, width = "49%", height = "40%",
                                             fluidRow(
                                               
                                               column(12, radioButtons("radio", h3("Select Crime", id = "myh3"),
                                                            choices = list( "All Crimes" = "violent","Rape" = "rape", 
                                                                            "Assault" = "assault", "Homicide" = "homicide",
                                                                            "Robbery" = "robbery"), inline = TRUE, selected = "violent")),
                                               
                                               column(5, 
                                                      
                                                      radioButtons("selection", h3("Ranking"), 
                                                                   choices = list("Top 10" = 1, "Bottom 10" = 2,
                                                                                  "All" = 3), inline = TRUE, selected = 1),
                                                      
                                                      sliderInput("slider", h3("Filter Year", id = "myh3"), 
                                                                  min = 2009, max = 2016, value = c(2009, 2016), sep = "")
                                                      
                                                      ),
                                               column(7, 
                                                      uiOutput("cities"))
                                             )
                                             ),
                               absolutePanel(top = 70, right = 0, width = "49%", height = "80%",
                                             plotlyOutput("barPlot"),
                                             plotlyOutput("linePlot", height = "53%")
                               )
                               
                      ),
                      tabPanel("About"
                      )
           ),
fluid = TRUE)

