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
library(shinyjs)

navbarPage("US Crime Visualization",
           tabPanel("Dashboard",
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }",
                                   "#alternateplot {height: calc(60vh - 80px) !important;}",
                                   '.navbar { color: black; }',
                                   
                                   '.navbar-dropdown {color: black; }',
                                   
                                   '.navbar-default .navbar-brand {
                                   color: black;
                                   }'),
                    
                    
                    #https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height/36471739#36471739
                    
                    shinyjs::useShinyjs(),
                    #https://stackoverflow.com/questions/30852356/add-a-page-refresh-button-by-using-r-shiny?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
                    shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
                    #https://shiny.rstudio.com/gallery/absolutely-positioned-panels.html
                    absolutePanel(top = 80, width = "49%",
                                  leafletOutput("alternateplot", width = "100%")),
                    absolutePanel(bottom = 0, left = 20, width = "50%", height = "40%",
                                  fluidRow(
                                    #https://stackoverflow.com/questions/41048584/radio-buttons-and-select-inputs-in-one-row-in-shiny?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
                                    column(8, radioButtons("radio", h3("Select Crime", id = "myh3"),
                                                            choices = list( "All Crimes" = "violent","Rape" = "rape", 
                                                                            "Assault" = "assault", "Homicide" = "homicide",
                                                                            "Robbery" = "robbery"), inline = TRUE, selected = "violent")
                                           ),
                                    
                                    column(4, radioButtons("selection", h3("Ranking"), 
                                                           choices = list("Top 5" = 1, "Bottom 5" = 2),
                                                           inline = TRUE, selected = 1)
                                           
                                  )

                    ),
                    
                    fluidRow(
                      column(6, sliderInput("slider", h3("Filter Year", id = "myh3"), 
                                         min = 2009, max = 2016, value = c(2009, 2016), sep = ""),
                             actionButton("refresh", h5("View US Map"))
                      ),
                      
                      hr(style="border:0px;"),
                      
                      column(6, uiOutput("cities")
                             
                      )
                      
                    )
                    
                    
                    
                    ),
                    absolutePanel(top = 70, right = 0, width = "49%", height = "80%",
                                  plotlyOutput("barPlot"),
                                  plotlyOutput("linePlot", height = "53%")
                    )
                    
           ),
           tabPanel("Table",
                    DT::dataTableOutput("mytable")
           ),
           navbarMenu("More",
                      tabPanel("About"
                      )
           ),
fluid = TRUE)

