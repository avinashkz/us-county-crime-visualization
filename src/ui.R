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
                    sidebarLayout(
                      sidebarPanel(
                        uiOutput("year"),

                        sliderInput("slider", h3("Filter Year", id = "myh3"),
                                    min = 1995, max = 2017, value = c(2014, 2016), sep = ""),
                        
                        uiOutput("cities"),
                        
                        radioButtons("radio", h3("Select Feature", id = "myh3"),
                                     choices = list( "Total Crimes" = "violent","Rape" = "rape",
                                                     "Assault" = "assault", "Homicide" = "homicide", "Robbery" = "robbery"),selected = "violent"),
                        
                        
                        # Extra options for user.
                        checkboxGroupInput("checkGroup", 
                                           h3("Extra Options", id = "myh3"), 
                                           choices = c("Crime Per 100k" = 1, 
                                                       "Remove Legend" = 2),
                                           selected = c())
                        
                      ),
                      mainPanel(
                        plotlyOutput("geoPlot"),
                        plotlyOutput("linePlot2")
                      )
                    )
           ),
           tabPanel("Counties",
                    verbatimTextOutput("summary")
           ),
           navbarMenu("More",
                      tabPanel("Table",
                               DT::dataTableOutput("mytable")
                      ),
                      tabPanel("About"
                      )
           )
)
