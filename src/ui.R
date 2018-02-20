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

navbarPage("US Crime Visualization",
           tabPanel("States",
                    sidebarLayout(
                      sidebarPanel(
                        uiOutput("stateyear"),
                        radioButtons("radio", h3("Select Feature", id = "myh3"),
                                     choices = list( "Total Crimes" = "violent","Total Population" = "pop","Rape" = "rape",
                                                     "Assault" = "assault", "Homicide" = "homicide", "Robbery" = "robbery"),selected = "violent"),
                        uiOutput("states")
                        
                      ),
                      mainPanel(
                        plotOutput("plot")
                      )
                    )
           ),
           tabPanel("Counties",
                    verbatimTextOutput("summary")
           ),
           navbarMenu("More",
                      tabPanel("Table",
                               DT::dataTableOutput("table")
                      ),
                      tabPanel("About"
                      )
           )
)
