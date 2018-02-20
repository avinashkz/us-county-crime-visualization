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

navbarPage("Navbar!",
           tabPanel("Plot",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("plotType", "Plot type",
                                     c("Scatter"="p", "Line"="l")
                        )
                      ),
                      mainPanel(
                        plotOutput("plot")
                      )
                    )
           ),
           tabPanel("Summary",
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
