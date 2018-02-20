#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

#data <- read_feather("../results/2016.feather")

function(input, output, session) {
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  data_func <- reactive({
    data
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(data = data_func(),
                  extensions = 'Buttons', 
                  options = list(
                    scrollX = TRUE,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ))
  })
  
  
  output$states <- renderUI({
    

  })
  
  output$stateyear <- renderUI({
    selectInput(
      "stateyear",
      h3("Select Year"),
      sort(c(1995:2016)),
      selected = 2016,
      multiple = FALSE)
    
  })
  
}
