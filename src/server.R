library(shiny)
library(plotly)
library(tidyverse)
library(shinycssloaders)
library(DT)
library(shinydashboard)
library(shinyjs)
library(feather)
library(rgdal)
library(readr)

shinyServer(function(input, output) {
  
  
  get_radio <- reactive({
    
    #Switching the y-axis of scatter and line plot
    
    if(input$radio == "violent") {
      xtitle <- "Violent Crimes"
      title <- "Violent Crime Trend In"
      y <- "violent_crime"
      q <- "violent"
      color <- 1
    }
    else if(input$radio == "rape") {
      xtitle <- "Rapes"
      title <- "Rape Trend In"
      y <- "rape_sum"
      q <- "rape"
      color <- 2
    }
    else if(input$radio == "assault") {
      xtitle <- "Assaults"
      title <- "Assault Trend In"
      y <- "agg_ass_sum"
      q <- "assault"
      color <- 4
    }
    else if(input$radio == "homicide") {
      xtitle <- "Homicides"
      title <- "Homicide Trend In"
      y <- "homs_sum"
      q <- "homicide"
      color <- 5
    }
    else if(input$radio == "robbery") {
      xtitle <- "Robberies"
      title <- "Robbery Trend In"
      y <- "rob_sum"
      q <- "robbery"
      color <- 6
    }
    
    return(c(xtitle, title, y, q, color))
    
  })
  
  crime_switch <- reactive({
    
    geo_click <- event_data("plotly_click")
    
    #To handle error when starting application. No click at start!
    if (!length(geo_click)) {return(tibble())}
    
    x <- geo_data %>% filter(get(input$radio) == geo_click$z)
    #Fix issue of changing crime type.
    if(nrow(x)) {global_state <<- x}
    
    return(global_state)
    
  })
  
  

  output$geoPlot <- renderPlotly({
    
    #Function for US plotly map
    
    font <- "'Lucida Console', Monaco, monospace"
    f <- list(family = font)
    
    my_colors <- c('#008d4a', '#ffc300', '#FD850E', '#e31a1c', '#5D11A9', '#1131A9')
    
    radio_data <- get_radio()
    xtitle <- radio_data[1]
    title <- radio_data[2]
    q <- radio_data[4]
    color <- my_colors[as.numeric(radio_data[5])]
    
    #Filter the data for plotting the geo map
    geo_data <<- crime %>% filter(year == 2014) %>%  group_by(region, code) %>%
      summarise(rape = sum(rape_sum, na.rm = TRUE), assault = sum(agg_ass_sum, na.rm = TRUE),
                robbery = sum(rob_sum, na.rm = TRUE), homicide = sum(homs_sum, na.rm = TRUE), 
                violent = sum(violent_crime, na.rm = TRUE))
    
    #Creating column for contents to hover
    geo_data$hover <- with(geo_data, paste(region, '<br>',
                                           "Rape: ",rape, '<br>',"Assault: ", assault,'<br>',
                                           "Robbery: ", robbery, '<br>',"Homicide: ", homicide))
    
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    #Plot US map using shiny
    plot1 <- plot_geo(geo_data, locationmode = 'USA-states') %>%
      add_trace(
        z = ~get(q), text = ~hover, locations = ~code,
        color = ~sqrt(sqrt(get(q))), colors = c(color, "#323232")
      ) %>%
      colorbar(title = xtitle) %>%
      layout(
        title = paste('<br>', title, 'Map(State Selector)'),
        geo = g, font = f
      )
  })
  
  

  output$linePlot2 <- renderPlotly({
    # Function for line and scatter plot
    
    #Reading in the extra option multiple selector input
    a <- !str_detect(paste(input$checkGroup, collapse = ","), "1")
    #b <- !str_detect(paste(input$checkGroup, collapse = ","), "2")
    
    font <- "'Lucida Console', Monaco, monospace"
    
    #Setting the font size
    f <- list(family = font)
    
    #Switching between line and markers
    m <- 'lines+markers'
    
    radio_data <- get_radio()
    xtitle <- radio_data[1]
    title <-radio_data[2]
    y <- radio_data[3]
    
    #Detect geo click
    x <- crime_switch()
    
    #Readin the input from city selector
    mycities <- input$cityInput

    
    if (nrow(x) & length(mycities)) {
      
      x <- crime_switch()
      
      p <- crime %>% 
        filter(region == x$region, year >= input$slider[1], year <= input$slider[2]) %>%
        mutate(city = str_to_title(city)) %>% 
        filter(city %in% mycities) %>% 
        plot_ly(x = ~year, y = ~get(y), type = 'scatter', 
                mode = m, split = ~city,  text = ~paste("Total Crime In ", city)) %>% 
        layout(title = ~paste(title, x$region), font = f ,
               xaxis = list(title = "Years", titlefont = f, tickfont = f),
               yaxis = list(title = xtitle, titlefont = f, tickfont = f),
               legend = list(font = f), showlegend = a)
      
      # } else if (length(mycities)){
      #   #observe({print("I am here too")})
      #   p <- crime %>% filter(year >= input$slider[1], year <= input$slider[2]) %>% filter(city %in% mycities) %>% 
      #     plot_ly(x = ~year, y = ~get(y), type = 'scatter', 
      #             mode = m, split = ~city,  text = ~paste("Total Crime In ", city)) %>% 
      #     layout(title = ~paste("<br>",title, x[[1]]), font = f ,xaxis = list(title = "Years", titlefont = f, tickfont = f),
      #            yaxis = list(title = xtitle, titlefont = f, titlefont = f),
      #            legend = list(font = f),showlegend = c)
    } else {
      plot_data <- crime %>% filter(year >= input$slider[1], year <= input$slider[2])  %>%
        group_by(region, year) %>%
        summarise(custom = sum(get(y), na.rm = TRUE))
      
      plot_data %>% 
        plot_ly(x = ~year, y = ~custom, type = 'scatter', 
                mode = m, split = ~region,  text = ~paste("Total Crime In ", region)) %>% 
        layout(title =  ~paste(title, "US"), font = f, 
               xaxis = list(title = "Years", titlefont = f, tickfont = f),
               yaxis = list(title = xtitle, titlefont = f, titlefont = f),
               legend = list(font = f),showlegend = a)
    }
  })
  
  
  output$countyplot <- renderLeaflet({
    
    radio_data <- get_radio()
    xtitle <- radio_data[1]
    title <- radio_data[2]
    y <- radio_data[3]
    
    x <- crime_switch()
    
    if (nrow(x)) {

      custom <- crime %>% filter(year == 2016)
      
      second <- left_join(df, custom, by = c("code", "NAME" = "city"))
      
      leaf_data@data <- second    
      
      neStates <- subset(leaf_data, leaf_data$code == x$code)
      
      pal <- colorNumeric("viridis", NULL)
      
      leaflet(neStates) %>% addTiles() %>% 
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 1,
                    fillColor = ~pal(get(y)),
                    label = ~paste0(city,": ", get(y)),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE)) %>% 
        addLegend(pal = pal, values = ~get(y), title = ~paste0(xtitle), bins = 3, opacity = 1.0)
      
    }
  })
  
  #https://stackoverflow.com/questions/36980999/change-plotly-chart-y-variable-based-on-selectinput#
  
  
  output$cities <- renderUI({
    
    #Function for multiple city selector
    x <- crime_switch()
    
    if (nrow(x)) {
      q <- crime %>% filter(region == x$region) %>%
        mutate(city = str_to_title(city)) %>% 
        group_by(city) %>% summarise() %>% as.list()
      selectInput(
        "cityInput",
        h3("Cities Selected"),
        sort(q$city),
        selected = q$city,
        multiple = TRUE)
    }
  })
  
  
  
  data_func <- reactive({
    #To update data table when filters change
    mycities <- input$cityInput
    if(length(mycities)) {
      crime %>%
        filter(year >= input$slider[1], year <= input$slider[2]) %>%
        filter(city %in% mycities)
      } else
      {
      crime %>%
          filter(year >= input$slider[1], year <= input$slider[2])
        }
  })
  
  
  
  output$mytable <- renderDataTable({
    
    #Function to output the data table
    DT::datatable(data = data_func(),
                  extensions = 'Buttons', 
                  options = list(
                    scrollX = TRUE,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ))
  })
  
  
  output$menuitem <- renderMenu({
    
    #For rendering the tabs of shiny dashboard
    menuItem("Menu item", icon = icon("calendar"))
  })
  
})

