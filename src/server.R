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
      xtitle <- "Rape"
      title <- "Rape Trend In"
      y <- "rape_sum"
      q <- "rape"
      color <- 2
    }
    else if(input$radio == "assault") {
      xtitle <- "Assault"
      title <- "Assault Trend In"
      y <- "agg_ass_sum"
      q <- "assault"
      color <- 4
    }
    else if(input$radio == "homicide") {
      xtitle <- "Homicide"
      title <- "Homicide Trend In"
      y <- "homs_sum"
      q <- "homicide"
      color <- 5
    }
    else if(input$radio == "robbery") {
      xtitle <- "Robbery"
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
  
  
  output$linePlot <- renderPlotly({
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
      
      pre_plot_data <- crime %>% filter(region == x$region, year >= input$slider[1], year <= input$slider[2]) %>%
        mutate(city = str_to_title(city), custom = get(y)) %>% 
        select(region, city, year, custom) %>% 
        filter(city %in% mycities) %>%
        spread("year", "custom") %>%
        arrange(get(as.character(input$slider[1])))
        #arrange(`2014`) %>% View()
      
      #Reordering the factors so that legend appears in correct order
      plot_data <- pre_plot_data %>% gather("year", "custom", 3:ncol(pre_plot_data)) #%>% View()
      plot_data$city <- factor(plot_data$city, levels = rev(pre_plot_data$city))
        
      plot_data %>% plot_ly(x = ~year, y = ~custom, type = 'scatter',
                mode = m, split = ~city,  text = ~paste("Total Crime In ", city)) %>% 
        layout(title = ~paste(""), font = f ,
               xaxis = list(title = "", titlefont = f, tickfont = f),
               yaxis = list(title = xtitle, titlefont = f, tickfont = f),
               legend = list(font = f), showlegend = a)
      
    } else {
      
      if(input$selection == 1){
        pre_plot_data <- crime %>% filter(year >= input$slider[1], year <= input$slider[2]) %>%
          group_by(region, year) %>% 
          summarise(custom = sum(get(y), na.rm = TRUE)) %>% spread("year", "custom") %>%
          arrange(get(as.character(input$slider[1]))) %>% tail(10) 

      } else if(input$selection == 2){
        pre_plot_data <- crime %>% filter(year >= input$slider[1], year <= input$slider[2]) %>%
          group_by(region, year) %>% 
          summarise(custom = sum(get(y), na.rm = TRUE)) %>% spread("year", "custom") %>%
          arrange(get(as.character(input$slider[1]))) %>% head(10) 
        
      } else{
        pre_plot_data <- crime %>% filter(year >= input$slider[1], year <= input$slider[2]) %>%
          group_by(region, year) %>% 
          summarise(custom = sum(get(y), na.rm = TRUE)) %>% spread("year", "custom") %>%
          arrange(get(as.character(input$slider[1])))
      }

      #https://stackoverflow.com/questions/48159713/plotly-r-order-scatter-plot-legend-entries?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
      #Reordering the factors so that legend appears in correct order
      plot_data <- pre_plot_data %>% gather("year", "custom", 2:ncol(pre_plot_data))
      plot_data$region <- factor(plot_data$region, levels = rev(pre_plot_data$region))
      
      plot_data %>% 
        plot_ly(x = ~year, y = ~custom, type = 'scatter', 
                mode = m, split = ~region,  text = ~paste("Total Crime In ", region)) %>% 
        layout(title =  ~paste(""), font = f, 
               xaxis = list(title = "", titlefont = f, tickfont = f),
               yaxis = list(title = xtitle, titlefont = f, titlefont = f),
               legend = list(font = f),showlegend = a)
    }
  })
  
  
  output$barPlot <- renderPlotly({
    
    # Function for bar plot
    
    #Reading in the extra option multiple selector input
    #a <- !str_detect(paste(input$checkGroup, collapse = ","), "1")
    
    font <- "'Lucida Console', Monaco, monospace"
    
    #Setting the font size
    f <- list(family = font)
    
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
      
      pre_plot_data <- crime %>% filter(region == x$region, year == input$slider[2]) %>% rowwise() %>% 
        mutate(violent_crime = sum(homs_sum, rob_sum, agg_ass_sum, rape_sum, na.rm = TRUE)) %>% 
        mutate(city = str_to_title(city)) %>% 
        arrange(violent_crime) %>% tail(5) 
      
      plot_data <- pre_plot_data
      plot_data$city <- factor(plot_data$city, levels = rev(pre_plot_data$city))
      
      plot_ly(plot_data, x = ~city, y = ~agg_ass_sum, type = 'bar', name = 'Assault', text = ~agg_ass_sum,
              textposition = 'auto',
              marker = list(line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        add_trace(y = ~rob_sum, name = 'Robbery', text = ~rob_sum,
                  textposition = 'auto',
                  marker = list(line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        add_trace(y = ~rape_sum, name = 'Rape', text = ~rape_sum,
                  textposition = 'auto',
                  marker = list(line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        add_trace(y = ~homs_sum, name = 'Homicide', text = ~homs_sum,
                  textposition = 'auto',
                  marker = list(line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        layout(yaxis = list(title = 'Count'), barmode = 'stack') %>% 
        layout(title =  ~paste("Violent crimes in ", x$region), font = f, 
               xaxis = list(title = paste(""), titlefont = f, tickfont = f),
               yaxis = list(title = "Violent Crimes", titlefont = f, titlefont = f),
               legend = list(font = f),showlegend = TRUE)
      
    } else {
      
      #https://stackoverflow.com/questions/48159713/plotly-r-order-scatter-plot-legend-entries?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
      #Reordering the factors so that legend appears in correct order

      if(input$selection == 1){
        pre_plot_data <- crime %>% filter(year == input$slider[2]) %>% rowwise() %>% 
          mutate(violent_crime = sum(homs_sum, rob_sum, agg_ass_sum, rape_sum, na.rm = TRUE)) %>% 
          mutate(region = str_to_title(region)) %>% 
          group_by(region) %>% summarise(violent_crime = sum(violent_crime, na.rm = TRUE), 
                                         homs_sum = sum(homs_sum, na.rm = TRUE), 
                                         rob_sum = sum(rob_sum, na.rm = TRUE), 
                                         agg_ass_sum = sum(agg_ass_sum, na.rm = TRUE), 
                                         rape_sum = sum(rape_sum, na.rm = TRUE)) %>% 
          arrange(violent_crime) %>% tail(5) 
        
      } else {
        pre_plot_data <- crime %>% filter(year == input$slider[2]) %>% rowwise() %>% 
          mutate(violent_crime = sum(homs_sum, rob_sum, agg_ass_sum, rape_sum, na.rm = TRUE)) %>% 
          group_by(region) %>% summarise(violent_crime = sum(violent_crime, na.rm = TRUE), 
                                         homs_sum = sum(homs_sum, na.rm = TRUE), 
                                         rob_sum = sum(rob_sum, na.rm = TRUE), 
                                         agg_ass_sum = sum(agg_ass_sum, na.rm = TRUE), 
                                         rape_sum = sum(rape_sum, na.rm = TRUE)) %>% 
          arrange(violent_crime) %>% head(5) 
      }
      
      
      
      plot_data <- pre_plot_data
      plot_data$region <- factor(plot_data$region, levels = rev(pre_plot_data$region))
      
      plot_ly(plot_data, x = ~region, y = ~agg_ass_sum, type = 'bar', name = 'Assault', text = ~agg_ass_sum,
              textposition = 'auto',
              marker = list(line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        add_trace(y = ~rob_sum, name = 'Robbery', text = ~rob_sum,
                  textposition = 'auto',
                  marker = list(line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        add_trace(y = ~rape_sum, name = 'Rape', text = ~rape_sum,
                  textposition = 'auto',
                  marker = list(line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        add_trace(y = ~homs_sum, name = 'Homicide', text = ~homs_sum,
                  textposition = 'auto',
                  marker = list(line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        layout(yaxis = list(title = 'Count'), barmode = 'stack') %>% 
        layout(title =  ~paste("Violent crimes in US"), font = f, 
               xaxis = list(title = paste(""), titlefont = f, tickfont = f),
               yaxis = list(title = "Violent Crimes", titlefont = f, titlefont = f),
               legend = list(font = f),showlegend = TRUE)
      
      
    }
    

    
  })
  
  output$countyplot <- renderLeaflet({
    
    radio_data <- get_radio()
    xtitle <- radio_data[1]
    title <- radio_data[2]
    y <- radio_data[3]
    
    x <- crime_switch()
    
    if (nrow(x)) {

      custom <- crime %>% filter(year == input$slider[2])
      
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
        addLegend(pal = pal, values = ~get(y), title = ~paste0(xtitle), bins = 4, opacity = 1.0)
      
    }
  })
  
  #https://stackoverflow.com/questions/36980999/change-plotly-chart-y-variable-based-on-selectinput#
  
  
  output$cities <- renderUI({
    
    #Function for multiple city selector
    x <- crime_switch()
    
    radio_data <- get_radio()
    y <- radio_data[3]
    
    
    all_cities <- crime %>% filter(region == x$region, year == input$slider[1]) %>%
      mutate(city = str_to_title(city)) %>% 
      group_by(city) %>% summarise() %>% as.list()
  
    
    if ((input$selection == 1) & nrow(x)) {
      q <- crime %>% filter(region == x$region, year == input$slider[1]) %>%
        arrange(get(y)) %>% 
        tail(10) %>% 
        mutate(city = str_to_title(city)) %>% 
        group_by(city) %>% summarise() %>% as.list()
      observe({print(q$city)})
      selectInput(
        "cityInput",
        h3("Cities Selected"),
        sort(all_cities$city),
        selected = c(q$city),
        multiple = TRUE)
    } else if ((input$selection == 2) & nrow(x)) {
      q <- crime %>% filter(region == x$region, year == input$slider[1]) %>% 
        arrange(get(y)) %>% 
        head(10) %>% 
        mutate(city = str_to_title(city)) %>% 
        group_by(city) %>% summarise() %>% as.list()
      observe({print(q$city)})
      selectInput(
        "cityInput",
        h3("Cities Selected"),
        sort(all_cities$city),
        selected = c(q$city),
        multiple = TRUE)
    } else if (nrow(x)) {
      selectInput(
        "cityInput",
        h3("Cities Selected"),
        sort(all_cities$city),
        selected = all_cities$city,
        multiple = TRUE)
    }
    
  })
  
  
  data_func <- reactive({
    #To update data table when filters change
    mycities <- input$cityInput
    if(length(mycities)) {
      crime %>%
        filter(year >= input$slider[1], year <= input$slider[2]) %>%
        mutate(city = str_to_title(city)) %>%
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
  
  
  leaflet_click <- reactive({
    
    click <- input$alternateplot_shape_click
    if(is.null(click)) return()
    
    points <- SpatialPoints(as.data.frame(cbind(click$lng, click$lat)))
    #https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
    proj4string(points) <- proj4string(state_data)
    result <- as.character(over(points, state_data)$STUSPS)
    return(c(result, click$lng, click$lat))
    
  })
  
  
  
  # observeEvent(input$alternateplot_shape_click, {
  #   
  #   click <- input$alternateplot_shape_click
  #   
  #   if(is.null(click)) return()
  #   
  #   points <- SpatialPoints(as.data.frame(cbind(click$lng, click$lat)))
  #   #https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
  #   proj4string(points) <- proj4string(state_data)
  #   result <- as.character(over(points, state_data)$STUSPS)
  #   
  #   output$alternateplot <- renderLeaflet({
  #     
  #     y <- "violent_crime"
  #     
  #     xtitle <- "Violent Crime"
  #     
  #     custom <- crime %>% filter(year == input$slider[2])
  #     
  #     second <- left_join(df, custom, by = c("code", "NAME" = "city"))
  #     
  #     leaf_data@data <- second    
  #     
  #     neStates <- subset(leaf_data, leaf_data$code == result)
  #     
  #     pal <- colorNumeric("viridis", NULL)
  #     
  #     leaflet(neStates) %>% addTiles() %>% 
  #       addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
  #                   opacity = 1.0, fillOpacity = 1,
  #                   fillColor = ~pal(get(y)),
  #                   label = ~paste0(city,": ", get(y)),
  #                   highlightOptions = highlightOptions(color = "white", weight = 2,
  #                                                       bringToFront = TRUE)) %>%
  #       addLegend(pal = pal, values = ~get(y), title = ~paste0(xtitle), bins = 4, opacity = 1.0)
  #     
  #   })
  #   
  # })
  
  
  output$alternateplot <- renderLeaflet({
    
      leaf_state <- leaflet_click()
      state <- leaf_state[1]
      long <- as.numeric(leaf_state[2])
      lat <- as.numeric(leaf_state[3])
      
      
      radio_data <- get_radio()
      xtitle <- radio_data[1]
      title <- radio_data[2]
      y <- radio_data[3]
      #q <- radio_data[4]
      #color <- my_colors[as.numeric(radio_data[5])]
      
      if(is.null(leaf_state)) {
      
      
      #custom <- crime %>% filter(year == input$slider[2])
      
      #second <- left_join(df, custom, by = c("code", "NAME" = "city"))
      
      state_data@data <- states_join   
      
      #neStates <- subset(leaf_data, leaf_data$code == x$code)
      
      pal <- colorNumeric("viridis", NULL)
      
      leaflet(state_data) %>%
        setView(lng = -96, lat = 37.8, 4) %>% addTiles() %>% 
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 1,
                    dashArray = "3",
                    fillColor = ~pal(get(y)),
                    label = ~paste0(NAME,": ", get(y)),
                    highlightOptions = highlightOptions(color = "black", weight = 2,
                                                        bringToFront = TRUE)) %>%
        addLegend(pal = pal, values = ~get(y), title = ~paste0(xtitle), bins = 4, opacity = 1.0, position = "bottomright")

      } else{
        
        custom <- crime %>% filter(year == input$slider[2])
        
        second <- left_join(df, custom, by = c("code", "NAME" = "city"))
        
        leaf_data@data <- second    
        
        neStates <- subset(leaf_data, leaf_data$code == state)
        
        pal <- colorNumeric("viridis", NULL)
        
        leaflet(neStates) %>% addTiles() %>% 
          addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 1,
                      fillColor = ~pal(get(y)),
                      label = ~paste0(city,": ", get(y)),
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE)) %>%
          addLegend(pal = pal, values = ~get(y), title = ~paste0(xtitle), bins = 4, opacity = 1.0, position = "bottomright")
        
      }
      
      
  })
  

  
})