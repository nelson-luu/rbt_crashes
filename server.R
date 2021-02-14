

# ========================================================
# ========================================================
#                       SERVER CODE
# ========================================================
# ========================================================

crash_data <- read_csv("crash_data.csv")
alcoholcrash_rbt_years <- read_csv("alcoholcrash_rbt_years.csv")
alcoholcrash_years <- read_csv("alcoholcrash_years.csv")
crash_rbt_data <- read_csv("crash_rbt_data.csv")
location_data <- read_csv("location_data.csv")
rbt_data <- read_csv("rbt_data.csv")

server <- function(input, output, session){
  
  # create welcome dialogue when app is first run
  showModal(modalDialog(
    title = "Welcome!",
    HTML("This is <b>Nelson's Data Visualisation Project</b> on how we can improve 
    Random Breath Testing in Victoria to help make our roads safer. <br> <br>
    Please have a read through the description, go through the findings and make your own discoveries
    with this highly interactive and customisable visualisation! <br> <br>
    Please be patient while it loads and apologies for any bugs!")))
  
  # FILTERED MAP DATA
  # create dataframe for proportional symbol map that is reactive on user input
  # such as choosing year, city type 
  filtered_map_data <- reactive({
    
    validate(
      need(input$select_year != "", "Please select at least one year")
    )
    
    location_data <- crash_data %>%
      filter(year(ACCIDENT_DATE) %in% input$select_year) %>%
      group_by(LGA_NAME, STAT_DIV_NAME) %>%
      summarise(city_name = LGA_NAME, STAT_DIV_NAME, LONGITUDE, LATITUDE) %>%
      ungroup() %>%
      group_by(city_name) %>%
      summarise(city_type = unique(STAT_DIV_NAME),
                LONGITUDE = mean(LONGITUDE),
                LATITUDE = mean(LATITUDE),
                accident_count = n())
    
    location_data
    
  })
  
  # FILTER BAR DATA
  # create dataframe for bar chart that is reactive on user input
  # such as year or city name
  filtered_bar_data <- reactive({
    
    validate(
      need(input$select_city != "", "Please select at least one city")
    )
    
    
    city_data <- crash_data %>%
      filter(year(ACCIDENT_DATE) %in% input$select_year) %>%
      filter(LGA_NAME %in% input$select_city) %>%
      group_by(Year, Month) %>%
      summarise(Count = mean(n())) %>%
      ungroup() %>%
      group_by(Month) %>%
      mutate(Total = sum(Count))
    
    city_data
    
  })
  
  
  # OBSERVE BUTTONS
  # provide instructions when button is clicked
  
  # show all cities button
  observeEvent(input$show_cities_button, {
    updateSelectizeInput(session, "select_city", selected = unique(location_data$city_name))
  })
  
  # clear all cities button
  observeEvent(input$clear_cities_button, {
    updateSelectizeInput(session, "select_city", selected = "")
  })
  
  # show all years button
  observeEvent(input$show_year_button, {
    updateCheckboxGroupInput(session, "select_year", selected = unique(alcoholcrash_rbt_years$Year))
    updateSelectInput(session, "select_city_type", selected = "Both")
  })
  
  # clear all years button
  observeEvent(input$clear_year_button, {
    updateCheckboxGroupInput(session, "select_year", selected = "")
    updateSelectInput(session, "select_city_type", selected = "Both")
    # double arrows NECESSARY
    # double arrow searches for variable in parent environment
    # i.e out of local scope
    selection_cities <<- reactiveVal(NULL)
  })
  
  # reset all button
  observeEvent(input$reset_all_button, {
    updateSelectInput(session, "select_city_type", selected = "Both")
    updateSelectizeInput(session, "select_year", selected = "")
    updateSelectizeInput(session, "select_city", selected = "")
    updateSelectizeInput(session, "select_year", selected = unique(alcoholcrash_rbt_years$Year))
    updateSelectizeInput(session, "select_city", selected = unique(location_data$city_name))
    # double arrows NECESSARY
    # double arrow searches for variable in parent environment
    # i.e out of local scope
    selection_cities <<- reactiveVal(NULL)
    
  })
  
  # Show findings buttons
  # Finding 1
  observeEvent(input$f1_button, {
    showNotification(
      HTML(
        "<h3>Finding 1:</h3> <b>You can drag this note</b> <br> <br>
        Here on our left we have a line chart showing the percentage change in Random Breath Tests (RBTs)
        and Alcohol Related Crashes from 2014 to 2018. <br>
        
        One may assume that increasing the number of RBTs
        would lead to capturing a greater number of positive cases which appears to be the case
         during 2015 and 2016. However, on <b>2017 total RBTs increased by 28% but positive RBTs and accident
         counts dropped by about 4%</b> each! One explanation is we may be approaching diminishing returns on
         reducing accident counts as we increase the number of RBTs. This would not be an efficient use
         of resources (RBT, equipment, wages and time). <br> <br>
         
         This suggests to us that <b>as of 2018, given that we are conducting a relatively high number of RBTs, 
         we should either maintain the number of RBTs or reduce to save costs</b>. <br>"
        
      ),
      duration = 180,
      closeButton = TRUE,
      type = "warning"
    )
    # allow message box to be draggable
    jqui_draggable(selector = '.shiny-notification')
    updateSelectInput(session, "select_city_type", selected = "Both")
    updateSelectizeInput(session, "select_year", selected = unique(alcoholcrash_rbt_years$Year))
    updateSelectizeInput(session, "select_city", selected = unique(location_data$city_name))
  })
  
  # Finding 2
  observeEvent(input$f2_button, {
    showNotification(
      HTML(
        "<h3>Finding 2:</h3> <b>You can drag this note</b> <br> <br>
        The plot of Victoria in the middle of the page is known as a proportional symbol map. 
        The circles represent total alcohol accident counts from 2014 to 2018 relative to 
        each regional city. Try hovering over the circles with your mouse cursor. <br> <br>
        
        We find that <b>Geelong</b> (98 Counts), <b>Ballarat</b> (53 Counts), <b>Bendigo</b> (52 Counts) 
        and <b>Mildura</b> (Counts 44) are the top four highest regional Victorian cities in 
        accident counts <br> <br>
        
        <b>Within regional cities, we should allocate a greater proportion of RBTs to 
        cities with higher alcohol related accidents such as Ballarat, Bendigo, Geelong and Mildura.</b><br>"
        
      ),
      duration = 180,
      closeButton = TRUE,
      type = "error"
    )
    # allow message box to be draggable
    jqui_draggable(selector = '.shiny-notification')
    updateSelectInput(session, "select_city_type", selected = "Country")
    updateSelectizeInput(session, "select_year", selected = unique(alcoholcrash_rbt_years$Year))
    updateSelectizeInput(session, "select_city", selected = unique(location_data$city_name))
  })
  
  # Finding 3
  observeEvent(input$f3_button, {
    showNotification(
      HTML(
        "<h3>Finding 3:</h3> <b>You can drag this note</b> <br> <br>
        Looking at the same proportional symbol map we saw in Finding 2, we may need to zoom in (using your mouse
        scroll wheel or pressing the plus '+' symbol at the top left of the map) to get a closer look. <br>
        
        We find that <b>Casey</b> (90 Counts), <b>Yarra Ranges</b> (86 Counts), <b>Dandenong</b> (77 Counts) 
        are the top four highest metropolitan Victorian cities in accident counts <br> <br>
        
        <b>Within Melbourne Metro, we should allocate a greater proportion of RBTs to 
        cities with higher alcohol related accidents such as Casey, Yarra Ranges and Dandenong.</b><br>"
        
      ),
      duration = 180,
      closeButton = TRUE
    )
    # allow message box to be draggable
    jqui_draggable(selector = '.shiny-notification')
    updateSelectInput(session, "select_city_type", selected = "Metro")
    updateSelectizeInput(session, "select_year", selected = unique(alcoholcrash_rbt_years$Year))
    updateSelectizeInput(session, "select_city", selected = unique(location_data$city_name))
  })
  
  # Finding 4
  observeEvent(input$f4_button, {
    showNotification(
      HTML(
        "<h3>Finding 4:</h3> <b>You can drag this note</b> <br> <br>
    Have a look at the stacked bar chart at the bottom of the page. Notice how <b>March and
         December have the highest accident counts</b> compared to all other months. A possible
         explanation for this is that alcohol consumption is more prevalent during seasonal events such
         as Easter, Christmas and New Years Eve. <br> <br>
         This suggests that we should <b>allocate more Random Breath Testing during March and December</b> where
         there is greater likelihood of alcohol being consumed</p> <br>
         Congratulations on making it to Last Finding! Try perform your own data exploration by
         playing around with the filters and see what else you can discover!<br><br>
        <b>Here's a tip:</b> You can click on cities on the map to filter them into
        the bar chart too.<br><br>"
      ),
      duration = 180,
      closeButton = TRUE,
      type = "message"
    )
    # allow message box to be draggable
    jqui_draggable(selector = '.shiny-notification')
    updateSelectInput(session, "select_city_type", selected = "Both")
    updateSelectizeInput(session, "select_year", selected = unique(alcoholcrash_rbt_years$Year))
    updateSelectizeInput(session, "select_city", selected = unique(location_data$city_name))
  })
  
  observeEvent(input$help_button, {
    showNotification(
      HTML(
        "<h3>Stuck?</h3> 
        In most cases clicking the <i>RESET ALL</i> button will revert all changes back
        to their original state. By having clicked the help button, these changes have been
        made for you.<br><br>
        
        <i>I've accidently travelled too far from Australia on the map and now I can't
        get back!</i> <br><br>
        
        Simply press <i>CLEAR ALL CITIES</i> button followed by the <i>SHOW ALL CITIES</i> button and your map will be positioned in
        Victoria again. Similarly, the <i>RESET ALL</i> button followed by selecting a year will 
        achieve the same result.
        "
      ),
      duration = 180,
      closeButton = TRUE,
      type = "message"
    )
    # allow message box to be draggable
    jqui_draggable(selector = '.shiny-notification')
    updateSelectInput(session, "select_city_type", selected = "Both")
    updateSelectizeInput(session, "select_year", selected = "")
    updateSelectizeInput(session, "select_city", selected = "")
    updateSelectizeInput(session, "select_year", selected = unique(alcoholcrash_rbt_years$Year))
    updateSelectizeInput(session, "select_city", selected = unique(location_data$city_name))
    
    # double arrows NECESSARY
    # double arrow searches for variable in parent environment
    # i.e out of local scope
    # selection_cities originally stored cities clicked from map
    selection_cities <<- reactiveVal(NULL)
  })
  
  # create variable to store cities clicked on map
  selection_cities <- reactiveVal(list())
  
  observeEvent(input$mymap_marker_click, {
    p <- input$mymap_marker_click
    
    # this approach was a workaround to using "filtered_map_data()$LATITUDE$lat == p$lat"
    # lat and long coordinates didn't exactly match, possibly something changes
    # during the reactive value of the filtered data
    # differences were small enough to find the corresponding match
    
    pick_city <- (filtered_map_data()$city_name[abs(filtered_map_data()$LATITUDE - p$lat) <= 0.0001 
                                                & abs(filtered_map_data()$LONGITUDE - p$lng) <= 0.0001])
    
    
    # adapted from:
    # https://community.rstudio.com/t/how-to-store-each-click-on-a-different-object-leaflet-shiny/65522
    temp <- selection_cities()
    temp[[length(temp)+1]] <- pick_city
    selection_cities(temp)
    updateSelectizeInput(session, "select_city", selected = selection_cities())
    
  })
  
  # OBSERVE MAP INPUT
  # listen on both user city type and year selection
  observeEvent(input$select_city_type, {
    
    selection_city_type <- input$select_city_type
    if (selection_city_type == "Metro") {
      updateSelectInput(session, "select_city", "Choose city name",
                        choices = location_data[location_data$city_type == "Metro", ]$city_name,
                        selected = location_data[location_data$city_type == "Metro", ]$city_name)
    } else if (selection_city_type == "Country") {
      updateSelectInput(session, "select_city", "Choose city name",
                        choices = location_data[location_data$city_type == "Country", ]$city_name,
                        selected = location_data[location_data$city_type == "Country", ]$city_name)
    } else if (selection_city_type == "Both") {
      updateSelectInput(session, "select_city", "Choose city name",
                        choices = location_data$city_name,
                        selected = location_data$city_name)
    }
    
    colour = NULL
    scale_factor = 0.25
    
    # adjust leaflet marker scales based on how many years chosen
    # this attempts to make markers not too small
    if (length(input$select_year) == 1){
      scale_factor = 1
    } else if (length(input$select_year) == 2){
      scale_factor = 0.6
    } else if (length(input$select_year) == 3){
      scale_factor = 0.4
    } else if (length(input$select_year) == 4){
      scale_factor = 0.3}
    
    # set colour for city type
    pal <- colorFactor(
      palette = c('#fb7537', '#3e8fc1'),
      domain = filtered_map_data()$city_type
    )
    
    # filter reactive map data from earlier based on city type
    # this is required to update proportional symbol map after initially created
    if (input$select_city_type != "Both") {
      filtered_map_data <- filtered_map_data() %>%
        filter(city_type == input$select_city_type)
    } else {
      filtered_map_data <- filtered_map_data()
    }
    
    # update proportional symbol map
    leafletProxy("mymap", data = filtered_map_data) %>%
      clearMarkers() %>%
      addCircleMarkers(~LONGITUDE,
                       ~LATITUDE,
                       radius = ~accident_count*scale_factor,
                       color = ~pal(city_type),
                       stroke = TRUE, 
                       fillOpacity = 0.5,
                       popup = ~paste("City:", city_name, "<br>", 
                                      "Number of Crashes:", accident_count, "<br>"),
                       label = ~paste(city_name, accident_count, "Crashes"))
    
    
    
    
  })
  
  
  
  
  # LINE CHART
  # create initial line chart
  output$myplot <- renderPlotly({
    
    p1 <- plot_ly(
      data = alcoholcrash_rbt_years,
      y = ~ accident_perc_change*100,
      x = ~ Year,
      name = "Number of \n Alcohol \n Related \n Accident \n",
      type = 'scatter',
      mode = 'lines + markers',
      color = '#a6611a',
      line = list(width = 2),
      text = ~paste("", round(accident_perc_change*100, 2), "%  |  ", "Alcohol Related Accidents \n"),
      hoverinfo = 'text'
    ) %>%
      add_trace(
        y = ~ positive_rbt_perc_change*100,
        text = ~paste("", round(positive_rbt_perc_change*100, 2), "%  |  ", "Positive RBT \n"),
        name = "Number of \n Positive RBT \n",
        color = '#dfc27d'
      ) %>%
      add_trace(
        y = ~ conducted_rbt_perc_change*100,
        text = ~paste("", round(conducted_rbt_perc_change*100, 2), "%  |  ", "Conducted RBT \n"),
        name = "Number of \n Total RBT",
        color = '#018571'
      )
    
    p1 %>%
      # hide plotly logo and remove buttons
      config(displaylogo = FALSE,
             modeBarButtons = list(list("resetViews"))) %>%
      # allow display grouped x values on hover
      layout(hovermode = "x unified",
             
             # prevent zoom on plot
             xaxis = list(title = "",
                          fixedrange = TRUE),
             yaxis = list(title = "Percentage Change",
                          fixedrange = TRUE)) 
    
  }
  )
  
  # PROPORTIONAL SYMBOL MAP
  # create initial proportional symbol map
  output$mymap <- renderLeaflet({
    
    scale_factor = 0.5
    pal <- colorFactor(
      palette = c('#EF8A62', '#67A9CF'),
      domain = filtered_map_data()$city_type
    )
    
    
    # add markers based on data
    leaflet(data = filtered_map_data()) %>% 
      addTiles() %>%
      addCircleMarkers(~LONGITUDE, 
                       ~LATITUDE, 
                       radius = ~accident_count*scale_factor,
                       color = ~pal(city_type),
                       popup = ~paste("<b>City:</b>", city_name, "<br>", 
                                      "<b>Number of Crashes:</b>", accident_count, "<br>"),
                       label = ~paste(city_name, accident_count, "Crashes")
      )
    
  })
  
  # BAR CHART
  # create initial bar chart based on reactive bar data
  # because bar chart is created AFTER proportional symbol map
  output$mybar <- renderPlotly({
    
    # initialise variable
    bar_colour = NULL
    
    if (input$select_city_type == "Country") {
      bar_colour = "#ef8a62"
    } else if (input$select_city_type == "Metro") {
      bar_colour = "#67a9cf"
    } else {
      bar_colour = "#998ec3"
    }
    
    # bar chart uses reactive bar data as it is created AFTER proportional symbol
    # map and selected years, city type and cities
    plot_ly(x = filtered_bar_data()$Month,
            y = filtered_bar_data()$Count,
            type = 'bar',
            color = I(bar_colour),
            marker = list(line = list(color = 'white',
                                      width = 1.5)),
            text = paste(' <b>Count</b>: ', filtered_bar_data()$Count, '\n',
                         '<b>Date</b>: ', filtered_bar_data()$Month, filtered_bar_data()$Year, '\n',
                         '<b>Yearly Total</b>: ', filtered_bar_data()$Total, '\n'),
            hoverinfo = 'text') %>%
      layout(yaxis = list(title = 'Count', 
                          fixedrange = TRUE),
             xaxis = list(fixedrange = TRUE),
             hovermode = "closest",
             hoverlabel=list(bgcolor="white")) %>%
      config(displaylogo = FALSE,
             modeBarButtons = list(list("resetViews")))
    
    
  })
  
  
  
}

