
# Author: Nelson Luu
# Date Created: 21/10/2020
# Last Modified: 13/02/2021
# R Version: 4.0.2

# DISCLAIMER:
# USE version 4.0.2
# MAY NOT WORK ON OLDER VERSION e.g. 3.6.3

# Personal Notes:
# first attempt during mid semester break (22/09/2020)

# Description:
# This application creates narrative visualisation based 
# on random breath testing and alcohol related crashes in
# Victoria. The purpose is to present 4 key findings and
# allow users to perform their own data exploration.
# It is highly interactive with filters, selection inputs 
# to customise the plots. 


# ========================================================
# ========================================================
#            IMPORT LIBRARIES
# ========================================================
# ========================================================

# install.packages("shiny")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("plotly")
# install.packages("leaflet")
# install.packages("shinyWidgets")
# install.packages("shinyjqui")

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(shinyWidgets)
library(shinyjqui)


# ========================================================
# ========================================================
#            PRELIMINARY DATA PROCESSING
# ========================================================
# ========================================================


# write.csv(crash_data, "crash_data.csv", row.names = FALSE)
# write.csv(alcoholcrash_rbt_years, "alcoholcrash_rbt_years.csv", row.names = FALSE)
# write.csv(alcoholcrash_years, "alcoholcrash_years.csv", row.names = FALSE)
# write.csv(crash_rbt_data, "crash_rbt_data.csv", row.names = FALSE)
# write.csv(location_data, "location_data.csv", row.names = FALSE)
# write.csv(rbt_data, "rbt_data.csv", row.names = FALSE)

crash_data <- read_csv("crash_data.csv")
alcoholcrash_rbt_years <- read_csv("alcoholcrash_rbt_years.csv")
alcoholcrash_years <- read_csv("alcoholcrash_years.csv")
crash_rbt_data <- read_csv("crash_rbt_data.csv")
location_data <- read_csv("location_data.csv")
rbt_data <- read_csv("rbt_data.csv")

# ========================================================
# ========================================================
#                         UI CODE
# ========================================================
# ========================================================

ui <- fluidPage(
  # add light sky blue background
  setBackgroundColor("#ecf4f9"),
  
  # add scroll bar to select city input box
  tags$head(tags$style(HTML('.selectize-input {height: 100px; overflow-y: scroll;}'),
                       HTML('.shiny-notification {opacity: 0.97;}'))),
  
  # add title and description
  headerPanel(HTML("Random Breath Testing (RBT) and Alcohol Crash Related Accidents in Victoria")),
  HTML("Author: Nelson Luu <br> 
  Last Modified: 13/02/2021 <br> <br>"),
  HTML("This visualisation attempts to present the findings discovered by the author on Random Breath Tests
       and Alcohol Crash Related Accidents. With the motivation to improve road safety, 4 key findings 
       and their recommendations on how to improve Random Breath Testing in Victoria are presented in this 
       visualisation.<br> <br>
       Please begin by going through findings 1 to 4."),
  HTML("<br>"),
  
  # create section for line chart
  titlePanel("Percentage Change in of RBT against Alcohol Accidents"),
  sidebarLayout(position = "right",
                sidebarPanel(br(HTML("<b>Adjusting Number of RBT: </b>")), 
                             actionButton("f1_button", 
                                          HTML("Finding 1")), br(), br(HTML("<b>Allocation of RBT within Regional Victoria: </b>")),
                             actionButton("f2_button", 
                                          HTML("Finding 2")), br(), br(HTML("<b>Allocation of RBT within Melbourne Metro: </b>")),
                             actionButton("f3_button", 
                                          HTML("Finding 3")), br(), br(HTML("<b>Seasonal Trends: </b>")),
                             actionButton("f4_button", 
                                          HTML("Finding 4"))),
                mainPanel(plotlyOutput("myplot"))),
  
  # create section for proportional symbol map
  titlePanel("Alcohol Crash Accidents by Cities"),
  sidebarLayout(position = "right",
                sidebarPanel(radioButtons("select_city_type",
                                          label = "Choose City Type",
                                          choiceValues = c("Both", unique(location_data$city_type)),
                                          choiceNames = c("Both", "Regional", "Metro"),
                                          selected = "Both"),
                             checkboxGroupInput("select_year",
                                                label = "Choose Year",
                                                choices = unique(alcoholcrash_rbt_years$Year),
                                                selected = unique(alcoholcrash_rbt_years$Year)),
                             actionButton("show_year_button", "Show All Years"),
                             br(), br(),
                             actionButton("clear_year_button", "Clear All Years")),
                
                mainPanel(leafletOutput("mymap"))),
  
  # create section for bar chart 
  titlePanel("Accident Counts By Months"),
  sidebarLayout(position = "right",
                sidebarPanel(selectizeInput("select_city",
                                         label = "Choose City Name",
                                         choices = unique(location_data$city_name),
                                         multiple = TRUE,
                                         options = list(placeholder = 'Pick a City!')),
                             actionButton("show_cities_button", "Show All Cities"),
                             br(), br(),
                             actionButton("clear_cities_button", "Clear All Cities"),
                             br(), br(),
                             actionButton("reset_all_button", "RESET ALL"),
                             br(), br(),
                             actionButton("help_button", "Click for Help")),
                mainPanel(plotlyOutput("mybar"))),
  
  # include reference to data source
  HTML("
  <br><br>
  <b>Interactive Charts Powered by:</b>
  <ul>
    <li>
      <a href=https://rstudio.github.io/leaflet/>Plotly</a>
    </li>
    <li>
      <a href=https://plotly.com/r/>Leaflet</a>
    </li>
  </ul>
  
  <b>Dataset Sources:</b> <br>
  
  <a href=https://discover.data.vic.gov.au/dataset/crashes-last-five-years1>
    https://discover.data.vic.gov.au/dataset/crashes-last-five-years1
  </a>
  <br>
   <a href=https://data.gov.au/dataset/ds-dga-a814a8c5-ef57-463c-9c8b-a6e625bfb860/details>
    https://data.gov.au/dataset/ds-dga-a814a8c5-ef57-463c-9c8b-a6e625bfb860/details
  </a> 
  <br> <br>
  
       ")
  
                
  )



