#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggmap)
library(ggplot2)
library(mapdata)
library(maps)
library(shinyWidgets)

data<- read_csv("/home/gregory/kr/testdataMakeup.csv")
m = map_data('worldHires')

countries = m %>%
  distinct(region)

korea = m %>%
  filter(region %in% c('South Korea', 'North Korea'))

RATIO = 1


 # Define UI for application that draws a histogram
 ui <- fluidPage(

   # Application title
   titlePanel("Korea1"),

   # Sidebar with a date range input
   sidebarLayout(
      sidebarPanel(
        
        #Time range Slider Input
        sliderInput("datetime",
                    label = "Time range",
                    min = as.POSIXct("2018-04-14 12:00:00"),
                    max = as.POSIXct("2018-04-30 14:00:00"),
                    value = c(
                      as.POSIXct("2018-04-14 12:00:00"),
                      as.POSIXct("2018-04-30 14:00:00")
                    )
        ),
        #actionButton("update", "Update range")
        pickerInput(inputId = "goalPicker",
                    label = "select goals",
                    choices = unique(data$goal),
                    options = list(
                      `actions-box` = TRUE,
                      size = 12,
                      `selected-text-format` = "count >3"
                    ),
                    selected = unique(data$goal),
                    multiple =TRUE
                    ),
        pickerInput(inputId = "typePicker",
                    label = "select types",
                    choices = unique(data$type),
                    options = list(
                      `actions-box` = TRUE,
                      size = 12,
                      `selected-text-format` = "count >3"
                    ),
                    selected = unique(data$type),
                    multiple =TRUE
        )
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput(outputId = "scatterplot")
        
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data_subset <- reactive({
    #req(input$datetime)
    #req(input$goalPicker)
    #req(input$typePicker)
    data %>% filter((goal %in% input$goalPicker| 
                      type %in% input$typePicker) &  
                      (date >= input$datetime[1] & 
                         date <= input$datetime[2])
                    )
                      
    
    
  })
  
  output$scatterplot <- renderPlot({
   ggplot(data = data_subset(), 
          aes(x =long, y =lat)) + 
    geom_point() +
    geom_polygon(data = korea,
          aes(x=long, y =lat, group = group),
          fill = 'white', color = 'black') + 
    coord_fixed(RATIO)
  })
  
  
  output$from <- renderText(input$datetime[1])
  output$to <- renderText(input$datetime[2])
  observeEvent(input$update, {
    updateSliderInput(session, "timeRange", 
      value = c(
        as.POSIXct("2018-04-25 12:00:00"),
        as.POSIXct("2018-04-29 14:00:00"))
    )
  })


}

# Run the application
shinyApp(ui = ui, server = server)

