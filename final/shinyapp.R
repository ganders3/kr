library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggmap)
library(ggplot2)
library(mapdata)
library(maps)
library(shinyWidgets)
library(DT)

#==================select data source================
data = data
#=============(=======================================

#=================set controls========================
RATIO = 1
#=====================================================
Sys.setenv(TZ = 'Asia/Seoul')
m = map_data('worldHires')

countries = m %>%
  distinct(region)

korea = m %>%
  filter(region %in% c('South Korea', 'North Korea'))

# Define UI for application that draws a histogram
ui = fluidPage(
  
  # Application title
  titlePanel('Korea1'),
  
  # Sidebar with data range input
  sidebarLayout(
    sidebarPanel(
      
      # Time range slider input
      sliderInput('datetime',
                  label = 'Time range',
                  min = as.POSIXct(min(data$Time)),
                  max = as.POSIXct(max(data$Time)),
                  value = c(
                    as.POSIXct(min(data$Time)),
                    as.POSIXct(max(data$Time))
                    )
                  ),
      
      pickerInput(inputId = 'typePicker',
                  label = 'select types',
                  choices = unique(data$Type),
                  options = list(
                    `actions-box` = T,
                    size = 12,
                    `selected-text-format` = 'count >3'
                  ),
                  selected = unique(data$Type),
                  multiple = T
      )
    )
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput(outputId = 'scallterplot', height = 800),
    # DT::dataTableOutput(outputId = 'datatable'),
    imageOutput('weather')
  )
)

# Define server logic required to draw histogram
server = function(input, output) {
  
  data_subset = reactive({
    #req(input$datetime)
    #req(input$goalPicker)
    #req(input$typePicker)
    
    data %>% filter(
      #goal %in% input$goalPicker|
      Type %in% input%typePicker &
        (as.POSIXct(Time) >= input$datetime[1] &
           as.POSIXct(Time) <= input$datetime[2])
    )
  })
  
  # Create data table
  output$datatable = renderDataTable({
    DT::datatable(data = data_subset(),
                  options = list(pageLength = 10),
                  rownames = F)
  })
  
  output$scatterplot = renderPlot({
    ggplot() +
      ggtitle('SAM Chart') +
      geom_polygon(data = korea,
                   aes(x = long, y = lat, group = group),
                   fill = 'white', color = 'black') +
      geom_point(data = sams,
                 aes(x = LONGITUDE, y = LATITUDE, shape = WEAPONS, col = STAT, fill = MOVED),
                 size =  7) +
      scale_shape_manual() +
      scale_color_manual() +
      scale_fill_manual(values = c('no' = 'NA', 'yes' = 'yellow'), guide = F) +
      geom_point(data = data_subset(),
                 aes(x = Long, y = Lat),
                 col = 'red', shape = 4) +
      # geom_text(data = data, aes(x = LONG + 0.05, y = LAT, label = NAME), size = 3, hjust = 0) +
      coord_fixed(RATIO)
  })
  
  output$from = renderText(input$datetime[1])
  output$to = renderText(input$datetime[2])
  observeEvent(input$update, {
    updateSliderInput(session, 'timeRange')
  })
  
  output$weather = renderImage({
    file.path('link to image')
  },
  deleteFile = F)
}

# Run the application
shinyApp(ui = ui, server = server)