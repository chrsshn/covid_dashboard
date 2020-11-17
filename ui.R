library (plotly)
library (shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("COVID-19 Incidence"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("daterange1", "Date range:",
                     start = Sys.Date() - 28,
                     end = Sys.Date(),
                     min = "2020-08-19",
                     max   = "2020-11-17"),
      
      
      downloadButton("downloadData", "Download"),
      textOutput("test")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot")
    )
    
  )
)