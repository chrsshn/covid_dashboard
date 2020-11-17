library (plotly)
library (shiny)

regions = c("Canton", "Plymouth City", "Plymouth Township", "Combined")
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
      checkboxGroupInput("regionsavailable","Regions Available:",regions, selected = regions),
      
      
      downloadButton("downloadData", "Download"),
      # textOutput("test")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotlyOutput("distPlot")),
                  tabPanel("About Page", htmlOutput("aboutpage")),
                  tabPanel("Help", htmlOutput("helppage"))),
      textOutput("datelastupdated")
      
      
      
    )
  )
)