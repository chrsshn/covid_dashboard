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
                     start = "2020-10-10",
                     end = "2020-11-07",
                     min = "2020-08-19",
                     max   = "2020-11-07"),
      p ("The earliest date available is 2020-08-19 and latest date available is 2020-11-07"),
      br(),
      
      
      checkboxGroupInput("regionsavailable","Regions Available:",regions, selected = regions),
      p ("The 'combined' line is the combined 7 day incidence averages for Canton, Plymouth City, and Plymouth Township added together; it does not update for the specific regions selected"),
      
      
      br(),
      h4 ("Notes on Using the Plot:"),
      p ("-the plot will look the best when viewed in a full screen window"),
      p ("-to download the plot as a png, hover over the top right corner of the plot and click on the camera button"),
      p ("-to pan through dates, hover over the top right corner of the plot and click on the up-down-left-right arrows button"),
      p ("-to reset the plot, hover over the top right corner of the plot and click on the home button"),
      br(),
      br(),
      
      downloadButton("downloadData", "Download Data Used to Generate Plot"),
      textOutput("datelastupdated")
      # textOutput("test")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", 
                           plotlyOutput("distPlot")),
                  tabPanel("Upload Data", 
                           # htmlOutput("uploadpage"),
                           p ("This functionality is still under construction"),
                           fileInput("file1", "Choose CSV File",
                                     multiple = F,
                                     accept = c("text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv"))),
                  tabPanel("About Page", 
                           # htmlOutput("aboutpage"),
                           h2 ("About The Dashboard"),
                           p ("This dashboard was originially generated on a weekly basis to inform the Plymouth-Canton Community Community Schools on local rates COVID-19 cases."),
                           h2 ("About The Data"),
                           p("Data is derived from daily, municipality-level case reports from Wayne County Health Department. Data represent cases confirmed for each date (i.e., cases are associated with the date of confirmation, not date of symptom onset or specimen collection)."),
                           HTML("<p>For the COVID-19 cases in the state of Michigan, visit the  <a href='https://www.mistartmap.info/?mdoc=0&probable=1'>MI Safe Start Map</a></p>"),   
                           
                  h2 ("Contact"),
                  p ("Contact Dr. Emily Somers (emsomers@umich.edu) or Kaitlyn Akel (kbakel@umich.edu) for more information about the data used for the dashboard.")),
                  tabPanel("Help", 
                           # htmlOutput("helppage"),
                           h2 ("Contact"),
                           p("Contact Chris Shin (shincd@umich.edu) for questions related to the dashboard.")
                           )
                  )
      
      
      
    )
  )
)