library (shiny)
library (plotly)
library (shinydashboard)
library (shinyjs)

municipalities = c("Canton", "Combined (Canton and Plymouth)", "Plymouth City", "Plymouth Township")


ui_main_page <- dashboardPage(
  
  dashboardHeader(title = "COVID-19 Incidence",
                  
                  tags$li (actionLink ("login", label = "Login"),
                           class = "dropdown")),
  
  dashboardSidebar(
    sidebarMenu(
      useShinyjs(),
      id = "tabs",
      menuItem("Home", tabName = "home"),
      menuItem ("About", tabName = "about"),
      menuItem ("Help", tabName = "help"),
      hidden(menuItem("Add Data", tabName = 'add_data'))
    )
  ),
  dashboardBody (
    tabItems (
      tabItem (tabName = "home",
               fluidRow (
                 
                 
                 box (title = "Parameters",
                      width = 4,
                      status = "primary",
                      dateRangeInput("daterange1", "Date range:",
                                     start = "2020-10-10",
                                     end = "2020-11-07",
                                     min = "2020-08-19",
                                     max   = "2020-11-07"),
                      p ("The earliest date available is 2020-08-19 and latest date available is 2020-11-07"),
                      br(),
                      checkboxGroupInput("municipalitiesavailable","Municipalities Available:",municipalities, selected = municipalities),
                      br(),
                      h4 ("Notes on Using the Plot:"),
                      p ("-horizontal lines correspond to region risk levels designated by the Michigan Department of Health and Human Services"),
                      p ("-to download the plot as a png, hover over the top right corner of the plot and click on the camera button ('Download plot as a png')"),
                      p ("-to pan through dates, hover over the top right corner of the plot and click on the up-down-left-right arrows button ('Pan')"),
                      p ("-to reset the plot, hover over the top right corner of the plot and click on the home button ('Reset axes')"),
                      br(),
                      br(),
                      downloadButton("downloadData", "Download Data Used to Generate Plot"),
                      textOutput("datelastupdated")
                      # textOutput("test")
                 ),
                 
                 box (
                   width = 8,
                   plotlyOutput("distPlot")
                 )
               )
      ),
      tabItem (tabName = "about",
               h2 ("About The Data"),
               p("Data is derived from daily, municipality-level case reports from Wayne County Health Department. Data represent cases confirmed for each date (i.e., cases are associated with the date of confirmation, not date of symptom onset or specimen collection)."),
               HTML("<p>For the COVID-19 cases in the state of Michigan, visit the  <a href='https://www.mistartmap.info/?mdoc=0&probable=1'>MI Safe Start Map</a></p>"),   
               
               h2 ("Contact"),
               p ("Contact Dr. Emily Somers (emsomers@umich.edu) or Kaitlyn Akel (kbakel@umich.edu) for more information about the data used for the dashboard.")),
      tabItem (tabName = "help",
               h2 ("Contact"),
               p("Contact Chris Shin (shincd@umich.edu) for questions related to the dashboard."))
    )
  )
)


