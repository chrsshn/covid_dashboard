library (shiny)
library (plotly)
library (shinydashboard)
library (shinyjs)
# library (DT)
library (markdown)

municipalities = c("Canton", 
                   "Combined (Canton and Plymouth)", 
                   "Plymouth City", 
                   "Plymouth Township")

incidence_calculations = c("7 day average", 
                           "15 day average", 
                           "28 day average")

surgeindicators = c("3 consecutive days of at least 10% increase in incidence",
                    "5 consecutive days of sustained increase in 7 day average incidence")

source ("getdata.R")

earliest_date = min (dat_all$date)
latest_date = max (dat_all$date)

ui_main_page <- dashboardPage(
  dashboardHeader(title = "COVID-19 Incidence",
                  tags$li (actionLink ("login", 
                                       label = "Login"),
                           class = "dropdown")
  ),
  dashboardSidebar(
    sidebarMenu(
      useShinyjs(),
      id = "tabs",
      menuItem("Home", tabName = "home"),
      hidden(menuItem ("News", tabName = "news")),
      menuItem ("About", tabName = "about"),
      menuItem ("Help", tabName = "help"),
      hidden(menuItem("Add Data", tabName = 'add_data')
      )
    )
  ),
  dashboardBody (
    tabItems (
      tabItem (tabName = "home",
               fluidRow (
                 box (title = "Parameters",
                      width = 3,
                      status = "primary",
                      dateRangeInput("daterange1", 
                                     "Date range:",
                                     start = latest_date - 28,
                                     end = latest_date,
                                     min = earliest_date,
                                     max   = latest_date),
                      p (paste0("The earliest date available is ",
                                earliest_date, 
                                " and latest date available is ", 
                                latest_date)),
                      br(),
                      checkboxGroupInput("municipalitiesavailable",
                                         "Municipalities Available:",
                                         municipalities, 
                                         selected = municipalities),
                      br(),
                      selectInput ("calculatedincidence", "Incidence Metric:",
                                   choices = incidence_calculations,
                                   multiple = F,
                                   selected = "7 day average"),
                      br(),
                      checkboxGroupInput("show_surge",
                                         "Show Surge Indicators:",
                                         surgeindicators, 
                                         selected = surgeindicators),
                      br(),
                      h4 ("Notes on interacting with the plot:"),
                      p ("-horizontal lines correspond to region risk levels designated by the Michigan Department of Health and Human Services"),
                      p ("-to download the plot as a png, hover over the top right corner of the plot and click on the camera button ('Download plot as a png')"),
                      p ("-to pan through dates, hover over the top right corner of the plot and click on the up-down-left-right arrows button ('Pan')"),
                      p ("-to reset the plot, hover over the top right corner of the plot and click on the home button ('Reset axes')"),

                      br(),
                      br(),
                      downloadButton("downloadData", "Download Data Used to Generate Plot")
                 ),
                 
                 box (
                   width = 9,
                   plotlyOutput("distPlot")
                 )
               )
      ),
      tabItem (tabName = "news",
               fluidRow(
                 box (
                   width = 7,
                      includeMarkdown("notes.md")),
                 box (width = 4,
                      title = "Upcoming Pop-Up Testing Sites in Wayne County",
                      p("Inkster, Dozier Recreation Center: November 14 & 15"),
                      p("Woodhaven: November 21 & 22"))
               
               )
               ),
      tabItem (tabName = "about",
               h2 ("About The Data"),
               p("Data is derived from daily, municipality-level case reports from Wayne County Health Department. Data represent cases confirmed for each date (i.e., cases are associated with the date of confirmation, not date of symptom onset or specimen collection)."),
               HTML("<p>For COVID-19 indicators across the state of Michigan, visit the  <a href='https://www.mistartmap.info/?mdoc=0&probable=1'>MI Safe Start Map</a>.</p>"),   
               h2 ("Contact"),
               HTML ("<p>Contact <a href='https://ihpi.umich.edu/our-experts/emsomers'>Dr. Emily Somers</a> (emsomers@umich.edu) or Kaitlyn Akel (kbakel@umich.edu) for more information about the data used for the dashboard.")),
      tabItem (tabName = "help",
               h2 ("Contact"),
               p("Contact Chris Shin (shincd@umich.edu) for technical help related to the dashboard.")
      ),
      tabItem (tabName = "add_data",
               
               box (
                 width = 8,
                 HTML("<p>To add new data points, add them to the google sheet  <a href='https://docs.google.com/spreadsheets/d/1_BWCAtqFdap8giAtqvZLqVcmPj_MkXUt3Dnge4NGgyk/edit?usp=sharing'>here</a></p>")
               )
      )
    )
    
  )


)



