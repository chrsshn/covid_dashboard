
library(shiny)
library (tidyverse)
library (plotly)

# source ("getdata.R")
dat = readr::read_csv("temp_covid_cases.csv",
                      col_names = c("date",
                                    "canton",
                                    "inc_canton",
                                    "7dayinc_canton",
                                    "plymouth city",
                                    "inc_plycity",
                                    "7dayinc_plycity",
                                    "plymouth twsp",
                                    "inc_plytown",
                                    "7dayinc_plytown",
                                    "cases_combo",
                                    "inc_combo",
                                    "7dayinc_combo" ),
                      col_types = "cdddddddddddd",
                      skip=1,
                      na = ".") %>%
  mutate (date = as.Date(date, format = c("%m/%d"))) %>%
  select (date, '7dayinc_canton', '7dayinc_plycity','7dayinc_plytown')%>%
  pivot_longer (!date, names_to = "region", values_to = "count") 



server <- function(input, output) {
  refreshdat <- reactive ({
    req(input$daterange1)
    dat %>%
      filter (date <= as.Date(input$daterange1[2]) & date >= as.Date(input$daterange1[1]))
    
  }) 
  
  output$distPlot <- renderPlotly({
    
    
    
    hline <- function(y = 0, color = "blue") {
      list(
        type = "line", 
        x0 = 0, 
        x1 = 1, 
        xref = "paper",
        y0 = y, 
        y1 = y, 
        line = list(color = color)
      )
    }
      
      date_range_print <- list(
        text = paste ("Dates:",as.Date(input$daterange1[1]),"to", as.Date(input$daterange1[2])),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.565,
        y = .96,
        showarrow = FALSE
      )
    
      time_frame <- round ((as.Date(input$daterange1[2]) - as.Date(input$daterange1[1])) / 7, 1)
      time_frame_print <- list(
        text = paste ("Time Frame:",time_frame,"Weeks"),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.564,
        y = .9,
        showarrow = FALSE
      )
    
    plot_ly (data = refreshdat(), x = ~date, y = ~count, type = "scatter", mode = "lines", linetype = ~region,
             line = list (color = c('#e90003','#1B9E77','#7570B3')),
             yaxis = list (title = 'Incidence\n7-day average (cases/million/day)')) %>%
      layout (shapes = list( hline(40, "8b96c9"), hline (70, "8d6cb0"), hline (150, "8a419e")),
              xaxis = list(title = "Date"), 
              yaxis = list(title = "Incidence\n7-day average (cases/million/day)"),
              title =  "Plymouth & Canton Incidence",
              annotations = list(date_range_print, time_frame_print)
              
      )
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  output$test <- renderPrint ({
    # refreshdat()
    
    
  })
  
  
}