
library(shiny)
library (tidyverse)
library (plotly)

source ("getdata.R")



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
    
      plot_ly (data = refreshdat(), 
               x = ~date, 
               y = ~count, 
               type = "scatter", 
               mode = "lines", 
               linetype = ~region, 
               linetypes = c('dash','dot','dotdash','solid'),
               color = ~region,
               colors = c('#e90003','#1B9E77','#7570B3','#AAAAAA'),
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
      paste("cantonplymouth.csv", sep = "")
    },
    content = function(file) {
      todownload = as.data.frame(refreshdat())%>% 
        pivot_wider (
          id_cols = "date",
          names_from = "region",
          values_from = "count"
        )
      
      write.csv(todownload, file, row.names = FALSE)
    }
  )
  
  output$test <- renderPrint ({
    # refreshdat()
    
    
  })

    output$datelastupdated <- renderText ({
    "Last Updated 2020-11-15"
    
    
  })
  output$aboutpage <- renderUI ({
      str1 <- paste("about the project")
      str2 <- paste("contact info")
      HTML(paste(str1, str2, sep = '<br/>'))
    
    
  })
  output$helppage <- renderUI ({
      str1 <- paste("FAQs")
      str2 <- paste("troubleshooting")
      HTML(paste(str1, str2, sep = '<br/>'))
    
    
  })

  
  
}