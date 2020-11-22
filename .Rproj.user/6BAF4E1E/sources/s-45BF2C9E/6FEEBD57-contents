library(shiny)
library (tidyverse)
library (plotly)
logged <- F




server <- function(input, output) {
  
  source ("getdata.R")
  source ("ui_add_data_page.R")
  
  
  

  calculate_7day_incidences <- reactive ({
    
    
    
    
  })
  
  selected_data_points <- reactive ({
    req(input$daterange1)
    dat_7dayincidences %>%
      filter (date <= as.Date(input$daterange1[2]) & date >= as.Date(input$daterange1[1]),
              as.character (region) %in% input$regionsavailable)

    
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
    
    l <- list(
      # bordercolor = "#111111",
      borderwidth = 1,
      # x = 0.08, 
      y = .5,
      title=list(text='<b> Municipality </b>'))
      
      date_range_print <- list(
        text = paste ("Dates:",as.Date(input$daterange1[1]),"to", as.Date(input$daterange1[2])),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.585,
        y = 1.03,
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
        x = 0.585,
        y = .98,
        showarrow = FALSE
      )
    
      #get min value and choose which hlines to show
      plot_ly (data = selected_data_points(), 
               x = ~date, 
               y = ~count, 
               name = ~as.factor(region),
               type = "scatter", 
               mode = "lines", 
               linetype = ~I(linetype),
               # linetypes = c('dash','dot','dotdash','solid'),
               color = ~I(color),
               # colors = c('#e90003','#1B9E77','#7570B3','#AAAAAA'),
               yaxis = list (title = 'Incidence\n7-day average (cases/million/day)')) %>%
      layout (shapes = list( hline(7, "bed4e6"),
                             hline(20, "a0bddc"),
                             hline(40, "8b96c9"), 
                             hline (70, "8d6cb0"), 
                             hline (150, "8a419e")),
              xaxis = list(title = "Date"), 
              yaxis = list(title = "Incidence\n7-day average (cases/million/day)",
                           autotick = F,
                           dtick = 50),
              title =  "Plymouth & Canton Incidence",
              annotations = list(date_range_print, time_frame_print),
              legend = l,
              showlegend = T,
              # images = list(
              #   source = base64enc::dataURI(file = "risk_levels_with_values.png"),
              #   x = .8, y = .8, 
              #   sizex = 1, sizey = 1,
              #   xref = "x", yref = "y",
              #   xanchor = "left", yanchor = "bottom",
              #   sizing = "stretch"
              # ),
              margin = list(t = 60),
              images = list (
                source = "https://github.com/chrsshn/covid_dashboard/blob/main/www/risk_levels_with_values.png?raw=true",
                xref = "paper",
                yref = "paper",
                x = .5,
                y = .5,
                sizex = 90,
                sizey = 90,
                sizing = "stretch",
                opacity = .9
              )
            
              )
              
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cantonplymouth.csv", sep = "")
    },
    content = function(file) {
      todownload = as.data.frame(selected_data_points())%>% 
        pivot_wider (
          id_cols = "date",
          names_from = "region",
          values_from = "count"
        )
      
      write.csv(todownload, file, row.names = FALSE)
    }
  )
  
  output$test <- renderPrint ({
    str (selected_data_points())
    
    
  })
  
  output$checkpassword <- renderText({
    req(input$go)
    isolate(input$password)
  })

    output$datelastupdated <- renderText ({
    paste ("The data was last updated on 2020-11-07")
    
    
  })
  output$aboutpage <- renderUI ({
    
      str1 <- paste("Data is derived from daily, municipality-level case reports from Wayne County Health Department. Data represent cases confirmed for each date (i.e., cases are associated with the date of confirmation, not date of symptom onset or specimen collection).")
      str2 <- paste("Contact Dr. Emily Somers (emsomers@umich.edu) or Kaitlyn Akel (kbakel@umich.edu) for more information about the project")
      HTML(paste(str1, str2, sep = '<br/>'))
    
    
  })
  output$uploadpage <- renderUI ({
      
    
    
  })
  output$helppage <- renderUI ({
      str1 <- paste("FAQs coming soon")
      str2 <- paste("Contact Chris Shin (shincd@umich.edu) for technical help with the dashboad")
      HTML(paste(str1, str2, sep = '<br/>'))
    
    
  })

  
  
}