library(shiny)
library (dplyr)
library (plotly)
library (DT)



server <- function(input, output, session) {
  
  all_cases <- reactiveValues (a = dat_cases_only)
  
  all_data_points <- reactiveValues (a = dat_all)
  
  selected_data_points <- reactiveValues (a = dat_selected)
  
  selected_3day_surge <- reactiveValues (a = dat_3day_surge)
  
  selected_5day_surge <- reactiveValues (a = dat_5day_surge)
  
  
  
  update_selected_data_points <- reactive ({
    incidence_type = case_when (
      input$calculatedincidence == "7 day average" ~ "7day",
      input$calculatedincidence == "15 day average" ~ "15day",
      input$calculatedincidence == "28 day average" ~ "28day",
    )
    
    selected_data_points$a <- all_data_points$a %>%
      filter (date <= as.Date(input$daterange1[2]) & date >= as.Date(input$daterange1[1]),
              as.character (municipality) %in% input$municipalitiesavailable,
              measure == incidence_type)
    
    if ("3 consecutive days of at least 10% increase in incidence" %in% input$show_surge ) {
      selected_3day_surge$a <- all_data_points$a %>%
        filter (date <= as.Date(input$daterange1[2]) & date >=as.Date(input$daterange1[1]),
                as.character (municipality) %in% input$municipalitiesavailable,
                is_3day_surge == 1,
                measure == incidence_type)
    }
    else
      selected_3day_surge$a <- all_data_points$a %>% 
      filter (is_3day_surge == 3)
    
    
    if ("5 consecutive days of sustained increase in 7 day average incidence" %in% input$show_surge ) {
      selected_5day_surge$a <- all_data_points$a %>%
        filter (date <= as.Date(input$daterange1[2]) & date >=as.Date(input$daterange1[1]),
                as.character (municipality) %in% input$municipalitiesavailable,
                is_5day_surge == 1,
                measure == incidence_type)
    }
    else
      selected_3day_surge$a <- all_data_points$a %>% 
      filter (is_5day_surge == 3)
    
  })
  
  output$distPlot2 <- renderPlotly ({
    plot_ly(data = selected_3day_surge$a,
            x = ~date,
            y = ~value,
            type = "scatter",
            mode = "markers",
            color = ~I(color),
            size = 2   ) 
    
  })
  output$distPlot <- renderPlotly({
    update_selected_data_points()
    
    
    
    add_hline <- function(y = 0, color = "blue") {
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
    
    hlines_print <- list( 
      add_hline(7, "a0bddc"),
      add_hline(20, "8b96c9"),
      add_hline(40, "8d6cb0"), 
      add_hline (70, "8a419e"), 
      add_hline (150, "6c1769"))
    
    
    modify_legend <- list(
      borderwidth = 1,
      y = .5,
      title=list(text='<b> Municipality </b>',
                 x = 0.5))
    
    date_range_print <- list(
      text = paste ("Dates:",as.Date(input$daterange1[1]),"to", as.Date(input$daterange1[2])),
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1.04,
      showarrow = FALSE
    )
    
    time_frame <- round ((as.Date(input$daterange1[2]) - as.Date(input$daterange1[1])) / 7, 1)
    time_frame_message <- ifelse (time_frame != 1, 
                                  paste ("Time Frame:",time_frame,"Weeks"),
                                  paste ("Time Frame:",time_frame,"Week")
                                  )
                                  
    time_frame_print <- list(
      text = time_frame_message,
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = .985,
      showarrow = FALSE
    )
    
    risk_levels_print <- list(
      source = base64enc::dataURI(file = "risk_levels.png"),
      x = .5, y = -0.356,
      sizex = 1, sizey = 1,
      xref = "paper", yref = "paper",
      xanchor = "center", yanchor = "bottom"
    )
    
    plot_ly () %>%
      add_trace(data = selected_data_points$a, 
                x = ~date, 
                y = ~value, 
                name = ~as.factor(municipality),
                type = "scatter", 
                mode = "lines", 
                hoverinfo = 'text',
                text = ~paste0 (municipality, " Incidence for ", date, ": ", value),
                linetype = ~I(linetype),
                color = ~I(color),
                yaxis = list (title = 'Incidence\n7-day average (cases/million/day)'))   %>%
      add_trace(data = selected_3day_surge$a,
                x = ~date,
                y = ~value,
                name = "3 Day Surge",
                type = "scatter",
                mode = "markers",
                hoverinfo = 'text',
                text = ~paste0("Change in ",municipality, " Incidence: ", consecutive_percent_increase_in_incidence),
                opacity = .8,
                marker = list (
                  color = "FEE12B",
                  size = 15
                ))    %>%
      add_trace(data = selected_5day_surge$a,
                x = ~date,
                y = ~value,
                name = "5 Day Surge",
                type = "scatter",
                mode = "markers",
                hoverinfo = 'text',
                text = ~paste0("Consecutive Increases in 7 Day Incidence: ", consecutive_increase_in_7day),
                opacity = .8,
                marker = list (
                  color = "FBB117",
                  size = 15
                ))    %>%
      
      layout (shapes = hlines_print,
              xaxis = list(title = "Date"), 
              yaxis = list(title = paste0 ("Incidence\n",input$calculatedincidence," (cases/million/day)"),
                           autotick = F,
                           dtick = 50),
              title =  list (text = "Plymouth & Canton Incidence", xref = 'paper',x = .5),
              annotations = list(date_range_print, time_frame_print),
              legend = modify_legend,
              showlegend = T,
              images = risk_levels_print,
              hovermode = "x unified",
              margin = list(t = 60,
                            b = 90
              )
      ) %>%
      config (modeBarButtonsToRemove = list ("toggleSpikelines",
                                             "select2d",
                                             "lasso2d"))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(format (Sys.Date(), "%Y_%m_%d_"), 
            "cantonplymouth_",
            str_replace_all (input$calculatedincidence, " ", "_" ),
            ".csv", 
            sep = "")
    },
    content = function(file) {
      todownload = as.data.frame(selected_data_points())%>% 
        pivot_wider (
          id_cols = "date",
          names_from = "municipality",
          values_from = "value"
        )
      write.csv(todownload, file, row.names = FALSE)
    }
  )
  
  
  output$datelastupdated <- renderText ({
    paste ("The data was last updated on 2020-11-07")
  })
  
  
  observeEvent(input$login, {
    showModal(modalDialog(
      passwordInput ("password", "Enter password"),
      actionButton ("submit_password", "Submit")
    )
    )
  })
  
  
  observeEvent(input$submit_password,{
    if(input$password == "canton"){
      showModal(modalDialog(
        p("The 'News' and Add data' tabs are now accessible"),
        show(selector = "ul li:eq(2)"),
        show(selector = "ul li:eq(5)")
      ))
    } else {
      showModal (modalDialog(p ("Incorrect password"),
                             hide(selector = "ul li:eq(2)"),
                             hide(selector = "ul li:eq(5)")))
      
    }
  })
  
  
}