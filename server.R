library(shiny)
library (dplyr)
library (plotly)
library (DT)



server <- function(input, output, session) {
  
  all_cases <- reactiveValues (a = dat_cases_only)
  
  all_data_points <- reactiveValues (a = dat_all)
  
  update_all_data_points <- function() {
    
    cases_temp = rbind (all_cases$a, 
                        data.frame (date = as.Date(input$newdate, format = c("%Y-%m-%d")),
                                    canton_cases = input$newcasecountcanton,
                                    plymouthcity_cases = input$newcasecountplymouthcity,
                                    plymouthtownship_cases = input$newcasecountplymouthtownship))
    all_temp <- recalculate_incidences (cases_temp)
    all_data_points$a <- all_temp
    
    
  }
  
  update_all_data_points <- eventReactive(input$add_new_data_point, {
    cases_temp = rbind (all_cases$a, 
                        data.frame (date = as.Date(input$newdate, format = c("%Y-%m-%d")),
                                    canton_cases = input$newcasecountcanton,
                                    plymouthcity_cases = input$newcasecountplymouthcity,
                                    plymouthtownship_cases = input$newcasecountplymouthtownship))
    all_temp <- recalculate_incidences (cases_temp)
     all_temp
    
    
  })
  
  
  
  output$see_new_values <- renderText({
    str (all_data_points$a)
  }
  )
  
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
    
  })
  
  selected_data_points <- reactiveValues (a = dat_selected)
  
  output$available_points <- renderDataTable({
    
    update_all_data_points() %>%
      select (date, municipality, measure, value) %>%
      pivot_wider (id_cols = c("date", "municipality"),
                   names_from ="measure",
                   values_from = "value",
                   values_fill = NA)
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
    
    time_frame_print <- list(
      text = paste ("Time Frame:",time_frame,"Weeks"),
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
    
    plot_ly (data = selected_data_points$a, 
             x = ~date, 
             y = ~value, 
             name = ~as.factor(municipality),
             type = "scatter", 
             mode = "lines", 
             linetype = ~I(linetype),
             color = ~I(color),
             yaxis = list (title = 'Incidence\n7-day average (cases/million/day)')) %>%
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
              margin = list(t = 60,
                            b = 90)
      )
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
        p("The 'Add data' tab is now accessible"),
        show(selector = "ul li:eq(4)")
      ))
    } else {
      showModal (modalDialog(p ("Incorrect password"),
                             hide(selector = "ul li:eq(4)") ))
      
    }
  })
  
  
}