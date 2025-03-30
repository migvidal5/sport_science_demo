server <- function(input, output, session) {
  
  team_overview <- reactive({
    team_overview <- combined_data %>% 
      filter(date %in% input$dl_date)
  })
  
  player_load_variable <- reactive({
    player_load_melt <- acwr_long %>% 
      filter(player %in% input$pl_player & variable %in% input$pl_variable) 
  })
  
  wellness_overview <- reactive({
    wellness_overview <- wellness_data %>% 
      filter(player %in% input$well_player & date %in% input$well_date) 
  })
  
  sleep_overview <- reactive({
    sleep_data %>%
      filter(player %in% input$sleep_player) %>%
      arrange(player, date) %>%
      group_by(player) %>%
      mutate(
        # Check if resting HR increased over 3 consecutive days
        hr_increase_3d = lag(resting_hr, 2) < lag(resting_hr, 1) & lag(resting_hr, 1) < resting_hr,
        
        # Check if HRV increased over 3 consecutive days
        hrv_increase_3d = lag(hrv, 2) < lag(hrv, 1) & lag(hrv, 1) < hrv,
        
        # Check if HRV decreased over 3 consecutive days
        hrv_decrease_3d = lag(hrv, 2) > lag(hrv, 1) & lag(hrv, 1) > hrv
      ) %>%
      ungroup()
  })
  
  
  
  
  output$team_table <- renderReactable({
    df <- team_overview()
    
    # Define the columns that have ACWR equivalents
    acwr_vars <- c("total_distance", "hsr_distance", "sprint_distance", "accels", "decels")
    
    # Build the conditional column definitions
    col_defs <- lapply(names(df)[1:14], function(col) {
      if (col %in% acwr_vars) {
        acwr_col <- paste0("acwr7_", col)
        colDef(style = function(value, index) {
          acwr_value <- df[[acwr_col]][index]
          if (is.na(acwr_value)) return(NULL)
          
          bg_color <- if (acwr_value < 0.8 || acwr_value > 1.2) {
            "red"
          } else if (acwr_value < 0.9 || acwr_value > 1.1) {
            "yellow"
          } else {
            "white"
          }
          
          list(background = bg_color)
        })
      } else {
        colDef()
      }
    })
    
    names(col_defs) <- names(df)[1:14]
    
    reactable(df[, c(1, 3:14)],
              searchable = TRUE,
              pagination = FALSE,
              striped = TRUE,
              compact = TRUE,
              resizable = TRUE,
              defaultColDef = colDef(align = 'center'),
              columns = col_defs
    )
  })
  
  
  output$pl_graph <- renderPlotly({
    ggplot(player_load_variable(), aes(x= date, y = value, fill = variable)) +
      geom_bar(stat = "summary",
               fun = "sum",
               position = "dodge",
               colour = "black") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position  = 'none',
            axis.title.x = element_blank(), axis.title.y = element_blank(), strip.text = element_text(size = 8)) +
      facet_wrap(variable ~ ., scales = 'free_y', ncol = 2) +
      theme_minimal()
  })
  
  output$player_acwr_graph <- renderPlotly({
    ggplot(player_load_variable(), aes(x= date, y = acwr7, Acute = atl, Chronic = ctl, ACWR = acwr)) +
      geom_line() +
      geom_point(aes(color = "#F2AD91")) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position  = 'none',
            axis.title.x = element_blank(), axis.title.y = element_blank(), strip.text = element_text(size = 8)) +
      ylim(0,2) +
      labs(title = "Daily ACWR")
  })
  
  output$stress <- renderValueBox({
    value <- round(wellness_overview()$stress, 0)
    color <- if (value < 3) "red" else if (value < 6) "yellow" else "green"
    valueBox(value, "Stress", color = color)
  })
  
  output$mood <- renderValueBox({
    value <- round(wellness_overview()$mood, 0)
    color <- if (value < 3) "red" else if (value < 6) "yellow" else "green"
    valueBox(value, "Mood", color = color)
  })
  
  output$soreness <- renderValueBox({
    value <- round(wellness_overview()$soreness, 0)
    color <- if (value < 3) "red" else if (value < 6) "yellow" else "green"
    valueBox(value, "Soreness", color = color)
  })
  
  output$energy <- renderValueBox({
    value <- round(wellness_overview()$energy, 0)
    color <- if (value < 3) "red" else if (value < 6) "yellow" else "green"
    valueBox(value, "Energy", color = color)
  })
  
  output$sleep <- renderValueBox({
    df <- sleep_overview() %>%
      filter(player == input$sleep_player, date == input$sleep_date)
    
    value <- round(df$sleep_duration[1], 1)
    color <- if (value < 4) "red" else if (value < 6) "yellow" else "green"
    valueBox(value, "Sleep Duration", color = color)
  })

  output$resting_hr <- renderValueBox({
    df <- sleep_overview() %>%
      filter(player == input$sleep_player, date == input$sleep_date)
    
    req(nrow(df) > 0)
    
    value <- round(df$resting_hr[1], 0)
    flag <- df$hr_increase_3d[1]
    
    color <- if (!is.na(flag) && flag) "red" else "green"
    
    valueBox(value, "Resting HR", color = color)
  })

  output$hrv <- renderValueBox({
    df <- sleep_overview() %>%
      filter(player == input$sleep_player, date == input$sleep_date)
    
    req(nrow(df) > 0)
    
    value <- round(df$hrv[1], 0)
    
    flag_increase <- isTRUE(df$hrv_increase_3d[1])
    flag_decrease <- isTRUE(df$hrv_decrease_3d[1])
    
    color <- if (flag_increase || flag_decrease) "red" else "green"
    
    valueBox(value, "HRV", color = color)
  })
  
  output$hr_summary <- renderPlotly({
    df <- sleep_overview() %>%
      mutate(
        resting_hr_flag = hr_increase_3d,
        hrv_flag = hrv_increase_3d | hrv_decrease_3d
      ) %>%
      select(player, date, resting_hr, hrv, resting_hr_flag, hrv_flag) %>%
      pivot_longer(cols = c(resting_hr, hrv), names_to = "metric", values_to = "value") %>%
      mutate(
        color = ifelse(metric == "resting_hr", "lightblue", "darkblue"),
        flag = case_when(
          metric == "resting_hr" ~ resting_hr_flag,
          metric == "hrv" ~ hrv_flag,
          TRUE ~ FALSE
        )
      )
    
    # Resting HR Plot
    hr_plot <- df %>% filter(metric == "resting_hr") %>%
      ggplot(aes(x = date, y = value)) +
      geom_line(color = "lightblue", linewidth = 1) +
      geom_point(color = "lightblue", size = 2) +
      geom_point(data = . %>% filter(flag == TRUE),
                 aes(x = date, y = value),
                 shape = 21, stroke = 1.5, color = "red", fill = NA, size = 3) +
      theme_minimal() +
      labs(title = "Resting HR", x = NULL, y = NULL) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
    # HRV Plot
    hrv_plot <- df %>% filter(metric == "hrv") %>%
      ggplot(aes(x = date, y = value)) +
      geom_line(color = "darkblue", linewidth = 1) +
      geom_point(color = "darkblue", size = 2) +
      geom_point(data = . %>% filter(flag == TRUE),
                 aes(x = date, y = value),
                 shape = 21, stroke = 1.5, color = "red", fill = NA, size = 3) +
      theme_minimal() +
      labs(title = "HRV", x = NULL, y = NULL) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
    subplot(ggplotly(hr_plot), ggplotly(hrv_plot), nrows = 2, shareX = TRUE, titleX = TRUE) %>%
      layout(title = list(text = "HR and HRV Summary", x = 0.5))
  })

  
}