# Frontend ----

ui <- dashboardPage(
  title = "Spurs Demo App",
  header = dashboardHeader(
    title = tagList(
      span(class = "logo-lg", img(src = "sa_spurs.png", height = "50px", width = "auto")), 
      span(class = "logo-mini", img(src = "spurs.png", height = "50px", width = "auto"))
    )
  ),
  
  ## Sidebar ----
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Team Overview", tabName = "team_overview", icon = icon("people-group"), selected = TRUE),
      menuItem("Physical Load", tabName = "player_load", icon = icon("person-running")),
      menuItem("Monitoring", tabName = "monitor_load", icon = icon("magnifying-glass-chart"),
               menuSubItem("Wellness Questionnaire", tabName = "wellness_report", icon = icon("spa")),
               menuSubItem("Sleep", tabName = "sleep_report", icon = icon("bed"))
      )
    )
  ),
  
  ## Body ----
  body = dashboardBody(
    use_theme(mytheme),
    tabItems(
      # Team Overview
      tabItem(
        tabName = "team_overview",
        h2("Team Overview"),
        fluidPage(
          pickerInput(
            "dl_date", label = "Date:",
            choices = sort(unique(as.character(combined_data$date)), decreasing = TRUE),
            multiple = FALSE,
            options = list(`actions-box` = TRUE, "style-base" = "form-control", style = ""),
            selected = sort(unique(as.character(combined_data$date)), decreasing = TRUE)[1]
          ),
          reactableOutput("team_table"),
          br(),
          tags$div(
            style = "font-size: 14px; color: #555;",
            HTML("<strong>Conditional Formatting:</strong> Cells with ACWR values < 0.8 or > 1.2 are <span style='background-color: red; padding: 2px 6px;'>red</span>, 
            values between 0.8–0.9 or 1.1–1.2 are <span style='background-color: yellow; padding: 2px 6px;'>yellow</span>, 
            and values within the 0.9–1.1 range are uncolored.")
          )
        )
      ),
      
      # Player Load
      tabItem(
        tabName = "player_load",
        h2("Player Load"),
        fluidPage(
          pickerInput(
            "pl_variable", label = "Variable:",
            choices = c("total_distance", "hsr_distance", "sprint_distance", "accels", "decels"),
            multiple = FALSE,
            options = list(`actions-box` = TRUE, "style-base" = "form-control"),
            selected = "total_distance"
          ),
          pickerInput(
            "pl_player", label = "Player:",
            choices = sort(unique(combined_data$player)),
            multiple = FALSE,
            options = list(`actions-box` = TRUE, "style-base" = "form-control"),
            selected = c(sort(combined_data$player))[1]
          ),
          plotlyOutput("pl_graph", height = '600px', width = '100%'),
          br(),
          plotlyOutput("player_acwr_graph", height = '600px', width = '100%')
        )
      ),
      
      # Wellness Report
      tabItem(
        tabName = "wellness_report",
        h2("Wellness Questionnaire"),
        fluidPage(
          pickerInput(
            "well_date", label = "Date:",
            choices = sort(unique(as.character(wellness_data$date)), decreasing = TRUE),
            multiple = FALSE,
            options = list(`actions-box` = TRUE, "style-base" = "form-control", style = ""),
            selected = sort(unique(as.character(wellness_data$date)), decreasing = TRUE)[1]
          ),
          pickerInput(
            "well_player", label = "Player:",
            choices = sort(unique(wellness_data$player)),
            multiple = FALSE,
            options = list(`actions-box` = TRUE, "style-base" = "form-control"),
            selected = c(sort(wellness_data$player))[1]
          ),
          valueBoxOutput("stress", width = 3),
          valueBoxOutput("mood", width = 3),
          valueBoxOutput("soreness", width = 3),
          valueBoxOutput("energy", width = 3)
        )
      ),
      
      # Sleep Report
      tabItem(
        tabName = "sleep_report",
        h2("Sleep"),
        fluidPage(
          pickerInput(
            "sleep_date", label = "Date:",
            choices = sort(unique(as.character(sleep_data$date)), decreasing = TRUE),
            multiple = FALSE,
            options = list(`actions-box` = TRUE, "style-base" = "form-control", style = ""),
            selected = sort(unique(as.character(sleep_data$date)), decreasing = TRUE)[1]
          ),
          pickerInput(
            "sleep_player", label = "Player:",
            choices = sort(unique(sleep_data$player)),
            multiple = FALSE,
            options = list(`actions-box` = TRUE, "style-base" = "form-control"),
            selected = c(sort(sleep_data$player))[1]
          ),
          fluidRow(
            valueBoxOutput("sleep", width = 4),
            valueBoxOutput("resting_hr", width = 4),
            valueBoxOutput("hrv", width = 4)
          ),
          br(),
          plotlyOutput("hr_summary"),
          br(),
          tags$div(
            style = "font-size: 14px; color: #555;",
            HTML("<strong>Flag Explanation:</strong><br>
        <ul style='padding-left: 20px;'>
          <li><strong>Sleep Duration:</strong> Value box is 
            <span style='background-color: red; color: white; padding: 2px 6px;'>red</span> if sleep is less than 3 hours, 
            <span style='background-color: #f0ad4e; color: white; padding: 2px 6px;'>orange</span> if between 3–6 hours, and 
            <span style='background-color: #5cb85c; color: white; padding: 2px 6px;'>green</span> if 6 or more hours were recorded.</li>
          <li><strong>Resting HR:</strong> A <span style='border: 2px solid red; padding: 0 4px;'>red-outlined</span> point or 
              <span style='background-color: red; color: white; padding: 2px 6px;'>red value box</span> indicates that resting HR has 
              <em>increased for 3 consecutive days</em>.</li>
          <li><strong>HRV:</strong> A <span style='border: 2px solid red; padding: 0 4px;'>red-outlined</span> point or 
              <span style='background-color: red; color: white; padding: 2px 6px;'>red value box</span> indicates that HRV has 
              <em>either increased or decreased for 3 consecutive days</em>.</li>
        </ul>")
          )
        )
      )
    )
  )
)