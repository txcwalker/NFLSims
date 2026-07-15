# R/tests/test_reviewer_ui.R
suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(dplyr)
})

csv_path <- "outputs/historical_test_results.csv"

ui <- fluidPage(
  titlePanel("Fourth Down Bot - Historical Testing Reviewer"),
  
  sidebarLayout(
    sidebarPanel(
      p("Use this UI to manually review the decisions made by the 4th down bot on historical data."),
      helpText("Filter by team, quarter, score differential, etc., to find specific edge cases."),
      hr(),
      actionButton("refresh", "Refresh Data", class = "btn-primary"),
      br(), br(),
      uiOutput("stats_ui"),
      width = 3
    ),
    
    mainPanel(
      DTOutput("results_table"),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    input$refresh
    # Try looking in current directory (if shiny set wd) or relative to project root
    path_to_check <- if(file.exists(csv_path)) csv_path else file.path("R/tests", csv_path)
    
    if(file.exists(path_to_check)) {
      df <- read.csv(path_to_check, stringsAsFactors = FALSE)
      # Format percentages
      if("pre_wp" %in% names(df)) df$pre_wp <- sprintf("%.1f%%", df$pre_wp * 100)
      if("wp_go" %in% names(df)) df$wp_go <- sprintf("%.1f%%", df$wp_go * 100)
      if("wp_punt" %in% names(df)) df$wp_punt <- sprintf("%.1f%%", df$wp_punt * 100)
      if("wp_fg" %in% names(df)) df$wp_fg <- sprintf("%.1f%%", df$wp_fg * 100)
      
      df <- df %>% 
        select(game_id, posteam, qtr, game_seconds_remaining, score_differential, 
               yardline_100, ydstogo, desc, pre_wp, wp_go, wp_punt, wp_fg, best_action)
      return(df)
    } else {
      return(data.frame(Message = "No test results found. Run auto_test_fourth_downs.R first."))
    }
  })
  
  output$stats_ui <- renderUI({
    df <- data()
    if("best_action" %in% names(df)) {
      go_pct <- mean(df$best_action == "go", na.rm = TRUE) * 100
      tagList(
        h4("Quick Stats"),
        p(sprintf("Total Scenarios Evaluated: %d", nrow(df))),
        p(sprintf("Go For It Rate: %.1f%%", go_pct))
      )
    }
  })
  
  output$results_table <- renderDT({
    df <- data()
    dt <- datatable(df, 
              options = list(pageLength = 25, scrollX = TRUE),
              filter = "top",
              rownames = FALSE,
              class = "display nowrap")
              
    if("best_action" %in% names(df)) {
      dt <- dt %>% formatStyle(
        'best_action',
        backgroundColor = styleEqual(c('go', 'punt', 'fg'), c('#d4edda', '#f8d7da', '#cce5ff'))
      )
    }
    dt
  })
}

shinyApp(ui, server)
