mod_dashboardplots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        div(class = "dashboard-card",
            plotlyOutput(ns("monthly_plot"))
        )
      ),
      column(
        6,
        div(class = "dashboard-card",
            plotlyOutput(ns("status_plot"))
        )
      ),
      column(
        6,
        div(class = "dashboard-card",
            plotlyOutput(ns("department_plot"))
        )
      )
    )
  )
}

mod_dashboardplots_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    df <- reactive({ req(data()); data() })
    
    # ----- Monthly Cost Plot -----
    output$monthly_plot <- renderPlotly({
      df_data <- df()
      if(!("Cost" %in% names(df_data))) return(NULL)
      
      date_cols <- names(df_data)[sapply(df_data, inherits, "Date")]
      if(length(date_cols) == 0) return(NULL)
      date_col <- date_cols[1]
      
      df_month <- df_data %>%
        mutate(YearMonth = format(.data[[date_col]], "%Y-%m")) %>%
        group_by(YearMonth) %>%
        summarise(TotalCost = sum(Cost, na.rm = TRUE), .groups = "drop")
      
      df_month$YearMonth <- factor(df_month$YearMonth, levels = df_month$YearMonth)
      
      p <- ggplot(df_month, aes(x = YearMonth, y = TotalCost)) +
        geom_col(fill = "#007bff") +
        labs(title = "Total Cost by Month", x = NULL, y = NULL) +   # Remove axis titles
        theme_minimal(base_size = 14) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks = element_blank()
        )
      
      ggplotly(p)
    })
    
    # ----- Count by Status -----
    output$status_plot <- renderPlotly({
      df_data <- df()
      if(!("Status" %in% names(df_data))) return(NULL)
      
      df_count <- df_data %>%
        group_by(Status) %>%
        summarise(Count = n(), .groups = "drop")
      
      p <- ggplot(df_count, aes(x = Status, y = Count, fill = Status)) +
        geom_col(show.legend = FALSE) +
        labs(title = "Count by Status", x = NULL, y = NULL) +      # Remove axis titles
        theme_minimal(base_size = 14) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks = element_blank()
        )
      
      ggplotly(p)
    })
    
    # ----- Count by Department -----
    output$department_plot <- renderPlotly({
      df_data <- df()
      if(!("Department" %in% names(df_data))) return(NULL)
      
      df_count <- df_data %>%
        group_by(Department) %>%
        summarise(Count = n(), .groups = "drop")
      
      p <- ggplot(df_count, aes(x = Department, y = Count, fill = Department)) +
        geom_col(show.legend = FALSE) +
        labs(title = "Count by Department", x = NULL, y = NULL) + # Remove axis titles
        theme_minimal(base_size = 14) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks = element_blank()
        )
      
      ggplotly(p)
    })
    
  })
}
