mod_plots_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6, plotlyOutput(ns("plot1"))),
    column(6, plotlyOutput(ns("plot2")))
  )
}

mod_plots_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$plot1 <- renderPlotly({
      df <- data()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      num_cols <- names(df)[sapply(df, is.numeric)]
      if (length(num_cols) >= 1) {
        p <- ggplot(df, aes_string(x = num_cols[1])) +
          geom_histogram(bins = 30, fill = "skyblue", color = "black") +
          theme_minimal()
        ggplotly(p)
      }
    })
    
    output$plot2 <- renderPlotly({
      df <- data()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      num_cols <- names(df)[sapply(df, is.numeric)]
      if (length(num_cols) >= 2) {
        if ("ClusterAI" %in% names(df)) {
          p <- ggplot(df, aes_string(x = num_cols[1], y = num_cols[2], color = "ClusterAI")) +
            geom_point() +
            theme_minimal()
        } else {
          p <- ggplot(df, aes_string(x = num_cols[1], y = num_cols[2])) +
            geom_point(color = "steelblue") +
            theme_minimal()
        }
        ggplotly(p)
      }
    })
    
  })
}
