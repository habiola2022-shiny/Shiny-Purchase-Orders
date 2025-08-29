mod_clustering_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("ai_filter_ui")),
    uiOutput(ns("select_cluster_ui"))
  )
}

mod_clustering_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$ai_filter_ui <- renderUI({
      req(data())
      df <- data()
      num_df <- df[sapply(df, is.numeric)]
      if (ncol(num_df) >= 2 && nrow(num_df) >= 3) {
        sliderInput(session$ns("cluster_count"), "Number of clusters (AI filter):", 
                    min = 2, max = 5, value = 3)
      }
    })
    
    clusters <- reactive({
      req(data())
      df <- data()
      num_df <- df[sapply(df, is.numeric)]
      req(ncol(num_df) >= 2, nrow(num_df) >= 3, input$cluster_count)
      set.seed(123)
      km <- kmeans(num_df, centers = input$cluster_count)
      df$ClusterAI <- as.factor(km$cluster)
      df
    })
    
    output$select_cluster_ui <- renderUI({
      req(clusters())
      choices <- sort(unique(clusters()$ClusterAI))
      selectInput(session$ns("ai_cluster"), "Show only rows in AI cluster:", 
                  choices = choices, selected = choices[1])
    })
    
    reactive({
      df <- clusters()
      if (!is.null(input$ai_cluster)) {
        df <- df[df$ClusterAI == input$ai_cluster, , drop = FALSE]
      }
      df
    })
  })
}
