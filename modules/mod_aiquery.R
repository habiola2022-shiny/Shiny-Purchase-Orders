mod_aiquery_ui <- function(id) {
  ns <- NS(id)
  textInput(ns("ai_query"), "Ask AI to filter the data:", "")
}

mod_aiquery_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    reactive({
      df <- data()
      if (!is.null(input$ai_query) && nzchar(input$ai_query)) {
        tryCatch({
          ellmer::query(df, input$ai_query)
        }, error = function(e) {
          df
        })
      } else {
        df
      }
    })
  })
}
