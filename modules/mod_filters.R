mod_filters_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("filter_ui"))
}

mod_filters_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$filter_ui <- renderUI({
      req(data())
      df <- data()
      filters <- lapply(seq_along(df), function(i) {
        colname <- names(df)[i]
        coldata <- df[[i]]
        if (is.character(coldata) || is.factor(coldata)) {
          selectInput(session$ns(paste0("filter_", colname)), 
                      paste("Filter", colname),
                      choices = unique(coldata), 
                      selected = unique(coldata),
                      multiple = TRUE)
        } else if (is.numeric(coldata)) {
          sliderInput(session$ns(paste0("filter_", colname)), 
                      paste("Filter", colname),
                      min = min(coldata, na.rm = TRUE),
                      max = max(coldata, na.rm = TRUE),
                      value = range(coldata, na.rm = TRUE))
        }
      })
      do.call(tagList, filters)
    })
    
    reactive({
      df <- data()
      req(df)
      for (colname in names(df)) {
        input_id <- paste0("filter_", colname)
        if (!is.null(input[[input_id]])) {
          if (is.numeric(df[[colname]])) {
            rng <- input[[input_id]]
            df <- df[df[[colname]] >= rng[1] & df[[colname]] <= rng[2], ]
          } else {
            df <- df[df[[colname]] %in% input[[input_id]], ]
          }
        }
      }
      df
    })
  })
}
