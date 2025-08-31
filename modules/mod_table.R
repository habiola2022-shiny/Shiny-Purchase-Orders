# modules/mod_table.R
mod_table_ui <- function(id) {
  ns <- NS(id)
  DTOutput(ns("table"))
}

mod_table_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderDT({
      req(data())   # make sure data exists
      datatable(data(), options = list(pageLength = 10))
    })
  })
}

