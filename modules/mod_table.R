mod_table_ui <- function(id) {
  ns <- NS(id)
  DTOutput(ns("datatable"))
}

mod_table_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$datatable <- renderDT({
      data()
    })
  })
}
