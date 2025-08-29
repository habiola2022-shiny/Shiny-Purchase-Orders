mod_upload_ui <- function(id) {
  ns <- NS(id)
  fileInput(ns("datafile"), "Upload Excel or CSV File",
            accept = c(".csv", ".xlsx", ".xls"))
}

mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      req(input$datafile)
      ext <- tools::file_ext(input$datafile$name)
      if (ext == "csv") {
        readr::read_csv(input$datafile$datapath)
      } else if (ext %in% c("xlsx", "xls")) {
        readxl::read_excel(input$datafile$datapath)
      } else {
        validate("Invalid file; Please upload a .csv, .xlsx, or .xls file")
      }
    })
  })
}
