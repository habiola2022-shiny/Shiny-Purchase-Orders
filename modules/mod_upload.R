mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload CSV or Excel", accept = c(".csv", ".xls", ".xlsx")),
    uiOutput(ns("sheet_ui"))  # only show if Excel
  )
}

mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive for sheet names (only for Excel)
    sheet_choices <- reactive({
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      if (ext %in% c("xls", "xlsx")) {
        excel_sheets(input$file$datapath)
      } else {
        NULL
      }
    })
    
    # Render sheet picker if needed
    output$sheet_ui <- renderUI({
      ns <- session$ns
      sheets <- sheet_choices()
      if (!is.null(sheets)) {
        selectInput(ns("sheet"), "Choose sheet:", choices = sheets)
      }
    })
    
    # Reactive for uploaded data
    reactive({
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      
      df <- NULL
      if (ext == "csv") {
        df <- read_csv(input$file$datapath)
      } else if (ext %in% c("xls", "xlsx")) {
        req(input$sheet)
        df <- read_excel(input$file$datapath, sheet = input$sheet)
      } else {
        showNotification("Unsupported file type", type = "error")
        return(NULL)
      }
      
      # Auto-detect date columns
      for (col in names(df)) {
        if (inherits(df[[col]], "character")) {
          parsed <- as.Date(df[[col]], format = "%Y-%m-%d")
          if (!all(is.na(parsed))) {
            df[[col]] <- parsed
          }
        }
      }
      
      df
    })
  })
}

