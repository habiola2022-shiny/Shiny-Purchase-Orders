mod_summary_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Rows card
    column(3,
           actionLink(
             ns("go_table"), 
             label = NULL, 
             class = "card-link",
             card(
               class = "bg-primary text-white text-center p-3",
               h4("Rows"),
               h2(textOutput(ns("nrows")))
             )
           )
    ),
    
    # Numeric Sum card
    column(3,
           actionLink(ns("go_plot1"), label = NULL, class = "card-link",
                      card(
                        class = "bg-success text-white text-center p-3",
                        h4("Numeric Columns Sum"),
                        h2(textOutput(ns("numsum")))
                      )
           )
    ),
    
    # Suppliers card
    column(3,
           actionLink(ns("go_plot2"), label = NULL, class = "card-link",
                      card(
                        class = "bg-info text-white text-center p-3",
                        h4("Unique Suppliers"),
                        h2(textOutput(ns("suppliers")))
                      )
           )
    ),
    
    # Date Range card
    column(3,
           card(
             class = "bg-warning text-dark text-center p-3",
             h4("Date Range"),
             h5(textOutput(ns("daterange")))
           )
    )
  )
}

mod_summary_server <- function(id, data, parent_session) {
  moduleServer(id, function(input, output, session) {
    
    # KPIs
    output$nrows <- renderText({ nrow(data()) })
    
    output$numsum <- renderText({
      num_cols <- data()[sapply(data(), is.numeric)]
      if (ncol(num_cols) == 0) return("–")
      format(sum(num_cols, na.rm = TRUE), big.mark = ",")
    })
    
    output$suppliers <- renderText({
      if (!"Supplier" %in% names(data())) return("–")
      length(unique(data()$Supplier))
    })
    
    output$daterange <- renderText({
      if (!"Date" %in% names(data())) return("–")
      d <- as.Date(data()$Date)
      paste(min(d, na.rm = TRUE), "to", max(d, na.rm = TRUE))
    })
    
    # Navigation
    observeEvent(input$go_table, {
      updateTabsetPanel(parent_session, "main_tabs", selected = "Data Table")
    })
    observeEvent(input$go_plot1, {
      updateTabsetPanel(parent_session, "main_tabs", selected = "Plot 1")
    })
    observeEvent(input$go_plot2, {
      updateTabsetPanel(parent_session, "main_tabs", selected = "Plot 2")
    })
  })
}
