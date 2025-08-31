# modules/mod_filters.R
mod_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    lapply(1:5, function(i) {
      card(
        class = "mb-3 shadow-sm",
        card_header(paste("Filter", i)),
        card_body(
          uiOutput(ns(paste0("col_select_", i))),
          uiOutput(ns(paste0("filter_input_", i)))
        )
      )
    })
  )
}


mod_filters_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # helper: build filter UI based on column type
    make_filter_ui <- function(i) {
      ns <- session$ns
      output[[paste0("col_select_", i)]] <- renderUI({
        req(data())
        selectInput(
          ns(paste0("col_", i)),
          paste("Choose column (Filter", i, ")"),
          choices = c("None", names(data())),
          selected = "None"
        )
      })
      
      output[[paste0("filter_input_", i)]] <- renderUI({
        req(data())
        colname <- input[[paste0("col_", i)]]
        if (is.null(colname) || colname == "None") return(NULL)
        
        vals <- data()[[colname]]
        
        # Date
        if (inherits(vals, "Date")) {
          dateRangeInput(
            ns(paste0("val_", i)),
            paste("Date range for", colname),
            start = min(vals, na.rm = TRUE),
            end   = max(vals, na.rm = TRUE)
          )
        }
        # Numeric
        else if (is.numeric(vals)) {
          sliderInput(
            ns(paste0("val_", i)),
            paste("Range for", colname),
            min = min(vals, na.rm = TRUE),
            max = max(vals, na.rm = TRUE),
            value = range(vals, na.rm = TRUE)
          )
        }
        # Categorical / character
        else {
          selectInput(
            ns(paste0("val_", i)),
            paste("Values for", colname),
            choices = unique(vals),
            selected = unique(vals),
            multiple = TRUE
          )
        }
      })
    }
    
    # create 5 filter slots
    lapply(1:5, make_filter_ui)
    
    # apply filters
    reactive({
      df <- data()
      req(df)
      
      for (i in 1:5) {
        colname <- input[[paste0("col_", i)]]
        val <- input[[paste0("val_", i)]]
        
        if (!is.null(colname) && colname != "None" && !is.null(val)) {
          if (inherits(df[[colname]], "Date")) {
            df <- df[df[[colname]] >= val[1] & df[[colname]] <= val[2], ]
          } else if (is.numeric(df[[colname]])) {
            df <- df[df[[colname]] >= val[1] & df[[colname]] <= val[2], ]
          } else {
            df <- df[df[[colname]] %in% val, ]
          }
        }
      }
      df
    })
  })
}
