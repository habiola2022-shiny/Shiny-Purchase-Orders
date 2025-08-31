library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(readxl)
library(bslib)


# modules
source("modules/mod_upload.R")
source("modules/mod_table.R")
source("modules/mod_filters.R")
source("modules/mod_dashboardplots.R")

# Professional theme
theme <- bs_theme(
  version = 5,
  bootswatch = "cosmo",
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins"),
  code_font = font_google("Fira Mono")
)

ui <- page_fillable(
  theme = bs_theme(version = 5),  # keep base bootstrap, light
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=NA),
    tags$link(href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=Poppins:wght@600&display=swap", 
              rel="stylesheet")
  ),
  
  # Header
  page_navbar(
    title = "Purchase Order Insights",
    nav_spacer()
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      class = "bg-light p-3 rounded shadow-sm",
      h5("⚙️ Controls", class = "fw-bold mb-3"),
      card(
        card_header("Upload Data"),
        card_body(mod_upload_ui("upload"))
      ),
      card(
        card_header("Filters"),
        card_body(mod_filters_ui("filters"))
      )
    ),
    fill = TRUE,
    card(
      full_screen = TRUE,
      card_header("Results"),
      card_body(
        tabsetPanel(
          type = "pills",
          tabPanel("Data Table", mod_table_ui("table")),
          tabPanel(
            "Dashboard",
            mod_dashboardplots_ui("dashboard")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- mod_upload_server("upload")
  filtered <- mod_filters_server("filters", data)
  mod_table_server("table", filtered)
  mod_dashboardplots_server("dashboard", filtered)
  
}

shinyApp(ui, server)
