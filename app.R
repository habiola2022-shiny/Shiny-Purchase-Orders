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
source("modules/mod_filters.R")
source("modules/mod_clustering.R")
source("modules/mod_aiquery.R")
source("modules/mod_table.R")
source("modules/mod_plots.R")
#source("modules/mod_summary.R")   # don't forget this one!

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),  # ✅ close bs_theme() here
  
  titlePanel("Purchase Order Insights"),
  
  sidebarLayout(
    sidebarPanel(
      mod_upload_ui("upload"),
      mod_filters_ui("filters"),
      mod_clustering_ui("clustering"),
      mod_aiquery_ui("aiquery")
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        #tabPanel("Summary", mod_summary_ui("summary")),
        tabPanel("Data Table", mod_table_ui("table")),
        tabPanel("Plots", mod_plots_ui("plots"))
      )
    )
  )
)

server <- function(input, output, session) {
  data <- mod_upload_server("upload")
  filtered <- mod_filters_server("filters", data)
  clustered <- mod_clustering_server("clustering", filtered)
  queried <- mod_aiquery_server("aiquery", clustered)
  
  mod_table_server("table", queried)
  mod_plots_server("plots", queried)
  #mod_summary_server("summary", data, parent_session = session)
}

shinyApp(ui, server)
