#' advanced_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_advanced_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(
      6,
      box(
        width = 12,
        title = "Start Advanced Matching",
        status = "success",
        solidHeader = FALSE,
        collapsible = FALSE,
        helpText("Identify which variables correspond to each piece of information"),
        actionButton(ns("match"), "Advanced Match"),
        hr(),
        br(),
        tags$label("Selected row(s) of Matching Results table:"),
        fluidRow(column(6, verbatimTextOutput(ns("info-main"))),
                 column(
                   6,
                   downloadButton(ns("download_selected"), "Download Selected")
                 )),
        hr(),
        tags$label("Summary of matching results:"),
        verbatimTextOutput(ns("matched-summary"))
      )
    ),
    column(
      6,
      box(
        width = 12,
        title = "Matching Summary",
        status = "success",
        solidHeader = FALSE,
        collapsible = FALSE,
        plotOutput(ns("plot-venn"))
      )
    )),
    box(
      width = 12,
      title = "Matching Results",
      status = "success",
      solidHeader = FALSE,
      collapsible = FALSE,
      column(12, DT::dataTableOutput(ns('matched')))
    ),
    fluidRow(
      column(
        width = 6,
        actionBttn(
          inputId = ns("previous_simple_settings"),
          label = "Previous: Simple Match Settings",
          style = "simple",
          color = "primary",
          icon = icon("arrow-left"),
          size = "sm"
        ),
        align = "left",
        style = "margin-bottom: 10px;",
        style = "margin-top: -10px;"
      ),
      column(
        width = 6,
        actionBttn(
          inputId = ns("next_simple_details"),
          label = "Next: Simple Match Details",
          style = "simple",
          color = "primary",
          icon = icon("arrow-right"),
          size = "sm"
        ),
        align = "right",
        style = "margin-bottom: 10px;",
        style = "margin-top: -10px;"
      ),
      style = "margin-left: 0px;",
      style = "margin-right: 0px;"
    )
  )
}

#' advanced_results Server Functions
#'
#' @noRd
mod_advanced_results_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_advanced_results_ui("advanced_results_1")

## To be copied in the server
# mod_advanced_results_server("advanced_results_1")
