#' advanced_details UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_advanced_details_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    fluidRow(
      column(6,
             box(
               width = 12,
               title = "Matched in Sample Dataset",
               status = "success",
               solidHeader = FALSE,
               collapsible = FALSE,
               column(12, DT::dataTableOutput(ns('matched_dfA')))
             )
      ),
      column(6,
             box(
               width = 12,
               title = "Matched in Matching Dataset",
               status = "success",
               solidHeader = FALSE,
               collapsible = FALSE,
               column(12, DT::dataTableOutput(ns('matched_dfB')))
             )
      )
    ),
    br(),
    fluidRow(
      column(6,
             box(
               width = 12,
               title = "Unmatched in Sample Dataset",
               status = "success",
               solidHeader = FALSE,
               collapsible = FALSE,
               column(12, DT::dataTableOutput(ns('unmatched_dfA')))
             )
      ),
      column(6,
             box(
               width = 12,
               title = "Unmatched in Matching Dataset",
               status = "success",
               solidHeader = FALSE,
               collapsible = FALSE,
               column(12, DT::dataTableOutput(ns('unmatched_dfB')))
             )
      )
    )
)}

#' advanced_details Server Functions
#'
#' @noRd
mod_advanced_details_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_advanced_details_ui("advanced_details_1")

## To be copied in the server
# mod_advanced_details_server("advanced_details_1")
