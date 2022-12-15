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
mod_advanced_details_server <- function(id, state, session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output[["matched_dfA"]] <- DT::renderDataTable(
      state$advanced_results[['dfA.match']],
      caption = 'Data in the Sample data set',
      extensions = 'Buttons',
      selection = "single",
      rownames = FALSE,
      server = FALSE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50,-1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons =
          list(
            "copy",
            list(
              extend = "collection"
              ,
              buttons = c("csv", "excel", "pdf")
              ,
              text = "Download"
            )
          )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    output[["unmatched_dfA"]] <- DT::renderDataTable(
      state$advanced_results[['dfA.unmatch']],
      caption = 'Data in the Sample data set',
      extensions = 'Buttons',
      selection = "single",
      rownames = FALSE,
      server = FALSE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50,-1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons =
          list(
            "copy",
            list(
              extend = "collection"
              ,
              buttons = c("csv", "excel", "pdf")
              ,
              text = "Download"
            )
          )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    output[["matched_dfB"]] <- DT::renderDataTable(
      state$advanced_results[['dfB.match']],
      caption = 'Data in the Sample data set',
      extensions = 'Buttons',
      selection = "single",
      rownames = FALSE,
      server = FALSE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50,-1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons =
          list(
            "copy",
            list(
              extend = "collection"
              ,
              buttons = c("csv", "excel", "pdf")
              ,
              text = "Download"
            )
          )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    output[["unmatched_dfB"]] <- DT::renderDataTable(
      state$advanced_results[['dfB.unmatch']],
      caption = 'Data in the Sample data set',
      extensions = 'Buttons',
      selection = "single",
      rownames = FALSE,
      server = FALSE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50,-1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons =
          list(
            "copy",
            list(
              extend = "collection"
              ,
              buttons = c("csv", "excel", "pdf")
              ,
              text = "Download"
            )
          )
      ),
      class = 'compact hover row-border nowrap stripe'
    )
  })
}

## To be copied in the UI
# mod_advanced_details_ui("advanced_details_1")

## To be copied in the server
# mod_advanced_details_server("advanced_details_1")
