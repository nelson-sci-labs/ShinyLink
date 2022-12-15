#' simple_details UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simple_details_ui <- function(id){
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
    ),
    fluidRow(
      column(
        width = 6,
        actionBttn(
          inputId = ns("previous_simple_results"),
          label = "Previous: Simple Match Results",
          style = "simple",
          color = "primary",
          icon = icon("arrow-left"),
          size = "sm"
        ),
        align = "left",
        style = "margin-bottom: 10px;",
        style = "margin-top: -10px;"
      ),
      # column(
      #   width = 6,
      #   actionBttn(
      #     inputId = ns("next_advanced_parameters"),
      #     label = "Next: Advanced Parameters",
      #     style = "simple",
      #     color = "primary",
      #     icon = icon("arrow-right"),
      #     size = "sm"
      #   ),
      #   align = "right",
      #   style = "margin-bottom: 10px;",
      #   style = "margin-top: -10px;"
      # ),
      style = "margin-left: 0px;",
      style = "margin-right: 0px;"
    )
  )
}

#' simple_details Server Functions
#'
#' @noRd
mod_simple_details_server <- function(id, state, parent){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output[["matched_dfA"]] <- DT::renderDataTable(
      state$matched_results[['dfA.match']],
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
      state$matched_results[['dfA.unmatch']],
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
      state$matched_results[['dfB.match']],
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
      state$matched_results[['dfB.unmatch']],
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


    # Previous page button redirection
    observeEvent(input$previous_simple_results, {
      updateTabItems(session = parent, "tabs", "simple_results")
    })

  })
}

## To be copied in the UI
# mod_simple_details_ui("simple_details_1")

## To be copied in the server
# mod_simple_details_server("simple_details_1")
