#' manual_inspection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manual_inspection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Manual Inspection",
      status = "success",
      p("Row #1 of 1 records with filters applied")
    ),
    box(
      width = 8,
      title = "Data Zone",
      status = "success",
      solidHeader = FALSE,
      collapsible = FALSE,
      fluidRow(
        column(
          width = 6,
          textInput(ns("firstname_dfA"), "Firstname:"),
          textInput(ns("firstname_dfB"), NULL),

          textInput(ns("middlename_dfA"), "Middlename"),
          textInput(ns("middlename_dfB"), NULL),

          textInput(ns("lastname_dfA"), "Lastname"),
          textInput(ns("lastname_dfB"), NULL),

          textInput(ns("birthday_dfA"), "Birthday"),
          textInput(ns("birthday_dfB"), NULL),

          textInput(ns("race_dfA"), "Ethnicity"),
          textInput(ns("race_dfB"), NULL),

        ),
        column(
          width = 6,
          textInput(ns("sex_dfA"), "Sex"),
          textInput(ns("sex_dfB"), NULL),

          textInput(ns("housenum_dfA"), "Housenum"),
          textInput(ns("housenum_dfB"), NULL),

          textInput(ns("streetname_dfA"), "Streetname"),
          textInput(ns("streetname_dfB"), NULL),

          textInput(ns("city_dfA"), "City"),
          textInput(ns("city_dfB"), NULL),

          textInput(ns("birthyear_dfA"), "Birthyear"),
          textInput(ns("birthyear_dfB"), NULL)
        )
      )
    ),
    box(
      width = 4,
      title = "Control Zone",
      status = "success",
      solidHeader = FALSE,
      collapsible = FALSE,
      h4("Control Zone"),

      fluidRow(
        column(
        width = 4,
        actionButton(ns("diff_person"),"Different Person", icon = icon("minus"), class = "btn-danger", width = "150px"),
        br(),
        br(),

        actionButton(ns("same_person"),"Same Person", icon = icon("plus"), class = "btn-success", width = "150px"),
        br(),
        br(),

        actionButton(ns("undecided_person"),"Undecided", icon = icon("question"), class = "btn-warning", width = "150px")
        )),
      br(),
      fluidRow(
        column(width = 6,
               actionButton(ns("previous_row"),"Previous Row", icon = icon("chevron-left"), class = "btn-info", width = "150px")
        ),
        column(width = 6,
               actionButton(ns("next_row"),"Next Row", icon = icon("chevron-right"), class = "btn-info", width = "150px")
        )
      )




    )
  )
}

#' manual_inspection Server Functions
#'
#' @noRd
mod_manual_inspection_server <- function(id, state, parent) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_manual_inspection_ui("manual_inspection_1")

## To be copied in the server
# mod_manual_inspection_server("manual_inspection_1")
