#' advanced_parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_advanced_parameters_ui <- function(id){
  ns <- NS(id)
  tagList(
    chooseSliderSkin("Modern", color = "#ED5565"),
    fluidRow(
      box(
        width = 6,
        title = "Threshold options",
        status = "orange",
        solidHeader = FALSE,
        collapsible = FALSE,

        sliderInput(
          ns("cut_a"),
          p("Lower bound for full string-distance match"),
          min = 0,
          max = 1,
          value = 0.92
        ),
        sliderInput(
          ns("cut_p"),
          p("Lower bound for partial string-distance match"),
          min = 0,
          max = 1,
          value = 0.88
        )
      ),
      box(
        width = 6,
        title = "MLE (Maximum likelihood estimation) options (Not applicable)",
        status = "orange",
        solidHeader = FALSE,
        collapsible = FALSE,
        sliderInput(
          ns("w_lambda"),
          p("Weighting of the MLE and prior estimate for the lambda parameter"),
          min = 0,
          max = 1,
          value = 0
        ),
        sliderInput(
          ns("w_pi"),
          p("Weighting of the MLE and prior estimate for the pi parameter"),
          min = 0,
          max = 1,
          value = 0
        )
      )
    ),
    fluidRow(
      box(
        width = 6,
        title = "EM (Expectation maximization) estimation & Dedupe options",
        status = "orange",
        solidHeader = FALSE,
        collapsible = FALSE,

        materialSwitch(
          inputId = ns("estimate_only"),
          label = "estimate.only  (Not applicable)",
          status = "danger"
        ),

        materialSwitch(
          inputId = ns("dedupe_matches"),
          label = "dedupe.matches",
          status = "danger",
          value = TRUE
        ),
        materialSwitch(
          inputId = ns("linprog_dedupe"),
          label = "linprog.dedupe",
          status = "danger"
        )

      ),
      box(
        width = 6,
        title = "Miscellaneous options",
        status = "orange",
        solidHeader = FALSE,
        collapsible = FALSE,
        numericInput(ns("n_cores"),
                     p("The number of cores to parallelize"),
                     value = 1),
        numericInput(ns("tol_em"),
                     p(
                       "Convergence tolerance for the EM algorithm"
                     ),
                     value = 1e-04),
        sliderInput(
          ns("threshold_match"),
          p(
            "Lower bound for the posterior probability of a match that will be accepted  (Not applicable)"
          ),
          min = 0,
          max = 1,
          value = 0.85
        )
      ),


    )
  )
}

#' advanced_parameters Server Functions
#'
#' @noRd
mod_advanced_parameters_server <- function(id, state, session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({

      # Advanced parameters
      state$cut_a <- input$cut_a
      state$cut_p <- input$cut_p
      state$w_lambda <- input$w_lambda
      state$w_pi <- input$w_pi

      state$estimate_only <- input$estimate_only
      state$dedupe_matches <- input$dedupe_matches
      state$linprog_dedupe <- input$linprog_dedupe

      state$n_cores <- input$n_cores
      state$tol_em <- input$tol_em
      state$threshold_match <- input$threshold_match

    })
  })
}

## To be copied in the UI
# mod_advanced_parameters_ui("advanced_parameters_1")

## To be copied in the server
# mod_advanced_parameters_server("advanced_parameters_1")
