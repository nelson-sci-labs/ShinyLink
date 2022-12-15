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
          "cut.a",
          p("Lower bound for full string-distance match"),
          min = 0,
          max = 1,
          value = 0.92
        ),
        sliderInput(
          "cut.p",
          p("Lower bound for partial string-distance match"),
          min = 0,
          max = 1,
          value = 0.88
        )
      ),
      box(
        width = 6,
        title = "MLE (Maximum likelihood estimation) options",
        status = "orange",
        solidHeader = FALSE,
        collapsible = FALSE,
        sliderInput(
          "w.lambda",
          p("Weighting of the MLE and prior estimate for the lambda parameter"),
          min = 0,
          max = 1,
          value = 0.29
        ),
        sliderInput(
          "w.pi",
          p("Weighting of the MLE and prior estimate for the pi parameter"),
          min = 0,
          max = 1,
          value = 0.51
        )
      )
    ),
    fluidRow(
      box(
        width = 6,
        title = "EM (Expectation–maximization) estimation & Dedupe options",
        status = "orange",
        solidHeader = FALSE,
        collapsible = FALSE,

        materialSwitch(
          inputId = "id",
          label = "estimate.only",
          status = "danger"
        ),

        materialSwitch(
          inputId = "id2",
          label = "dedupe.matches",
          status = "danger"
        ),
        materialSwitch(
          inputId = "id3",
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
        numericInput("n.cores",
                     p("The number of cores to parallelize"),
                     value = 1),
        numericInput("tol.em",
                     p(
                       "Convergence tolerance for the EM algorithm"
                     ),
                     value = 1e-04),
        sliderInput(
          "threshold.match",
          p(
            "Lower bound for the posterior probability of a match that will be accepted"
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
mod_advanced_parameters_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_advanced_parameters_ui("advanced_parameters_1")

## To be copied in the server
# mod_advanced_parameters_server("advanced_parameters_1")
