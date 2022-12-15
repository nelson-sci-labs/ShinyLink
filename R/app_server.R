#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  app_state <- reactiveValues(
    dfA_uploaded = NULL,
    dfB_uploaded = NULL,
    dfA_cleaned_duplicate = NULL,
    dfB_cleaned_duplicate = NULL,
    dfA_cleaned_assignment = NULL,
    dfB_cleaned_assignment = NULL,
    dfA_cleaned_gender = NULL,
    dfB_cleaned_gender = NULL,
    dfA_cleaned_date = NULL,
    dfB_cleaned_date = NULL,
    dfA_cleaned_imputation = NULL,
    dfB_cleaned_imputation = NULL,

    state_dfA = NULL,
    state_dfB = NULL,

    matching_variables = NULL,
    string_matching = NULL,
    numeric_matching = NULL,
    partial_matching = NULL,
    matched_results = NULL
  )

  mod_uploading_server("uploading", app_state, session)
  mod_cleaning_duplicate_server("cleaning_duplicate", app_state, session)
  mod_cleaning_assignment_server("cleaning_assignment", app_state, session)
  mod_cleaning_gender_server("cleaning_gender", app_state, session)
  mod_cleaning_date_server("cleaning_date", app_state, session)
  mod_cleaning_imputation_server("cleaning_imputation", app_state, session)
  mod_simple_settings_server("simple_settings", app_state, session)
  mod_simple_results_server("simple_results", app_state, session)
  mod_simple_details_server("simple_details", app_state, session)

}
