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
      actionButton(ns("show"), "Show Manual Inspection dialog", class = "btn-primary")
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
          textInput(ns("level"), "Level of Match:"),

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
          textInput(ns("decision"), "Decision:"),

          textInput(ns("sex_dfA"), "Sex"),
          textInput(ns("sex_dfB"), NULL),

          textInput(ns("housenum_dfA"), "Housenum"),
          textInput(ns("housenum_dfB"), NULL),

          textInput(ns("streetname_dfA"), "Streetname"),
          textInput(ns("streetname_dfB"), NULL),

          textInput(ns("city_dfA"), "City"),
          textInput(ns("city_dfB"), NULL),

          textInput(ns("SSN_dfA"), "SSN"),
          textInput(ns("SSN_dfB"), NULL)
        )
      )
    ),
    box(
      width = 4,
      title = "Control Zone",
      status = "success",
      solidHeader = FALSE,
      collapsible = FALSE,

      textOutput(ns("my_text")),
      p('Your decision regarding this possible match will be recorded for these observations in a new variable called "manual_review"'),
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
    ),

    box(
      width = 12,
      title = "Uncertainty Matches",
      status = "success",
      column(12, DT::dataTableOutput(ns('uncertainty_matches')))
    ),
    box(
      width = 12,
      title = "Confirmed Matches",
      status = "success",
      column(12, DT::dataTableOutput(ns('confirmed_matches')))
    ),
    box(
      width = 12,
      title = "Confirmed Non-Matches",
      status = "success",
      column(12, DT::dataTableOutput(ns('confirmed_non_matches')))
    )
  )
}

#' manual_inspection Server Functions
#'
#' @noRd
mod_manual_inspection_server <- function(id, state, parent) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    update_review_data <- function(level = NULL, decision = NULL,
                                   firstname_dfA = NULL, firstname_dfB = NULL,
                                   middlename_dfA = NULL, middlename_dfB = NULL,
                                   lastname_dfA = NULL, lastname_dfB = NULL,
                                   birthday_dfA = NULL, birthday_dfB = NULL,
                                   race_dfA = NULL, race_dfB = NULL,
                                   sex_dfA = NULL, sex_dfB = NULL,
                                   housenum_dfA = NULL, housenum_dfB = NULL,
                                   streetname_dfA = NULL, streetname_dfB = NULL,
                                   city_dfA = NULL, city_dfB = NULL,
                                   SSN_dfA = NULL, SSN_dfB = NULL) {

      updateTextInput(session, "level", "Level of Match:",level)
      updateTextInput(session, "decision", "Decision:",decision)
      updateTextInput(session, "firstname_dfA", "Firstname", firstname_dfA)
      updateTextInput(session, "firstname_dfB", NULL, firstname_dfB)
      updateTextInput(session, "middlename_dfA", "Middlename", middlename_dfA)
      updateTextInput(session, "middlename_dfB", NULL, middlename_dfB)
      updateTextInput(session, "lastname_dfA", "Lastname", lastname_dfA)
      updateTextInput(session, "lastname_dfB", NULL, lastname_dfB)
      updateTextInput(session, "birthday_dfA", "Birthday", birthday_dfA)
      updateTextInput(session, "birthday_dfB", NULL, birthday_dfB)
      updateTextInput(session, "race_dfA", "Ethnicity", race_dfA)
      updateTextInput(session, "race_dfB", NULL, race_dfB)
      updateTextInput(session, "sex_dfA", "Sex", sex_dfA)
      updateTextInput(session, "sex_dfB", NULL, sex_dfB)
      updateTextInput(session, "housenum_dfA", "Housenum", housenum_dfA)
      updateTextInput(session, "housenum_dfB", NULL, housenum_dfB)
      updateTextInput(session, "streetname_dfA", "Streetname", streetname_dfA)
      updateTextInput(session, "streetname_dfB", NULL, streetname_dfB)
      updateTextInput(session, "city_dfA", "City", city_dfA)
      updateTextInput(session, "city_dfB", NULL, city_dfB)
      updateTextInput(session, "SSN_dfA", "SSN", SSN_dfA)
      updateTextInput(session, "SSN_dfB", NULL, SSN_dfB)
    }

    # reactiveValues object for storing current data
    vals <- reactiveValues(current_reviewing = 1,
                           choose_level = 3,
                           uncertain_dfs = NULL,
                           certain_dfs = NULL)

    output$my_text <- renderText({
      my_text <-
        paste0(
          "Row #",
          vals$current_reviewing,
          " of ",
          nrow(vals$uncertain_dfs),
          " records with filters applied"
        )
      strong_text <- paste0("<strong>", my_text, "</strong>")
      HTML(strong_text)
    })

    level_statistics <- reactive({
      # Production mode
      matched_dfs <- state$matched_results$matched_dfs

      tibble::tibble(
        `Level 1` = sum(matched_dfs$match_level == 1),
        `Level 2` = sum(matched_dfs$match_level == 2),
        `Level 3` = sum(matched_dfs$match_level == 3),
        `Level 4` = sum(matched_dfs$match_level == 4),
        `Level 5` = sum(matched_dfs$match_level == 5),
        `Level 6` = sum(matched_dfs$match_level == 6)
      )
    })

    output$n_levels_table <- renderTable({
      level_statistics()
    }, align = "c", striped = TRUE, spacing = "s")

    # Return the UI for a modal dialog with data selection input. If 'failed' is
    # TRUE, then display a message that the previous value was invalid.
    dataModal <- function(failed = FALSE) {

      # load the namespace
      ns <- session$ns

      # build the modal
      modalDialog(

        # make sure to wrap all id tokens in ns()
        # textInput(ns("dataset"), "Choose data set",
        #           placeholder = 'Try "mtcars" or "abc"'),

        hr(),

        strong("Here are the numbers of matches found in each level:"),

        tableOutput(ns("n_levels_table")),

        hr(),

        radioButtons(ns("choose_level"), "Up to which level would you like to review?",
                     c("Level 1 Certainty of Match: EXACT MATCH" = 1,
                       "Level 2 Certainty of Match: VERY HIGH" = 2,
                       "Level 3 Certainty of Match: HIGH" = 3,
                       "Level 4 Certainty of Match: FAIRLY-HIGH" = 4,
                       "Level 5 Certainty of Match: MODERATE" = 5,
                       "Level 6 Certainty of Match: LOW" = 6),
                     2),
        hr(),

        radioButtons(ns("choose_order"), "In what order would you like to review?",
                     c("LOW to HIGH" = "ascend",
                       "HIGH to LOW" = "descend")),

        # span('(Try the name of a valid data object like "mtcars", ',
        #      'then a name of a non-existent object like "abc")'),

        if (failed)
          div(tags$b("ZERO records to review. Try selecting a higher match level.", style = "color: red;")),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok"), "OK") # wrapped in ns()
        )

      )
    }

    # Show modal when button is clicked.
    observeEvent(input$show, {
      showModal(dataModal())
    })

    observeEvent(input$ok, {

      # print(input$dataset)

      # Production mode
      dfA <- state$state_dfA
      dfB <- state$state_dfB
      matched_dfs <- state$matched_results$matched_dfs

      # Check that data object exists and is data frame.
      if (!is.null(matched_dfs) && tibble::is_tibble(matched_dfs)) {

        vals$choose_level <- input$choose_level
        vals$uncertain_dfs <-
          matched_dfs %>% dplyr::filter(match_level >= vals$choose_level)
        vals$certain_dfs <-
          matched_dfs %>% dplyr::filter(match_level < vals$choose_level)
        vals$current_reviewing <- 1

        if(!is.null(vals$uncertain_dfs) && nrow(vals$uncertain_dfs) > 0) {

          currentA <- dfA[vals$uncertain_dfs$inds.a[vals$current_reviewing], ]
          currentB <- dfB[vals$uncertain_dfs$inds.b[vals$current_reviewing], ]

          update_review_data(
            level          = vals$uncertain_dfs$match_level[vals$current_reviewing],
            decision       = vals$uncertain_dfs$manual_selection[vals$current_reviewing],
            firstname_dfA  = currentA[["firstname"]],
            firstname_dfB  = currentB[["firstname"]],
            middlename_dfA = currentA[["middlename"]],
            middlename_dfB = currentB[["middlename"]],
            lastname_dfA   = currentA[["lastname"]],
            lastname_dfB   = currentB[["lastname"]],
            birthday_dfA   = currentA[["birthday"]],
            birthday_dfB   = currentB[["birthday"]],
            race_dfA       = currentA[["race"]],
            race_dfB       = currentB[["race"]],
            sex_dfA        = currentA[["sex"]],
            sex_dfB        = currentB[["sex"]],
            housenum_dfA   = currentA[["housenum"]],
            housenum_dfB   = currentB[["housenum"]],
            streetname_dfA = currentA[["streetname"]],
            streetname_dfB = currentB[["streetname"]],
            city_dfA       = currentA[["city"]],
            city_dfB       = currentB[["city"]],
            SSN_dfA        = currentA[["SSN"]],
            SSN_dfB        = currentB[["SSN"]])

          removeModal()
        } else {
          showModal(dataModal(failed = TRUE))
        }
      }
    })


    observeEvent(input$diff_person, {
      # Production mode
      dfA <- state$state_dfA
      dfB <- state$state_dfB
      matched_dfs <- state$matched_results$matched_dfs

      if (!is.null(matched_dfs) && tibble::is_tibble(matched_dfs)) {
        if (!is.null(vals$uncertain_dfs) && nrow(vals$uncertain_dfs) > 0) {

          vals$uncertain_dfs$manual_selection[vals$current_reviewing] <- 0

          if (vals$current_reviewing < nrow(vals$uncertain_dfs)) {


            vals$current_reviewing <- vals$current_reviewing + 1

            currentA <- dfA[vals$uncertain_dfs$inds.a[vals$current_reviewing],]
            currentB <- dfB[vals$uncertain_dfs$inds.b[vals$current_reviewing],]

            update_review_data(
              level          = vals$uncertain_dfs$match_level[vals$current_reviewing],
              decision       = vals$uncertain_dfs$manual_selection[vals$current_reviewing],
              firstname_dfA  = currentA[["firstname"]],
              firstname_dfB  = currentB[["firstname"]],
              middlename_dfA = currentA[["middlename"]],
              middlename_dfB = currentB[["middlename"]],
              lastname_dfA   = currentA[["lastname"]],
              lastname_dfB   = currentB[["lastname"]],
              birthday_dfA   = currentA[["birthday"]],
              birthday_dfB   = currentB[["birthday"]],
              race_dfA       = currentA[["race"]],
              race_dfB       = currentB[["race"]],
              sex_dfA        = currentA[["sex"]],
              sex_dfB        = currentB[["sex"]],
              housenum_dfA   = currentA[["housenum"]],
              housenum_dfB   = currentB[["housenum"]],
              streetname_dfA = currentA[["streetname"]],
              streetname_dfB = currentB[["streetname"]],
              city_dfA       = currentA[["city"]],
              city_dfB       = currentB[["city"]],
              SSN_dfA        = currentA[["SSN"]],
              SSN_dfB        = currentB[["SSN"]]
            )

            state$matched_results[['matched_intersect']] <- dplyr::bind_rows(
              vals$uncertain_dfs %>% dplyr::filter(manual_selection == 1), vals$certain_dfs)
          }
        }
      }
    })

    observeEvent(input$same_person, {
      # Production mode
      dfA <- state$state_dfA
      dfB <- state$state_dfB
      matched_dfs <- state$matched_results$matched_dfs

      if (!is.null(matched_dfs) && tibble::is_tibble(matched_dfs)) {
        if (!is.null(vals$uncertain_dfs) && nrow(vals$uncertain_dfs) > 0) {

          vals$uncertain_dfs$manual_selection[vals$current_reviewing] <- 1

          if (vals$current_reviewing < nrow(vals$uncertain_dfs)) {


            vals$current_reviewing <- vals$current_reviewing + 1

            currentA <- dfA[vals$uncertain_dfs$inds.a[vals$current_reviewing],]
            currentB <- dfB[vals$uncertain_dfs$inds.b[vals$current_reviewing],]

            update_review_data(
              level          = vals$uncertain_dfs$match_level[vals$current_reviewing],
              decision       = vals$uncertain_dfs$manual_selection[vals$current_reviewing],
              firstname_dfA  = currentA[["firstname"]],
              firstname_dfB  = currentB[["firstname"]],
              middlename_dfA = currentA[["middlename"]],
              middlename_dfB = currentB[["middlename"]],
              lastname_dfA   = currentA[["lastname"]],
              lastname_dfB   = currentB[["lastname"]],
              birthday_dfA   = currentA[["birthday"]],
              birthday_dfB   = currentB[["birthday"]],
              race_dfA       = currentA[["race"]],
              race_dfB       = currentB[["race"]],
              sex_dfA        = currentA[["sex"]],
              sex_dfB        = currentB[["sex"]],
              housenum_dfA   = currentA[["housenum"]],
              housenum_dfB   = currentB[["housenum"]],
              streetname_dfA = currentA[["streetname"]],
              streetname_dfB = currentB[["streetname"]],
              city_dfA       = currentA[["city"]],
              city_dfB       = currentB[["city"]],
              SSN_dfA        = currentA[["SSN"]],
              SSN_dfB        = currentB[["SSN"]]
            )

            state$matched_results[['matched_intersect']] <- dplyr::bind_rows(
              vals$uncertain_dfs %>% dplyr::filter(manual_selection == 1), vals$certain_dfs)
          }
        }
      }
    })

    observeEvent(input$undecided_person, {
      # Production mode
      dfA <- state$state_dfA
      dfB <- state$state_dfB
      matched_dfs <- state$matched_results$matched_dfs

      if (!is.null(matched_dfs) && tibble::is_tibble(matched_dfs)) {
        if (!is.null(vals$uncertain_dfs) && nrow(vals$uncertain_dfs) > 0) {

          vals$uncertain_dfs$manual_selection[vals$current_reviewing] <- NA

          if (vals$current_reviewing < nrow(vals$uncertain_dfs)) {


            vals$current_reviewing <- vals$current_reviewing + 1

            currentA <- dfA[vals$uncertain_dfs$inds.a[vals$current_reviewing],]
            currentB <- dfB[vals$uncertain_dfs$inds.b[vals$current_reviewing],]

            update_review_data(
              level          = vals$uncertain_dfs$match_level[vals$current_reviewing],
              decision       = vals$uncertain_dfs$manual_selection[vals$current_reviewing],
              firstname_dfA  = currentA[["firstname"]],
              firstname_dfB  = currentB[["firstname"]],
              middlename_dfA = currentA[["middlename"]],
              middlename_dfB = currentB[["middlename"]],
              lastname_dfA   = currentA[["lastname"]],
              lastname_dfB   = currentB[["lastname"]],
              birthday_dfA   = currentA[["birthday"]],
              birthday_dfB   = currentB[["birthday"]],
              race_dfA       = currentA[["race"]],
              race_dfB       = currentB[["race"]],
              sex_dfA        = currentA[["sex"]],
              sex_dfB        = currentB[["sex"]],
              housenum_dfA   = currentA[["housenum"]],
              housenum_dfB   = currentB[["housenum"]],
              streetname_dfA = currentA[["streetname"]],
              streetname_dfB = currentB[["streetname"]],
              city_dfA       = currentA[["city"]],
              city_dfB       = currentB[["city"]],
              SSN_dfA        = currentA[["SSN"]],
              SSN_dfB        = currentB[["SSN"]]
            )

            state$matched_results[['matched_intersect']] <- dplyr::bind_rows(
              vals$uncertain_dfs %>% dplyr::filter(manual_selection == 1), vals$certain_dfs)
          }
        }
      }
    })

    observeEvent(input$previous_row, {
      # Production mode
      dfA <- state$state_dfA
      dfB <- state$state_dfB
      matched_dfs <- state$matched_results$matched_dfs

      if (!is.null(matched_dfs) && tibble::is_tibble(matched_dfs)) {
        if (!is.null(vals$uncertain_dfs) && nrow(vals$uncertain_dfs) > 0) {
          if (vals$current_reviewing > 1) {

            vals$current_reviewing <- vals$current_reviewing - 1

            currentA <- dfA[vals$uncertain_dfs$inds.a[vals$current_reviewing],]
            currentB <- dfB[vals$uncertain_dfs$inds.b[vals$current_reviewing],]

            update_review_data(
              level          = vals$uncertain_dfs$match_level[vals$current_reviewing],
              decision       = vals$uncertain_dfs$manual_selection[vals$current_reviewing],
              firstname_dfA  = currentA[["firstname"]],
              firstname_dfB  = currentB[["firstname"]],
              middlename_dfA = currentA[["middlename"]],
              middlename_dfB = currentB[["middlename"]],
              lastname_dfA   = currentA[["lastname"]],
              lastname_dfB   = currentB[["lastname"]],
              birthday_dfA   = currentA[["birthday"]],
              birthday_dfB   = currentB[["birthday"]],
              race_dfA       = currentA[["race"]],
              race_dfB       = currentB[["race"]],
              sex_dfA        = currentA[["sex"]],
              sex_dfB        = currentB[["sex"]],
              housenum_dfA   = currentA[["housenum"]],
              housenum_dfB   = currentB[["housenum"]],
              streetname_dfA = currentA[["streetname"]],
              streetname_dfB = currentB[["streetname"]],
              city_dfA       = currentA[["city"]],
              city_dfB       = currentB[["city"]],
              SSN_dfA        = currentA[["SSN"]],
              SSN_dfB        = currentB[["SSN"]]
            )
            state$matched_results[['matched_intersect']]

            state$matched_results[['matched_intersect']] <- dplyr::bind_rows(
              vals$uncertain_dfs %>% dplyr::filter(manual_selection == 1), vals$certain_dfs)
          }
        }
      }
    })

    observeEvent(input$next_row, {
      # Production mode
      dfA <- state$state_dfA
      dfB <- state$state_dfB
      matched_dfs <- state$matched_results$matched_dfs

      if (!is.null(matched_dfs) && tibble::is_tibble(matched_dfs)) {
        if (!is.null(vals$uncertain_dfs) && nrow(vals$uncertain_dfs) > 0) {
          if (vals$current_reviewing < nrow(vals$uncertain_dfs)) {

            vals$current_reviewing <- vals$current_reviewing + 1

            currentA <- dfA[vals$uncertain_dfs$inds.a[vals$current_reviewing],]
            currentB <- dfB[vals$uncertain_dfs$inds.b[vals$current_reviewing],]

            update_review_data(
              level          = vals$uncertain_dfs$match_level[vals$current_reviewing],
              decision       = vals$uncertain_dfs$manual_selection[vals$current_reviewing],
              firstname_dfA  = currentA[["firstname"]],
              firstname_dfB  = currentB[["firstname"]],
              middlename_dfA = currentA[["middlename"]],
              middlename_dfB = currentB[["middlename"]],
              lastname_dfA   = currentA[["lastname"]],
              lastname_dfB   = currentB[["lastname"]],
              birthday_dfA   = currentA[["birthday"]],
              birthday_dfB   = currentB[["birthday"]],
              race_dfA       = currentA[["race"]],
              race_dfB       = currentB[["race"]],
              sex_dfA        = currentA[["sex"]],
              sex_dfB        = currentB[["sex"]],
              housenum_dfA   = currentA[["housenum"]],
              housenum_dfB   = currentB[["housenum"]],
              streetname_dfA = currentA[["streetname"]],
              streetname_dfB = currentB[["streetname"]],
              city_dfA       = currentA[["city"]],
              city_dfB       = currentB[["city"]],
              SSN_dfA        = currentA[["SSN"]],
              SSN_dfB        = currentB[["SSN"]]
            )

            state$matched_results[['matched_intersect']] <- dplyr::bind_rows(
              vals$uncertain_dfs %>% dplyr::filter(manual_selection == 1), vals$certain_dfs)
          }
        }
      }
    })

    uncertainty_matches <- reactive({
      if (!is.null(vals$uncertain_dfs)){
        vals$uncertain_dfs %>% dplyr::filter(is.na(manual_selection))
      } else {NULL}
    })

    output[["uncertainty_matches"]] <- DT::renderDataTable(
      uncertainty_matches(),
      caption = 'uncertainty_matches',
      extensions = 'Buttons',
      selection = "multiple",
      rownames = FALSE,
      server = FALSE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50, -1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "Uncertainty matches"),
              list(extend = 'excel', filename = "Uncertainty matches")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    confirmed_matches <- reactive({
      if (!is.null(vals$uncertain_dfs)){
        vals$uncertain_dfs %>% dplyr::filter(manual_selection == 0)
      } else {NULL}
    })

    output[["confirmed_matches"]] <- DT::renderDataTable(
      confirmed_matches(),
      caption = 'uncertainty_matches',
      extensions = 'Buttons',
      selection = "multiple",
      rownames = FALSE,
      server = FALSE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50, -1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "Uncertainty matches"),
              list(extend = 'excel', filename = "Uncertainty matches")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    confirmed_non_matches <- reactive({
      if (!is.null(vals$uncertain_dfs)){
        vals$uncertain_dfs %>% dplyr::filter(manual_selection == 0)
      } else {NULL}
    })

    output[["confirmed_non_matches"]] <- DT::renderDataTable(
      confirmed_non_matches(),
      caption = 'uncertainty_matches',
      extensions = 'Buttons',
      selection = "multiple",
      rownames = FALSE,
      server = FALSE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50, -1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "Uncertainty matches"),
              list(extend = 'excel', filename = "Uncertainty matches")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )


  })
}

## To be copied in the UI
# mod_manual_inspection_ui("manual_inspection_1")

## To be copied in the server
# mod_manual_inspection_server("manual_inspection_1")
