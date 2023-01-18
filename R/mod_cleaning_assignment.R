#' cleaning_assignment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cleaning_assignment_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Assign Variables",
      status = "success",
      solidHeader = FALSE,
      collapsible = FALSE,
      helpText("Identify which variables correspond to each piece of information")
      ),
    fluidRow(column(
      width = 6,
      box(
        width = 12,
        title = "Sample data set",
        status = "orange",
        solidHeader = FALSE,
        collapsible = FALSE,
        fluidRow(
          column(
            width = 6,
            selectInput(ns("firstname_dfA"),
                        "Firstname:",
                        choices = NULL),
            selectInput(ns("middlename_dfA"),
                        "Middlename",
                        choices = NULL),
            selectInput(ns("lastname_dfA"),
                        "Lastname",
                        choices = NULL),
            selectInput(ns("birthday_dfA"),
                        "Birthday",
                        choices = NULL),
            selectInput(ns("race_dfA"),
                        "Race",
                        choices = NULL)
          ),
          column(
            width = 6,
            selectInput(ns("sex_dfA"),
                        "Sex",
                        choices = NULL),
            selectInput(ns("housenum_dfA"),
                        "Housenum",
                        choices = NULL),
            selectInput(ns("streetname_dfA"),
                        "Streetname",
                        choices = NULL),
            selectInput(ns("city_dfA"),
                        "City",
                        choices = NULL),
            selectInput(ns("birthyear_dfA"),
                        "Birthyear",
                        choices = NULL)
          )
        )
      )
    ),
    column(
      width = 6,
      box(
        width = 12,
        title = "Matching data set",
        status = "maroon",
        solidHeader = FALSE,
        collapsible = FALSE,
        fluidRow(
          column(
            width = 6,
            selectInput(ns("firstname_dfB"),
                        "Firstname:",
                        choices = NULL),
            selectInput(ns("middlename_dfB"),
                        "Middlename",
                        choices = NULL),
            selectInput(ns("lastname_dfB"),
                        "Lastname",
                        choices = NULL),
            selectInput(ns("birthday_dfB"),
                        "Birthday",
                        choices = NULL),
            selectInput(ns("race_dfB"),
                        "Race",
                        choices = NULL)
          ),
          column(
            width = 6,
            selectInput(ns("sex_dfB"),
                        "Sex",
                        choices = NULL),
            selectInput(ns("housenum_dfB"),
                        "Housenum",
                        choices = NULL),
            selectInput(ns("streetname_dfB"),
                        "Streetname",
                        choices = NULL),
            selectInput(ns("city_dfB"),
                        "City",
                        choices = NULL),
            selectInput(ns("birthyear_dfB"),
                        "Birthyear",
                        choices = NULL)
          )
        )
      )
    )),
    fluidRow(column(
      width = 6,
      box(
        width = 12,
        title = "Sample data set",
        status = "orange",
        solidHeader = FALSE,
        collapsible = TRUE,
        column(12, DT::dataTableOutput(ns('assigned_dfA'), width = "100%"))
      )
    ),
    column(
      width = 6,
      box(
        width = 12,
        title = "Matching data set",
        status = "maroon",
        solidHeader = FALSE,
        collapsible = TRUE,
        column(12, DT::dataTableOutput(ns('assigned_dfB'), width = "100%"))
      )
    )),
    fluidRow(
      column(
        width = 6,
        actionBttn(
          inputId = ns("previous_duplicate"),
          label = "Previous: Remove Duplicate Rows",
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
          inputId = ns("next_gender_race"),
          label = "Next: Recode Race & Gender",
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

#' cleaning_assignment Server Functions
#' @importFrom shinyWidgets sendSweetAlert
#' @noRd
mod_cleaning_assignment_server <- function(id, state, parent){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # library(magrittr)
    # pipe operator friendly set column names function

    set_col_names <- function(.data,
                              firstname = NULL,
                              middlename = NULL,
                              lastname = NULL,
                              housenum = NULL,
                              streetname = NULL,
                              city = NULL,
                              birthyear = NULL,
                              birthday = NULL,
                              race = NULL,
                              sex = NULL) {
      # message(str(.data))
      if (!is.null(firstname) && firstname != "") {
        .data <- dplyr::rename(.data, firstname = firstname)
      }
      if (!is.null(middlename) && middlename != "") {
        .data <- dplyr::rename(.data, middlename = middlename)
      }
      if (!is.null(lastname) && lastname != "") {
        .data <- dplyr::rename(.data, lastname = lastname)
      }
      if (!is.null(housenum) && housenum != "") {
        .data <- dplyr::rename(.data, housenum = housenum)
      }
      if (!is.null(streetname) && streetname != "") {
        .data <- dplyr::rename(.data, streetname = streetname)
      }
      if (!is.null(city) && city != "") {
        .data <- dplyr::rename(.data, city = city)
      }
      if (!is.null(birthyear) && birthyear != "") {
        .data <- dplyr::rename(.data, birthyear = birthyear)
      }
      if (!is.null(birthday) && birthday != "") {
        .data <- dplyr::rename(.data, birthday = birthday)
      }
      if (!is.null(race) && race != "") {
        .data <- dplyr::rename(.data, race = race)
      }
      if (!is.null(sex) && sex != "") {
        .data <- dplyr::rename(.data, sex = sex)
      }
      .data
    }

    observe({

      req(state$dfA_cleaned_duplicate)
      req(state$dfB_cleaned_duplicate)

      data_dfA <- state$dfA_cleaned_duplicate

      colnames_dfA <- colnames(data_dfA)
      # set the label and select items
      updateSelectInput(session, "firstname_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "middlename_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "lastname_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "housenum_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "streetname_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "city_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "birthyear_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "birthday_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "race_dfA",
                        choices = c('', colnames_dfA))
      updateSelectInput(session, "sex_dfA",
                        choices = c('', colnames_dfA))

      data_dfB <- state$dfB_cleaned_duplicate

      colnames_dfB <- colnames(data_dfB)
      # set the label and select items
      updateSelectInput(session, "firstname_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "middlename_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "lastname_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "housenum_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "streetname_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "city_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "birthyear_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "birthday_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "race_dfB",
                        choices = c('', colnames_dfB))
      updateSelectInput(session, "sex_dfB",
                        choices = c('', colnames_dfB))

      # sendSweetAlert(
      #   session = session,
      #   title = "Success!",
      #   text = "Now assign values for columns using the dropdown menus in each field",
      #   type = "success"
      # )
    })

    assigned_dataset_a <- reactive({

      req(state$dfA_cleaned_duplicate)

      data <- state$dfA_cleaned_duplicate %>% set_col_names(
        firstname = input$firstname_dfA,
        middlename = input$middlename_dfA,
        lastname = input$lastname_dfA,
        housenum = input$housenum_dfA,
        streetname = input$streetname_dfA,
        city = input$city_dfA,
        birthyear = input$birthyear_dfA,
        birthday = input$birthday_dfA,
        race = input$race_dfA,
        sex = input$sex_dfA
      )

      state$dfA_cleaned_assignment <- data # update state
      return(data)
    })

    assigned_dataset_b <- reactive({

      req(state$dfB_cleaned_duplicate)

      data <- state$dfB_cleaned_duplicate %>% set_col_names(
        firstname = input$firstname_dfB,
        middlename = input$middlename_dfB,
        lastname = input$lastname_dfB,
        housenum = input$housenum_dfB,
        streetname = input$streetname_dfB,
        city = input$city_dfB,
        birthyear = input$birthyear_dfB,
        birthday = input$birthday_dfB,
        race = input$race_dfB,
        sex = input$sex_dfB
      )

      state$dfB_cleaned_assignment <- data # update state
      return(data)
    })


    output$assigned_dfA <- DT::renderDataTable(
      assigned_dataset_a(),
      #  caption = '',
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

    output$assigned_dfB <- DT::renderDataTable(
      assigned_dataset_b(),
      #  caption = '',
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
    observeEvent(input$previous_duplicate, {
      updateTabItems(session = parent, "tabs", "duplicate")
    })

    # Next page button redirection
    observeEvent(input$next_gender_race, {
      updateTabItems(session = parent, "tabs", "gender_race")
    })
  })
}

## To be copied in the UI
# mod_cleaning_assignment_ui("cleaning_assignment_1")

## To be copied in the server
# mod_cleaning_assignment_server("cleaning_assignment_1")
