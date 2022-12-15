#' cleaning_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cleaning_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Gender Race Recoding",
      status = "success",
      solidHeader = FALSE,
      collapsible = TRUE,
      fluidRow(column(
        width = 6, helpText("Recode the gender and race in both data sets")
      ), column(
        width = 6,
        actionBttn(
          inputId = ns("load_gender_race_coding"),
          label = "Load Current Gender/Race Coding information",
          style = "simple",
          color = "primary",
          # icon = icon("bars"),
          size = "sm"
        )
      ))
    ),
    ## Box for Sample data Recoding
    fluidRow(column(
      width = 6,
      box(
        width = 12,
        title = "Sample Recoding",
        status = "orange",
        solidHeader = FALSE,
        collapsible = TRUE,
        helpText("Assign values for Gender"),
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = ns("recoding_male_a"),
              label = "Male Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("recoding_female_a"),
              label = "Female Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("recoding_transgender_a"),
              label = "Transergender Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          )
        ),
        helpText("Assign values for Race/Ethnicity"),
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_cauc_a"),
              label = "Cauc. Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_afric_a"),
              label = "Afric Amer Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_hisp_a"),
              label = "Hispanic Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_other_a"),
              label = "Other Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_asian_a"),
              label = "Asian Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_native_a"),
              label = "Native Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_mid_a"),
              label = "Mid East Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          )
        )
      )
    ),
    ## Box for Matching Data Recoding
    column(
      width = 6,
      box(
        width = 12,
        title = "Matching data Recoding",
        status = "maroon",
        solidHeader = FALSE,
        collapsible = TRUE,
        helpText("Assign values for Gender"),
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = ns("recoding_male_b"),
              label = "Male Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("recoding_female_b"),
              label = "Female Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("recoding_transgender_b"),
              label = "Transergender Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          )
        ),
        helpText("Assign values for Race/Ethnicity"),
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_cauc_b"),
              label = "Cauc. Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_afric_b"),
              label = "Afric Amer Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_hisp_b"),
              label = "Hispanic Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_other_b"),
              label = "Other Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_asian_b"),
              label = "Asian Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_native_b"),
              label = "Native Values",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("recoding_mid_b"),
              label = "Mid East Values",
              choices =NULL,
              multiple = TRUE,
              selected = NULL
            )
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
        column(12, DT::dataTableOutput(ns('gender_dfA'), width = "100%"))
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
        column(12, DT::dataTableOutput(ns('gender_dfB'), width = "100%"))
      )
    )),
    fluidRow(
      column(
        width = 6,
        actionBttn(
          inputId = ns("previous_assignment"),
          label = "Previous: Assign Variables",
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
          inputId = ns("next_date_format"),
          label = "Next: Format Dates",
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

#' cleaning_gender Server Functions
#' @importFrom shinyWidgets sendSweetAlert
#' @noRd
mod_cleaning_gender_server <- function(id, state, parent){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # library(magrittr)


    observeEvent(input$load_gender_race_coding, {

      req(state$dfA_cleaned_assignment)

      gender_A <- unique(state$dfA_cleaned_assignment$sex)
      # print(gender_A)
      race_A <- unique(state$dfA_cleaned_assignment$race)
      # print(race_A)

      updateSelectInput(session, "recoding_male_a",
                        choices = c('', gender_A))
      updateSelectInput(session, "recoding_female_a",
                        choices = c('', gender_A))
      updateSelectInput(session, "recoding_transgender_a",
                        choices = c('', gender_A))


      updateSelectInput(session, "recoding_cauc_a",
                        choices = c('', race_A))
      updateSelectInput(session, "recoding_afric_a",
                        choices = c('', race_A))
      updateSelectInput(session, "recoding_hisp_a",
                        choices = c('', race_A))

      updateSelectInput(session, "recoding_asian_a",
                        choices = c('', race_A))
      updateSelectInput(session, "recoding_native_a",
                        choices = c('', race_A))
      updateSelectInput(session, "recoding_mid_a",
                        choices = c('', race_A))

      updateSelectInput(session, "recoding_other_a",
                        choices = c('', race_A))


      req(state$dfB_cleaned_assignment)

      gender_B <- unique(state$dfB_cleaned_assignment$sex)
      # print(gender_B)
      race_B <- unique(state$dfB_cleaned_assignment$race)
      # print(race_B)

      updateSelectInput(session, "recoding_male_b",
                        choices = c('', gender_B))
      updateSelectInput(session, "recoding_female_b",
                        choices = c('', gender_B))
      updateSelectInput(session, "recoding_transgender_b",
                        choices = c('', gender_B))


      updateSelectInput(session, "recoding_cauc_b",
                        choices = c('', race_B))
      updateSelectInput(session, "recoding_afric_b",
                        choices = c('', race_B))
      updateSelectInput(session, "recoding_hisp_b",
                        choices = c('', race_B))

      updateSelectInput(session, "recoding_asian_b",
                        choices = c('', race_B))
      updateSelectInput(session, "recoding_native_b",
                        choices = c('', race_B))
      updateSelectInput(session, "recoding_mid_b",
                        choices = c('', race_B))

      updateSelectInput(session, "recoding_other_b",
                        choices = c('', race_B))

      sendSweetAlert(
        session = session,
        title = "Success!",
        text = "Now assign values for race and gender using the dropdown menus in each field",
        type = "success"
      )

    })

    gender_dataset_a <- reactive({

      req(state$dfA_cleaned_assignment)
      data <- state$dfA_cleaned_assignment

      data %<>% dplyr::mutate(sex = replace(sex, sex %in% input$recoding_male_a, "male"))
      data %<>% dplyr::mutate(sex = replace(sex, sex %in% input$recoding_female_a, "female"))
      data %<>% dplyr::mutate(sex = replace(sex, sex %in% input$recoding_transgender_a, "Transgender"))

      data %<>% dplyr::mutate(race = replace(race, race %in% input$recoding_cauc_a, "Caucasian"))
      data %<>% dplyr::mutate(race = replace(race, race %in% input$recoding_afric_a, "Afican American"))
      data %<>% dplyr::mutate(race = replace(race, race %in% input$recoding_hisp_a, "Hispanic"))

      data %<>% dplyr::mutate(race = replace(race, race %in% input$recoding_asian_a, "Asian"))
      data %<>% dplyr::mutate(race = replace(race, race %in% input$recoding_native_a, "Native"))
      data %<>% dplyr::mutate(race = replace(race, race %in% input$recoding_mid_a, "Mid East"))

      data %<>% dplyr::mutate(race = replace(race, race %in% input$recoding_other_a, "Other"))
      state$dfA_cleaned_gender <- data # update state

      return(data)
    })

    gender_dataset_b <- reactive({

      req(state$dfB_cleaned_assignment)
      data <- state$dfB_cleaned_assignment

      data %<>% dplyr::mutate(sex = replace(sex, sex %in% input$recoding_male_b, "male"))
      data %<>% dplyr::mutate(sex = replace(sex, sex %in% input$recoding_female_b, "female"))
      data %<>% dplyr::mutate(sex = replace(sex, sex %in% input$recoding_transgender_b, "Transgender"))

      data %<>% dplyr::mutate(race = replace(race, race %in% input$recoding_cauc_b, "Caucasian"))
      data %<>% dplyr::mutate(race = replace(race, race %in% input$recoding_afric_b, "Afican American"))
      data %<>% dplyr::mutate(race = replace(race, race %in% input$recoding_hisp_b, "Hispanic"))

      data %<>% dplyr::mutate(race = replace(race, race %in% input$recoding_asian_b, "Asian"))
      data %<>% dplyr::mutate(race = replace(race, race %in% input$recoding_native_b, "Native"))
      data %<>% dplyr::mutate(race = replace(race, race %in% input$recoding_mid_b, "Mid East"))

      data %<>% dplyr::mutate(race = replace(race, race %in% input$recoding_other_b, "Other"))
      state$dfB_cleaned_gender <- data # update state

      return(data)
    })


    output$gender_dfA <- DT::renderDataTable(
      gender_dataset_a(),
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

    output$gender_dfB <- DT::renderDataTable(
      gender_dataset_b(),
      caption = 'Data in the Matching data set',
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
    observeEvent(input$previous_assignment, {
      updateTabItems(session = parent, "tabs", "assignment")
    })

    # Next page button redirection
    observeEvent(input$next_date_format, {
      updateTabItems(session = parent, "tabs", "date_format")
    })
  })
}

## To be copied in the UI
# mod_cleaning_gender_ui("cleaning_gender_1")

## To be copied in the server
# mod_cleaning_gender_server("cleaning_gender_1")
