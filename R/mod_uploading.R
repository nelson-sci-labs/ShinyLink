#' uploading UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_uploading_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Inputs",
      status = "success",
      solidHeader = FALSE,
      fluidRow(
        column(
          width = 3,
          fileInput(
            inputId = ns("file_path_dfA"),
            label = "Select Sample data set",
            multiple = FALSE,
            accept = c(
              # "text/csv",
              # "text/comma-separated-values,text/plain",
              ".csv",
              ".xlsx",
              ".sas7bdat",
              ".sav",
              ".dta",
              ".tsv"
            )
          ),
          fileInput(
            inputId = ns("file_path_dfB"),
            label = "Select Matching data set",
            multiple = FALSE,
            accept = c(
              # "text/csv",
              # "text/comma-separated-values,text/plain",
              ".csv",
              ".xlsx",
              ".sas7bdat",
              ".sav",
              ".dta",
              ".tsv"
            )
          ),
          HTML("<p><i>Supported formats: Excel, csv, tsv, SAS, SPSS, Stata.</i></p>")
        ),
        column(
          width = 3,
          fluidRow(
            HTML("<h5><b>Download our demo data</b></h5>"),
            HTML("<h5>Sample data</h5>"),
            HTML(
              "<a href='https://raw.githubusercontent.com/nelson-sci-labs/ShinyLink/main/inst/app/www/lkselectedrecs.xlsx'> <i class='fa fa-download'> </i> xlsx</a>"
            ),
            HTML(
              "<a href='https://raw.githubusercontent.com/nelson-sci-labs/ShinyLink/main/inst/app/www/lkselectedrecs.sas7bdat'> <i class='fa fa-download'> </i> sas7bdat</a>"
            ),
            HTML(
              "<a href='https://raw.githubusercontent.com/nelson-sci-labs/ShinyLink/main/inst/app/www/lkselectedrecs.sav'> <i class='fa fa-download'> </i> sav</a>"
            ),
            HTML(
              "<a href='https://raw.githubusercontent.com/nelson-sci-labs/ShinyLink/main/inst/app/www/lkselectedrecs.dta'> <i class='fa fa-download'> </i> dta</a>"
            ),
            # HTML(
            #   "<a href='https://raw.githubusercontent.com/nelson-sci-labs/ShinyLink/main/inst/app/www/lkselectedrecs.csv'> <i class='fa fa-download'> </i> csv</a>"
            # ),
            br(),
            br(),
            HTML("<h5>Matching data</h5>"),
            HTML(
              "<a href='https://raw.githubusercontent.com/nelson-sci-labs/ShinyLink/main/inst/app/www/redcapoutput.xlsx'> <i class='fa fa-download'> </i> xlsx</a>"
            ),
            HTML(
              "<a href='https://raw.githubusercontent.com/nelson-sci-labs/ShinyLink/main/inst/app/www/redcapoutput.sas7bdat'> <i class='fa fa-download'> </i> sas7bdat</a>"
            ),
            HTML(
              "<a href='https://raw.githubusercontent.com/nelson-sci-labs/ShinyLink/main/inst/app/www/redcapoutput.sav'> <i class='fa fa-download'> </i> sav</a>"
            ),
            HTML(
              "<a href='https://raw.githubusercontent.com/nelson-sci-labs/ShinyLink/main/inst/app/www/redcapoutput.dta'> <i class='fa fa-download'> </i> dta</a>"
            ),
            # HTML(
            #   "<a href='https://raw.githubusercontent.com/nelson-sci-labs/ShinyLink/main/inst/app/www/redcapoutput.csv'> <i class='fa fa-download'> </i> csv</a>"
            # ),
            br(),
            br(),
            HTML(
              "<p><i>Optimized for 1980 x 1080 resolution screen (1080p) and Google Chrome Web Browser Version 106.0.5249.119.</i></p>"
            )
          )
        ),
        column(
          width = 3,
          descriptionBlock(
            number = "17%",
            numberColor = "green",
            numberIcon = icon("caret-up"),
            header = "168",
            text = "Total Entries",
            rightBorder = TRUE,
            marginBottom = TRUE
          ),
          descriptionBlock(
            number = "58%",
            numberColor = "red",
            numberIcon = icon("caret-down"),
            header = "42",
            text = "Percent of Differences",
            rightBorder = TRUE,
            marginBottom = FALSE
          )
        ),
        column(
          width = 3,
          boxPad(
            color = "green",
            descriptionBlock(
              header = "85",
              text = "Unique Source IDs",
              rightBorder = FALSE,
              marginBottom = TRUE
            ),
            descriptionBlock(
              header = "73",
              text = "Unique Matching IDs",
              rightBorder = FALSE,
              marginBottom = TRUE
            ),
            descriptionBlock(
              header = "35%",
              text = "Percentage Matching",
              rightBorder = FALSE,
              marginBottom = FALSE
            )
          )
        )
      )
    ),
    fluidRow(column(
      width = 6,
      box(
        width = 12,
        title = "Sample data set",
        status = "orange",
        solidHeader = FALSE,
        collapsible = TRUE,
        column(12, DT::dataTableOutput(outputId = ns('upload_dfA'), width = "100%"))
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
        column(12, DT::dataTableOutput(outputId = ns('upload_dfB'), width = "100%"))
      )
    )),
    fluidRow(
      column(
        width = 6,
        # actionBttn(
        #   inputId = ns("Previous"),
        #   label = "Previous",
        #   style = "simple",
        #   color = "primary",
        #   icon = icon("arrow-left"),
        #   size = "sm"
        # ),
        align = "left",
        style = "margin-bottom: 10px;",
        style = "margin-top: -10px;"
      ),
      column(
        width = 6,
        actionBttn(
          inputId = ns("next_duplicate"),
          label = "Next: Remove Duplicate Rows",
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

#' uploading Server Functions
#'
#' @importFrom shinydashboard updateTabItems
#'
#' @noRd

mod_uploading_server <- function(id, state, parent){

  # File uploading limit: 9MB
  options(shiny.maxRequestSize = 9*1024^2)

  moduleServer( id, function(input, output, session){
    ns <- session$ns
    load_file <- function(name, path) {
      ext <- tools::file_ext(name)
      switch(ext,
             csv      = vroom::vroom(path, delim = ",", col_types = list()),
             tsv      = vroom::vroom(path, delim = "\t", col_types = list()),
             sas7bdat = haven::read_sas(path),
             sav      = haven::read_sav(path),
             dta      = haven::read_dta(path),
             xlsx     = readxl::read_excel(path),
             validate("Invalid file; Please upload a .csv or .tsv file")
      )
    }

    dfA <- reactive({
      req(input$file_path_dfA)

      dfA <- load_file(basename(input$file_path_dfA$datapath),
                  input$file_path_dfA$datapath)

      state$dfA_uploaded <- dfA
      return(dfA)
    })

    dfB <- reactive({
      req(input$file_path_dfB)

      dfB <- load_file(basename(input$file_path_dfB$datapath),
                  input$file_path_dfB$datapath)

      state$dfB_uploaded <- dfB
      return(dfB)
    })

    output$upload_dfA <- DT::renderDataTable(
      dfA(),
      #  caption = 'Data in the Sample data set',
      extensions = 'Buttons',
      selection = "single",
      rownames = FALSE,
      server = FALSE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(13, 20, 50,-1), c('default', '20', '50', 'All')),
        pageLength = 13,
        dom = 'Blfrtip',
        buttons =
          list(
            "copy",
            list(
              extend = "collection",
              buttons = c("csv", "excel", "pdf"),
              text = "Download"
            )
          )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    output$upload_dfB <- DT::renderDataTable(
      dfB(),
      #  caption = 'Data in the Matching data set',
      extensions = 'Buttons',
      selection = "single",
      rownames = FALSE,
      server = FALSE,
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(13, 20, 50,-1), c('default', '20', '50', 'All')),
        pageLength = 13,
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

    # Next page button redirection
    observeEvent(input$next_duplicate, {
      # Accessing parent namespace inside the Module
      updateTabItems(session = parent,
                     inputId = "tabs",
                     selected = "duplicate")
    })
  })
}




## To be copied in the UI
# mod_uploading_ui("uploading_1")

## To be copied in the server
# mod_uploading_server("uploading_1")
