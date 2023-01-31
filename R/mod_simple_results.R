#' simple_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simple_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(
      6,
      box(
        width = 12,
        height = '20em',
        title = "Start Simple Matching",
        status = "success",
        solidHeader = FALSE,
        collapsible = FALSE,
        helpText("Click this button to execute matching, and your results will appear after a few seconds."),
        actionButton(ns("match"), "Simple Match", class = "btn-success"),
        # hr(),
        # br(),
        # tags$label("Selected row(s) of Matching Results table, and the results will reflected in the next detailed page"),
        # fluidRow(column(6, verbatimTextOutput(ns("info-main"))),
        #          column(
        #            6,
        #            downloadButton(ns("download_selected"), "Download Selected")
        #          )),
        hr(),
        tags$label("Summary of matching results:"),
        verbatimTextOutput(ns("matched-summary"))
      )
    ),
    column(
      6,
      box(
        width = 12,
        height = '20em',
        title = "Matching Summary",
        status = "success",
        solidHeader = FALSE,
        collapsible = FALSE,
        plotOutput(ns("plot-venn"), height = "268px")
      )
    )),
    box(
      width = 12,
      title = "Matching Results",
      status = "success",
      solidHeader = FALSE,
      collapsible = FALSE,
      tags$label("Selected row(s) of Matching Results table, and the results will be reflected in the Matching Details page"),
      column(12, DT::dataTableOutput(ns('matched')))
    ),
    fluidRow(
      column(
        width = 6,
        actionBttn(
          inputId = ns("previous_simple_settings"),
          label = "Previous: Simple Match Settings",
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
          inputId = ns("next_simple_details"),
          label = "Next: Simple Match Details",
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

#' simple_results Server Functions
#' @import fastLink ggplot2 ggvenn dplyr grid
#' @importFrom DT renderDT datatable
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom htmlwidgets JS
#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom utils capture.output write.csv
#' @noRd
mod_simple_results_server <- function(id, state, parent){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ## the callback ####
    registerInputHandler("x.child", function(x, ...) {
      fromJSON(toJSON(x, auto_unbox = TRUE, null = "null"),
               simplifyDataFrame = FALSE)
    }, force = TRUE)
    callback = JS(
      "var expandColumn = table.column(0).data()[0] === 'plus-sign' ? 0 : 1;",
      "table.column(expandColumn).nodes().to$().css({cursor: 'pointer'});",
      "",
      "// send selected columns of the main table to Shiny",
      "var tbl = table.table().node();",
      "var tblId = $(tbl).closest('.datatables').attr('id');",
      "var selector = 'td:not(:nth-child(' + (expandColumn+1) + '))';",
      "table.on('click', selector, function(){",
      "  setTimeout(function(){",
      "    var indexes = table.rows({selected:true}).indexes();",
      "    var indices = Array(indexes.length);",
      "    for(var i = 0; i < indices.length; ++i){",
      "      indices[i] = indexes[i];",
      "    }",
      "    Shiny.setInputValue(tblId + '_rows_selected', indices);",
      "  },0);",
      "});",
      "",
      "// make the table header of the nested table",
      "var format = function(d, childId){",
      "  if(d != null){",
      "    var html = '<table class=\"compact hover\" id=\"' + ",
      "                childId + '\"><thead><tr>';",
      "    for(var key in d[d.length-1][0]){",
      "      html += '<th>' + key + '</th>';",
      "    }",
      "    html += '</tr></thead></table>'",
      "    return html;",
      "  } else {",
      "    return '';",
      "  }",
      "};",
      "",
      "// row callback to style the rows background colors of the child tables",
      "var rowCallback = function(row, dat, displayNum, index){",
      "  if($(row).hasClass('odd')){",
      "    $(row).css('background-color', 'papayawhip');",
      "    $(row).hover(function(){",
      "      $(this).css('background-color', '#E6FF99');",
      "    }, function() {",
      "      $(this).css('background-color', 'papayawhip');",
      "    });",
      "  } else {",
      "    $(row).css('background-color', 'lemonchiffon');",
      "    $(row).hover(function(){",
      "      $(this).css('background-color', '#DDFF75');",
      "    }, function() {",
      "      $(this).css('background-color', 'lemonchiffon');",
      "    });",
      "  }",
      "};",
      "",
      "// header callback to style the header of the child tables",
      "var headerCallback = function(thead, data, start, end, display){",
      "  $('th', thead).css({",
      "    'border-top': '3px solid indigo',",
      "    'color': 'indigo',",
      "    'background-color': '#fadadd'",
      "  });",
      "};",
      "",
      "// make the child table",
      "var format_datatable = function(d, childId){",
      "  var dataset = [];",
      "  var n = d.length - 1;",
      "  for(var i = 0; i < d[n].length; i++){",
      "    var datarow = $.map(d[n][i], function(value, index){",
      "      return [value];",
      "    });",
      "    dataset.push(datarow);",
      "  }",
      "  var id = 'table#' + childId;",
      "  var subtable = $(id).DataTable({",
      "             'data': dataset,",
      "             'autoWidth': true,",
      "             'deferRender': true,",
      "             'info': false,",
      "             'lengthChange': false,",
      "             'ordering': d[n].length > 1,",
      "             'order': [],",
      "             'paging': false,",
      "             'scrollX': false,",
      "             'scrollY': false,",
      "             'searching': false,",
      "             'sortClasses': false,",
      "             'rowCallback': rowCallback,",
      "             'headerCallback': headerCallback,",
      "             'select': {style: 'multi'},",
      "             'columnDefs': [{targets: '_all', className: 'dt-center'}]",
      "           });",
      "};",
      "",
      "// send selected rows of the children tables to shiny server",
      "var nrows = table.rows().count();",
      "var nullinfo = Array(nrows);",
      "for(var i = 0; i < nrows; ++i){",
      "  nullinfo[i] = {row: i, selected: null};",
      "}",
      "Shiny.setInputValue(tblId + '_children:x.child', nullinfo);",
      "var sendToR = function(){",
      "  var info = [];",
      "  setTimeout(function(){",
      "    for(var i = 0; i < nrows; ++i){",
      "      var childId = 'child-' + i;",
      "      var childtbl = $('#'+childId).DataTable();",
      "      var indexes = childtbl.rows({selected:true}).indexes();",
      "      var indices;",
      "      if(indexes.length > 0){",
      "        indices = Array(indexes.length);",
      "        for(var j = 0; j < indices.length; ++j){",
      "          indices[j] = indexes[j];",
      "        }",
      "      } else {",
      "        indices = null;",
      "      }",
      "      info.push({row: i, selected: indices});",
      "    }",
      "    Shiny.setInputValue(tblId + '_children:x.child', info);",
      "  }, 0);",
      "}",
      "$('body').on('click', '[id^=child-] td', sendToR);",
      "",
      "// click event to show/hide the child tables",
      "table.on('click', 'td.details-control', function () {",
      "  var cell = table.cell(this);",
      "      row = table.row($(this).closest('tr'));",
      "  if(row.child.isShown()){",
      "    row.child.hide();",
      "    cell.data('expand');",
      "    sendToR();",
      "  } else {",
      "    var childId = 'child-' + row.index();",
      "    row.child(format(row.data(), childId)).show();",
      "    row.child.show();",
      "    cell.data('collapse-down');",
      "    format_datatable(row.data(), childId);",
      "  }",
      "});"
    )
    # Render function, to display the glyphicons ------------------------------
    render <- c(
      "function(data, type, row, meta){",
      "  if(type === 'display'){",
      "    return '<span style=\\\"color:black; font-size:18px\\\">' + ",
      "       '<i class=\\\"glyphicon glyphicon-' + data + '\\\"></i></span>';",
      "  } else {",
      "    return data;",
      "  }",
      "}"
    )

    # Simple Matching ---------------------------------------------------------
    matched_values <- eventReactive(input$match, {

      req(state$state_dfA)
      req(state$state_dfB)
      dfA <- state$state_dfA
      dfB <- state$state_dfB

      # Testing only
      # dfA <- readxl::read_excel('inst/app/www/lkselectedrecs_cleaned.xlsx')
      # dfB <- readxl::read_excel('inst/app/www/redcapoutput_cleaned.xlsx')
      # dfA <- readxl::read_excel('inst/app/www/Unique in Sample Data Set.xlsx')
      # dfB <- readxl::read_excel('inst/app/www/Unique in Matching Data Set.xlsx')
      # matches.out <- fastLink::fastLink(
      #   dfA = dfA, dfB = dfB,
      #   varnames = c("firstname", "middlename", "lastname", "race", "sex"),
      #   # stringdist.match = c("firstname", "middlename", "lastname", "race", "sex"),
      #   # numeric.match =
      #   # partial.match = c("firstname", "lastname"),
      #   n.cores = 1
      # )

      matches.out <- fastLink::fastLink(
        dfA = dfA,
        dfB = dfB,
        varnames = state$matching_variables,
        stringdist.match = state$string_matching,
        numeric.match = state$numeric_matching,
        partial.match = state$partial_matching
      )

      print(length(matches.out$matches$inds.a))
      print(length(matches.out$matches$inds.a) == 0)
      print(length(matches.out$matches$inds.a) != 0)

      if (length(matches.out$matches$inds.a) == 0) {
        matched_results <- list(
          Dat = tibble::tibble(),
          matches.out = NULL,
          matched_summary = NULL,
          dfA.match = NULL,
          dfA.unmatch = NULL,
          dfB.match = NULL,
          dfB.unmatch = NULL,
          matched_union = NULL
        )
        state$matched_results <- matched_results
        sendSweetAlert(
          session = session,
          title = "",
          text = "No matches found",
          type = "warning"
        )
      }

      if (length(matches.out$matches$inds.a) != 0) {

        dfA.match <- dfA[matches.out$matches$inds.a, ]
        dfA.unmatch <- dfA[-matches.out$matches$inds.a, ]
        dfB.match <- dfB[matches.out$matches$inds.b, ]
        dfB.unmatch <- dfB[-matches.out$matches$inds.b, ]


        matched_dfs <- fastLink::getMatches(
          dfA = dfA,
          dfB = dfB,
          fl.out = matches.out,
          threshold.match = 0.85
        )
        matched_dfs <- matched_dfs %>%
          dplyr::select(-tidyselect::any_of(
            c(
              'gamma.1',
              'gamma.2',
              'gamma.3',
              'gamma.4',
              'gamma.5',
              'gamma.6'
              # 'posterior'
            )
          ))

        subdat <- list()

        varnames <- state$matching_variables

        for (i in 1:nrow(matches.out$matches)) {
          dfA_current <-  dfA %>% dplyr::select(varnames)
          dfA_current <-
            dfA_current[matches.out$matches[i,]$inds.a,]
          dfA_current <- dfA_current %>%
            dplyr::mutate(`Data source` = "Sample Dataset", .before = colnames(dfA_current)[1])

          dfB_current <-  dfB %>% dplyr::select(varnames)
          dfB_current <-
            dfB_current[matches.out$matches[i,]$inds.b,]
          dfB_current <- dfB_current %>%
            dplyr::mutate(`Data source` = "Matching Dataset", .before = colnames(dfA_current)[1])

          subdat[[i]] <-
            dplyr::as_tibble(dplyr::bind_rows(dfA_current, dfB_current))

          if ("birthday" %in% colnames(subdat[[i]])) {
            subdat[[i]]$birthday <- as.character(subdat[[i]]$birthday)
          }


        }

        subdats <- lapply(subdat, purrr::transpose)
        Dat <-
          cbind(" " = "expand", matched_dfs, details = I(subdats))


        matched_summary <- summary(matches.out)

        sendSweetAlert(
          session = session,
          title = "Success!",
          text = "Please review each match",
          type = "success"
        )

        # for manual selection
        matched_results <- list(
          Dat = Dat,
          matches.out = matches.out,
          matched_summary = matched_summary,
          dfA.match = dfA.match,
          dfA.unmatch = dfA.unmatch,
          dfB.match = dfB.match,
          dfB.unmatch = dfB.unmatch,
          matched_union = dplyr::bind_rows(matched_dfs, dfA.unmatch, dfB.unmatch)
        )
        state$matched_results <- matched_results
        return(matched_results)
      }
    })

    # if (length(state$matched_results[['matches.out']]$matches$inds.a) != 0)
    # Output Matched ----------------------------------------------------------

    Dat <- reactive({
        dplyr::as_tibble(matched_values()[['Dat']])
    })

    output[["matched"]] <- renderDT({
        datatable(
        Dat(),
        callback = callback,
        escape = -2,
        extensions = c("Buttons", "Select"),
        selection = "none",
        options = list(
          select = list(style = "multi", selector = ".selectable"),
          autoWidth = FALSE,
          scrollX = TRUE,
          lengthMenu = list(c(10, 20, 50,-1), c('default', '20', '50', 'All')),
          pageLength = 10,
          dom = 'Blfrtip',
          buttons = list(),
          columnDefs = list(
            list(className = "selectable dt-center",
                 targets = c(0, 2:ncol(
                   matched_values()[['Dat']]
                 ))),
            list(visible = FALSE, targets = ncol(matched_values()[['Dat']])),
            list(
              orderable = FALSE,
              className = 'details-control',
              width = "10px",
              render = JS(render),
              targets = 1
            ),
            list(className = "dt-center", targets = "_all")
          )
        ),
        class = 'compact hover row-border nowrap stripe'
      )
    }, server = FALSE)

    output[["info-main"]] <- renderText({
      capture.output(input[["matched_rows_selected"]])
    })

    output[["info-children"]] <- renderText({
      paste0(capture.output(input[["matched_children"]]), collapse = "\n")
    })

    output[["matched-summary"]] <- renderPrint({
      matched_values()[['matched_summary']]
    })

    output[["plot-venn"]] <- renderPlot({
      if (length(state$matched_results[['matches.out']]$matches$inds.a) != 0) {
        n_dfA.unmatch <- nrow(matched_values()[['dfA.unmatch']])
        n_dfB.unmatch <- nrow(matched_values()[['dfB.unmatch']])
        n_match <- nrow(matched_values()[['Dat']])
        library("ggvenn") # Has to be removed for CRAN version
        # Warning: Error in inner_join: could not find function "inner_join"

        x <- list(Sample = c((1:n_dfA.unmatch) + 3e9, 1:n_match),
                  Matching = c(-(1:n_dfB.unmatch) + 5e7, 1:n_match))
        if (length(names(x)) == 2) {
          names(x) <- c("Sample Data", "Matching Data")
        }
        ggvenn::ggvenn(x, fill_color = c("#3b4992", "#008b45"))
      }
    })

    # Download selected rows --------------------------------------------------

    output$download_selected <- downloadHandler(
      filename = function() {
        paste("selected-data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(matched_values()[['Dat']][input[["matched_rows_selected"]] + 1, ], file)
      }
    )

    # Update matched results based on selection -------------------------------

    observe({
      if (!is.null(input[["matched_rows_selected"]])) {
        matched_rows_selected <- input[["matched_rows_selected"]] + 1

        state$matched_results[['matched_intersect']] <-
          matched_values()[['Dat']][matched_rows_selected, ] %>%
          dplyr::select(-tidyselect::any_of(c('details')))

        matches.out <- state$matched_results[['matches.out']]

        # Testing only
        # dfA <- readxl::read_excel('inst/app/www/lkselectedrecs_cleaned.xlsx')
        # dfB <- readxl::read_excel('inst/app/www/redcapoutput_cleaned.xlsx')
        # matched_rows_selected <- c(1, 2, 3, 7)
        #
        # matches.out <- fastLink::fastLink(
        #   dfA = dfA, dfB = dfB,
        #   varnames = c("firstname", "middlename", "lastname", "race", "sex"),
        #   # stringdist.match = c("firstname", "middlename", "lastname", "race", "sex"),
        #   # numeric.match =
        #   # partial.match = c("firstname", "lastname"),
        #   n.cores = 1
        # )

        dfA <- state$state_dfA
        dfB <- state$state_dfB

        matches.out.manual <- matches.out
        matches.out.manual$matches <- matches.out$matches[matched_rows_selected, ]
        matches.out.manual$patterns <- matches.out$patterns[matched_rows_selected, ]
        matches.out.manual$posterior <- matches.out$posterior[matched_rows_selected]

        dfA.match <- dfA[matches.out.manual$matches$inds.a, ]
        dfA.unmatch <- dfA[-matches.out.manual$matches$inds.a, ]
        dfB.match <- dfB[matches.out.manual$matches$inds.b, ]
        dfB.unmatch <- dfB[-matches.out.manual$matches$inds.b, ]

        matched_dfs <- fastLink::getMatches(
          dfA = dfA,
          dfB = dfB,
          fl.out = matches.out.manual,
          threshold.match = 0.85,
          combine.dfs = TRUE
        )
        matched_dfs <- matched_dfs %>%
          dplyr::select(-tidyselect::any_of(
            c(
              'gamma.1',
              'gamma.2',
              'gamma.3',
              'gamma.4',
              'gamma.5',
              'gamma.6',
              'posterior'
            )
          ))

        state$matched_results[['matched_union']] <- dplyr::bind_rows(matched_dfs, dfA.unmatch, dfB.unmatch)
        state$matched_results[['dfA.unmatch']] <- dfA.unmatch
        state$matched_results[['dfB.unmatch']] <- dfB.unmatch

      } else {
        state$matched_results[['matched_intersect']] <-
          dplyr::as_tibble(matched_values()[['Dat']])
      }
    })



    # Previous page button redirection
    observeEvent(input$previous_simple_settings, {
      updateTabItems(session = parent, "tabs", "simple_settings")
    })

    # Next page button redirection
    observeEvent(input$next_simple_details, {
      updateTabItems(session = parent, "tabs", "simple_details")
    })
  })
}

## To be copied in the UI
# mod_simple_results_ui("simple_results_1")

## To be copied in the server
# mod_simple_results_server("simple_results_1")


utils::globalVariables(c("Match Count", "Match Type"))

