.question_inspector_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(2, shiny::wellPanel(shiny::h4("Questions"), shiny::textOutput(ns("count_questions")))),
      shiny::column(2, shiny::wellPanel(shiny::h4("Open"), shiny::textOutput(ns("count_open")))),
      shiny::column(2, shiny::wellPanel(shiny::h4("Closed"), shiny::textOutput(ns("count_closed")))),
      shiny::column(2, shiny::wellPanel(shiny::h4("Options"), shiny::textOutput(ns("count_options")))),
      shiny::column(2, shiny::wellPanel(shiny::h4("Repeat groups"), shiny::textOutput(ns("count_repeat")))),
      shiny::column(2, shiny::wellPanel(shiny::h4("Source forms"), shiny::textOutput(ns("count_forms"))))
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4("Filter stored questions"),
        shiny::selectInput(ns("question_class"), "Question class",
                           c("All" = "all", "Open" = "open", "Closed" = "closed")),
        shiny::selectInput(ns("question_type"), "Ontology question type",
                           choices = c("All" = "all")),
        shiny::selectizeInput(ns("source_form"), "Source form",
                              choices = c("All" = "all"),
                              options = list(maxOptions = 1000)),
        shiny::selectInput(ns("repeat_membership"), "Repeat-group membership",
                           c("All" = "all", "In a repeat group" = "in_repeat",
                             "Not in a repeat group" = "not_repeat")),
        shiny::selectInput(ns("duplicate_filter"), "Caption duplicates",
                           c("All" = "all", "Potential duplicates" = "duplicates")),
        shiny::selectInput(ns("warning_filter"), "Extraction quality",
                           c("All" = "all", "Warnings only" = "warnings")),
        shiny::textInput(ns("search"), "Search caption, field, option, or form"),
        shiny::p("This page is read-only. Filters do not change data in Fluree.")
      ),
      shiny::mainPanel(
        shiny::h4("Questions"), DT::DTOutput(ns("question_table")),
        shiny::h4("Extraction-quality checks"), DT::DTOutput(ns("quality_summary"))
      )
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::h4("Selected question"), shiny::uiOutput(ns("question_detail"))),
      shiny::column(6, shiny::h4("Nested answer options"), DT::DTOutput(ns("option_table")))
    ),
    shiny::tags$details(
      shiny::tags$summary("Raw novaTagger question projection"),
      shiny::verbatimTextOutput(ns("question_record"))
    )
  )
}

.question_inspector_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      questions <- rv$questions
      if (is.null(questions)) return()
      types <- sort(unique(as.character(questions$question_type)))
      types <- types[!is.na(types) & nzchar(types)]
      forms <- sort(unique(.question_source_forms(questions)))
      forms <- forms[!is.na(forms) & nzchar(forms)]
      shiny::updateSelectInput(session, "question_type",
                               choices = c("All" = "all", stats::setNames(types, types)))
      shiny::updateSelectizeInput(session, "source_form",
                                  choices = c("All" = "all", stats::setNames(forms, forms)),
                                  server = TRUE)
    })

    counts <- shiny::reactive({
      if (is.null(rv$questions)) return(NULL)
      data <- .question_projection_counts(rv$questions)
      stats::setNames(data$value, data$metric)
    })
    count_value <- function(metric) if (is.null(counts())) "—" else counts()[[metric]]
    output$count_questions <- shiny::renderText(count_value("questions"))
    output$count_open <- shiny::renderText(count_value("open questions"))
    output$count_closed <- shiny::renderText(count_value("closed questions"))
    output$count_options <- shiny::renderText(count_value("answer options"))
    output$count_repeat <- shiny::renderText(count_value("questions in repeat groups"))
    output$count_forms <- shiny::renderText(count_value("source forms"))

    filtered <- shiny::reactive({
      if (is.null(rv$questions)) return(.empty_question_summary())
      .question_summary(
        rv$questions, question_class = input$question_class %||% "all",
        question_type = input$question_type %||% "all",
        source_form = input$source_form %||% "all",
        repeat_membership = input$repeat_membership %||% "all",
        duplicate_filter = input$duplicate_filter %||% "all",
        warning_filter = input$warning_filter %||% "all", search = input$search %||% ""
      )
    })
    selected_id <- shiny::reactive({
      selected <- input$question_table_rows_selected
      data <- filtered()
      if (length(selected) != 1L || selected > nrow(data)) return(NULL)
      data$id[[selected]]
    })

    output$question_table <- DT::renderDT({
      shown <- filtered()[, c("caption", "question_class", "question_type",
                             "response_cardinality", "answer_count", "source_form_id",
                             "in_repeat_group", "potential_duplicate", "quality_issues")]
      DT::datatable(shown, selection = "single", rownames = FALSE,
                    options = list(pageLength = 20, scrollX = TRUE))
    })
    output$quality_summary <- DT::renderDT({
      if (is.null(rv$questions)) return(DT::datatable(data.frame()))
      DT::datatable(.question_quality_summary(rv$questions), rownames = FALSE,
                    options = list(dom = "t", ordering = FALSE))
    })
    output$question_detail <- shiny::renderUI({
      id <- selected_id()
      if (is.null(id)) return(shiny::p("Select one question from the table."))
      record <- .question_record(rv$questions, id)
      source_form <- record$source_form_id %||% NA_character_
      repeat_group <- record$repeat_group_id
      if (is.null(repeat_group) || is.na(repeat_group) || !nzchar(repeat_group)) {
        repeat_group <- "Not in a repeat group"
      }
      shiny::tags$dl(
        shiny::tags$dt("Caption"), shiny::tags$dd(record$caption),
        shiny::tags$dt("Class and type"),
        shiny::tags$dd(paste(record$question_class, record$question_type, sep = " — ")),
        shiny::tags$dt("Response"),
        shiny::tags$dd(paste(record$response_cardinality, record$response_datatype, sep = " — ")),
        shiny::tags$dt("Source form"), shiny::tags$dd(source_form),
        shiny::tags$dt("Source field"), shiny::tags$dd(record$source_element_name),
        shiny::tags$dt("Repeat group"), shiny::tags$dd(repeat_group)
      )
    })
    output$option_table <- DT::renderDT({
      id <- selected_id()
      data <- if (is.null(id)) data.frame() else .question_options(rv$questions, id)
      DT::datatable(data, rownames = FALSE, options = list(pageLength = 15, dom = "tip"))
    })
    output$question_record <- shiny::renderText({
      id <- selected_id()
      if (is.null(id)) return("Select one question.")
      jsonlite::toJSON(.question_record(rv$questions, id), auto_unbox = TRUE,
                       pretty = TRUE, null = "null", dataframe = "rows", na = "null")
    })
    invisible(list(filtered = filtered, selected_id = selected_id))
  })
}
