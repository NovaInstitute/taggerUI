.connect_resume_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h4("1. Connect"),
      shiny::textInput(ns("fluree_url"), "Fluree URL", "http://localhost:8090"),
      shiny::textInput(ns("ledger"), "Ledger", ""),
      shiny::textInput(ns("branch"), "Branch", "main"),
      shiny::actionButton(ns("connect"), "Connect to Fluree", class = "btn-primary"),
      shiny::tags$details(
        shiny::tags$summary("Advanced connection settings"),
        shiny::passwordInput(ns("fluree_key"), "Fluree API key",
                             placeholder = "Uses FLUREE_API_KEY when blank"),
        shiny::numericInput(ns("fluree_timeout"), "Request timeout (seconds)",
                            300, min = 10, max = 3600)
      ),
      shiny::hr(),
      shiny::h4("2. Review workspace"),
      shiny::p(
        "Review work is isolated in a branch. The published baseline in main is read-only."
      ),
      shiny::actionButton(ns("refresh_branches"), "Refresh workspaces"),
      shiny::textInput(ns("new_review_branch"), "New review workspace name",
                       placeholder = "review-your-name"),
      shiny::actionButton(ns("create_review_branch"), "Create review workspace",
                          class = "btn-success"),
      shiny::hr(),
      shiny::h4("3. Select stored data"),
      shiny::textInput(ns("survey_graph"), "Survey named graph", ""),
      shiny::textInput(ns("run_id"), "Tagging run ID", ""),
      shiny::textInput(ns("tagging_graph_base"), "Tagging graph base", ""),
      shiny::tags$details(
        shiny::tags$summary("Advanced loading settings"),
        shiny::numericInput(ns("query_page_size"), "Query page size", 200,
                            min = 1, max = 2000),
        shiny::numericInput(ns("tag_batch_size"), "Persistence batch size", 50,
                            min = 1, max = 500)
      ),
      shiny::actionButton(ns("load_questions"), "Load questions only"),
      shiny::hr(),
      shiny::h4("3. Resume"),
      shiny::actionButton(ns("resume_run"), "Resume tagging run",
                          class = "btn-success"),
      shiny::actionButton(ns("reload_run"), "Reload from Fluree")
    ),
    shiny::mainPanel(
      shiny::fluidRow(
        shiny::column(4, shiny::wellPanel(
          shiny::h4("Connection"), shiny::textOutput(ns("connection_status"))
        )),
        shiny::column(4, shiny::wellPanel(
          shiny::h4("Run"), shiny::textOutput(ns("run_status"))
        )),
        shiny::column(4, shiny::wellPanel(
          shiny::h4("Current stage"), shiny::textOutput(ns("stage_status"))
        ))
      ),
      shiny::uiOutput(ns("branch_notice")),
      shiny::h4("Available review workspaces"),
      DT::DTOutput(ns("available_branches")),
      shiny::p(
        "Connection checks are lightweight. Resume loads the authoritative ",
        "question and tagging state once and retains it in this Shiny session."
      ),
      shiny::h4("Resolved data locations"),
      DT::DTOutput(ns("graph_locations")),
      shiny::h4("Tagging walkthrough"),
      DT::DTOutput(ns("walkthrough_progress")),
      shiny::h4("Stored survey summary"),
      DT::DTOutput(ns("question_counts")),
      shiny::verbatimTextOutput(ns("load_activity"))
    )
  )
}

.connect_resume_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    notify_error <- function(expr) tryCatch(expr, error = function(error) {
      shiny::showNotification(conditionMessage(error), type = "error",
                              duration = NULL)
      NULL
    })
    config <- shiny::reactive({
      key <- trimws(input$fluree_key %||% "")
      if (!nzchar(key)) key <- Sys.getenv("FLUREE_API_KEY", unset = "")
      if (!nzchar(key)) key <- NULL
      novaRush::setConfig(
        baseUrl = trimws(input$fluree_url), ledger = trimws(input$ledger),
        branch = trimws(input$branch), apiKey = key,
        timeout = as.numeric(input$fluree_timeout)
      )
    })
    graphs <- shiny::reactive(.tagger_graphs(input$tagging_graph_base))
    available_branches <- shiny::reactiveVal(data.frame())

    refresh_branches <- function() {
      shiny::req(isTRUE(rv$connected))
      result <- notify_error(novaRush::listBranches(config()))
      if (!is.null(result)) {
        rows <- lapply(result, function(branch) {
          data.frame(
            branch = as.character(branch$branch %||% ""),
            ledger_id = as.character(branch$ledger_id %||% ""),
            commit = as.character(branch$t %||% ""),
            workspace = if (identical(branch$branch, "main")) {
              "Published baseline"
            } else {
              "Review workspace"
            },
            stringsAsFactors = FALSE
          )
        })
        data <- if (length(rows)) dplyr::bind_rows(rows) else data.frame(
          branch = character(), ledger_id = character(), commit = character(),
          workspace = character(), stringsAsFactors = FALSE
        )
        available_branches(data[order(data$branch != "main", data$branch), , drop = FALSE])
      }
      invisible(result)
    }

    shiny::observeEvent(input$ledger, {
      ledger <- trimws(input$ledger)
      if (!nzchar(ledger)) return()
      shiny::updateTextInput(session, "survey_graph",
                             value = .tagger_default_survey_graph(ledger))
    }, ignoreInit = TRUE)
    shiny::observeEvent(list(input$ledger, input$run_id), {
      if (!nzchar(trimws(input$ledger)) || !nzchar(trimws(input$run_id))) return()
      shiny::updateTextInput(
        session, "tagging_graph_base",
        value = .tagger_default_graph_base(input$ledger, input$run_id)
      )
    }, ignoreInit = TRUE)

    invalidate_run <- function(connection = FALSE) {
      rv$questions <- NULL; rv$workflow <- NULL; rv$store <- NULL
      if (connection) {
        rv$connected <- FALSE
        rv$connection_message <- "Connection settings changed; reconnect"
      }
    }
    shiny::observeEvent(
      list(input$fluree_url, input$ledger, input$branch, input$fluree_key),
      invalidate_run(TRUE), ignoreInit = TRUE
    )
    shiny::observeEvent(
      list(input$survey_graph, input$run_id, input$tagging_graph_base),
      invalidate_run(FALSE), ignoreInit = TRUE
    )

    shiny::observeEvent(input$connect, {
      result <- notify_error(shiny::withProgress(
        message = "Checking Fluree and ledger", value = 0.3,
        novaRush::fluree_connect(
          ledger = trimws(input$ledger), branch = trimws(input$branch),
          base_url = trimws(input$fluree_url),
          api_key = config()$apiKey, timeout = input$fluree_timeout,
          check = TRUE
        )
      ))
      if (!is.null(result)) {
        rv$connected <- TRUE
        rv$connection_message <- paste0(
          "Connected to ", trimws(input$ledger), ":", trimws(input$branch)
        )
        refresh_branches()
      }
    })

    shiny::observeEvent(input$refresh_branches, refresh_branches())
    shiny::observeEvent(input$create_review_branch, {
      shiny::req(isTRUE(rv$connected))
      branch <- trimws(input$new_review_branch %||% "")
      if (!nzchar(branch)) {
        shiny::showNotification("Enter a review workspace name.", type = "error")
        return()
      }
      result <- notify_error(shiny::withProgress(
        message = "Creating review workspace from main", value = 0.4,
        novaRush::createBranch(config(), branch = branch, from = "main")
      ))
      if (!is.null(result) && isFALSE(result$created) && isTRUE(result$exists)) {
        shiny::showNotification(
          "That review workspace already exists. Choose it from the branch field or use a new name.",
          type = "error", duration = NULL
        )
      } else if (!is.null(result)) {
        refresh_branches()
        shiny::updateTextInput(session, "branch", value = branch)
        shiny::updateTextInput(session, "new_review_branch", value = "")
        rv$connection_message <- paste0(
          "Review workspace created from main: ", trimws(input$ledger), ":", branch,
          ". Reconnect before loading its data."
        )
        rv$connected <- FALSE
        shiny::showNotification(
          "Review workspace created. Reconnect to start reviewing it.",
          type = "message"
        )
      }
    })

    load_questions <- function() {
      shiny::req(isTRUE(rv$connected), nzchar(trimws(input$survey_graph)))
      started <- proc.time()[["elapsed"]]
      result <- notify_error(shiny::withProgress(
        message = "Loading survey questions", value = 0.2, {
          questions <- novaTagger::query_taggable_questions(
            config(), graph = trimws(input$survey_graph),
            branch = trimws(input$branch),
            page_size = as.integer(input$query_page_size)
          )
          shiny::setProgress(1, detail = paste(nrow(questions), "questions loaded"))
          questions
        }
      ))
      if (!is.null(result)) {
        rv$questions <- result; rv$workflow <- NULL; rv$store <- NULL
        rv$load_message <- paste0(
          "Loaded ", nrow(result), " questions in ",
          round(proc.time()[["elapsed"]] - started, 1), " seconds."
        )
      }
      result
    }
    shiny::observeEvent(input$load_questions, load_questions())

    resume <- function(force_questions = FALSE) {
      shiny::req(isTRUE(rv$connected), nzchar(trimws(input$run_id)),
                 nzchar(trimws(input$tagging_graph_base)))
      started <- proc.time()[["elapsed"]]
      questions <- rv$questions
      if (isTRUE(force_questions) || is.null(questions)) questions <- load_questions()
      if (is.null(questions)) return(NULL)
      result <- notify_error(shiny::withProgress(
        message = "Resuming authoritative tagging state", value = 0.35, {
          repository <- novaTagger::novarush_semantic_repository(
            config(), graphs = graphs(), branch = trimws(input$branch),
            batch_size = as.integer(input$tag_batch_size),
            query_page_size = as.integer(input$query_page_size)
          )
          store <- novaTagger::semantic_tag_store(
            repository, questions, trimws(input$run_id),
            base_iri = "https://data.nova.org/tagger/"
          )
          if (!novaTagger::tag_store_exists(store)) {
            stop("The requested tagging run was not found.", call. = FALSE)
          }
          value <- novaTagger::resume_tagging_workflow(store)
          shiny::setProgress(1, detail = paste("Revision", value$state$revision))
          list(workflow = value, store = store)
        }
      ))
      if (!is.null(result)) {
        rv$workflow <- result$workflow; rv$store <- result$store
        rv$load_message <- paste0(
          "Resumed revision ", rv$workflow$state$revision, " in ",
          round(proc.time()[["elapsed"]] - started, 1), " seconds."
        )
      }
      invisible(result)
    }
    shiny::observeEvent(input$resume_run, resume(FALSE))
    shiny::observeEvent(input$reload_run, resume(TRUE))

    output$connection_status <- shiny::renderText(rv$connection_message)
    output$run_status <- shiny::renderText({
      if (is.null(rv$workflow)) return("No run resumed")
      paste0(rv$workflow$state$run_id, "\nrevision ", rv$workflow$state$revision)
    })
    output$stage_status <- shiny::renderText({
      if (is.null(rv$workflow)) {
        if (is.null(rv$questions)) "Waiting for questions" else "Questions ready"
      } else rv$workflow$state$workflow$stage %||% "unknown"
    })
    output$branch_notice <- shiny::renderUI({
      branch <- trimws(input$branch %||% "")
      if (!nzchar(branch)) return(NULL)
      if (identical(branch, "main")) {
        shiny::div(class = "alert alert-warning",
                   "You are connected to main. Saved review actions modify main.")
      } else shiny::div(class = "alert alert-info",
                        "Review changes are isolated on branch: ", branch)
    })
    output$available_branches <- DT::renderDT({
      DT::datatable(
        available_branches(), rownames = FALSE,
        options = list(dom = "t", ordering = FALSE, scrollX = TRUE)
      )
    })
    output$graph_locations <- DT::renderDT({
      if (!nzchar(trimws(input$tagging_graph_base %||% ""))) {
        return(DT::datatable(data.frame()))
      }
      locations <- c(survey = input$survey_graph, unlist(graphs()))
      DT::datatable(data.frame(role = names(locations), iri = unname(locations)),
                    rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })
    output$walkthrough_progress <- DT::renderDT(DT::datatable(
      .tagger_progress_table(rv$workflow, rv$questions), rownames = FALSE,
      options = list(dom = "t", ordering = FALSE)
    ))
    output$question_counts <- DT::renderDT({
      if (is.null(rv$questions)) return(DT::datatable(data.frame()))
      DT::datatable(.question_projection_counts(rv$questions), rownames = FALSE,
                    options = list(dom = "t", ordering = FALSE))
    })
    output$load_activity <- shiny::renderText(rv$load_message)
  })
}
