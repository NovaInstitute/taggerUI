# Guided Shiny interface over the public novaRush and novaTagger APIs.

.tagger_default_scope <- function(ledger) {
  paste0("https://data.nova.org/integration/", trimws(ledger), "/")
}

.tagger_default_survey_graph <- function(ledger) {
  paste0(.tagger_default_scope(ledger), "graph/survey")
}

.tagger_default_graph_base <- function(ledger, run_id) {
  paste0(
    .tagger_default_scope(ledger), "graph/tagging/openai/",
    trimws(run_id), "/"
  )
}

.tagger_graphs <- function(base) {
  base <- paste0(sub("/+$", "", trimws(base)), "/")
  stats::setNames(paste0(base, c("run", "embedding", "hierarchy", "review")),
                  c("run", "embedding", "hierarchy", "review")) |>
    as.list()
}

.tagger_cluster_status <- function(state, level, cluster_id) {
  proposal <- .quality_latest_proposal(state, level, cluster_id)
  if (is.null(proposal)) "pending" else proposal$status %||% "pending"
}

.tagger_hierarchy_graph <- function(workflow) {
  if (is.null(workflow) || !is.data.frame(workflow$state$clusters) ||
      !nrow(workflow$state$clusters)) {
    return(list(nodes = data.frame(), edges = data.frame()))
  }
  clusters <- workflow$state$clusters
  status <- vapply(seq_len(nrow(clusters)), function(i) {
    .tagger_cluster_status(
      workflow$state, clusters$level[[i]], clusters$cluster_id[[i]]
    )
  }, character(1))
  label <- ifelse(
    is.na(clusters$tag) | !nzchar(clusters$tag),
    paste0("L", clusters$level, " / C", clusters$cluster_id),
    clusters$tag
  )
  nodes <- data.frame(
    id = paste0("L", clusters$level, "C", clusters$cluster_id),
    label = label,
    level = -as.integer(clusters$level),
    group = status,
    title = paste0(
      "Level ", clusters$level, "; cluster ", clusters$cluster_id,
      "; questions ", lengths(clusters$question_ids), "; status ", status
    ),
    stringsAsFactors = FALSE
  )
  quality <- .cluster_node_quality(workflow$state)
  quality_hit <- match(paste(clusters$level, clusters$cluster_id, sep = ":"),
                       quality$key)
  nodes$color.border <- ifelse(quality$flagged[quality_hit], "#d1495b", "#6c757d")
  nodes$borderWidth <- ifelse(quality$flagged[quality_hit], 4, 1)
  nodes$title <- paste0(
    nodes$title, "; quality warnings ", quality$warning_count[quality_hit]
  )
  edges <- lapply(seq_len(nrow(clusters)), function(i) {
    if (is.na(clusters$parent_cluster[[i]])) return(NULL)
    data.frame(
      from = paste0("L", clusters$level[[i]], "C", clusters$cluster_id[[i]]),
      to = paste0(
        "L", clusters$level[[i]] + 1L, "C", clusters$parent_cluster[[i]]
      ),
      stringsAsFactors = FALSE
    )
  })
  list(nodes = nodes, edges = dplyr::bind_rows(edges))
}

.tagger_progress_table <- function(workflow, questions) {
  question_count <- if (is.null(questions)) 0L else nrow(questions)
  state <- if (is.null(workflow)) NULL else workflow$state
  embedded <- if (is.null(state) || is.null(state$embeddings)) {
    0L
  } else if (is.matrix(state$embeddings)) {
    nrow(state$embeddings)
  } else {
    sum(!vapply(state$embeddings, is.null, logical(1)))
  }
  clusters <- if (is.null(state)) NULL else state$clusters %||% NULL
  cluster_count <- if (is.data.frame(clusters)) nrow(clusters) else 0L
  tagged <- if (cluster_count) {
    sum(!is.na(clusters$tag) & nzchar(clusters$tag) & clusters$tag != "untagged")
  } else 0L
  proposals <- if (is.null(state)) 0L else length(state$proposals %||% list())
  reviews <- if (is.null(state)) 0L else length(state$review_events %||% list())
  tibble::tibble(
    step = c("Questions", "Embeddings", "Hierarchy", "Proposals", "Reviews"),
    status = c(
      if (question_count) "ready" else "waiting",
      if (embedded == question_count && question_count) "complete" else
        if (embedded) "in progress" else "waiting",
      if (cluster_count) "complete" else "waiting",
      if (proposals) "in progress" else "waiting",
      if (reviews) "in progress" else "waiting"
    ),
    progress = c(
      paste0(question_count, " loaded"),
      paste0(embedded, " / ", question_count),
      paste0(cluster_count, " clusters"),
      paste0(proposals, " proposals"),
      paste0(reviews, " decisions; ", tagged, " / ", cluster_count, " tagged")
    )
  )
}

.walkthrough_app_ui <- function() {
  shiny::navbarPage(
    "Survey tagger walkthrough",
    shiny::tabPanel(
      "1. Connect & resume",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::textInput("fluree_url", "Fluree URL", "http://localhost:8090"),
          shiny::textInput("ledger", "Ledger", ""),
          shiny::textInput("branch", "Branch", "main"),
          shiny::passwordInput(
            "fluree_key", "Fluree API key",
            placeholder = "Uses FLUREE_API_KEY when blank"
          ),
          shiny::numericInput("fluree_timeout", "Request timeout (seconds)", 300,
                              min = 10, max = 3600),
          shiny::hr(),
          shiny::textInput("survey_graph", "Survey named graph", ""),
          shiny::numericInput("query_page_size", "Query page size", 200,
                              min = 1, max = 2000),
          shiny::actionButton(
            "load_questions", "Load stored questions", class = "btn-primary"
          ),
          shiny::hr(),
          shiny::textInput("run_id", "Tagging run ID", ""),
          shiny::textInput("tagging_graph_base", "Tagging graph base", ""),
          shiny::numericInput("tag_batch_size", "Persistence batch size", 50,
                              min = 1, max = 500),
          shiny::actionButton("resume_run", "Resume tagging run")
        ),
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::column(4, shiny::wellPanel(
              shiny::h4("Connection"), shiny::textOutput("connection_status")
            )),
            shiny::column(4, shiny::wellPanel(
              shiny::h4("Run"), shiny::textOutput("run_status")
            )),
            shiny::column(4, shiny::wellPanel(
              shiny::h4("Current stage"), shiny::textOutput("stage_status")
            ))
          ),
          shiny::h3("Tagging walkthrough"),
          shiny::p(
            "The application reads authoritative survey and tagging state from ",
            "Fluree. Loading questions does not modify the ledger. Resuming a ",
            "run reconstructs its latest persisted revision."
          ),
          DT::DTOutput("walkthrough_progress"),
          shiny::h4("Stored survey summary"),
          DT::DTOutput("question_counts")
        )
      )
    ),
    shiny::tabPanel(
      "2. Inspect questions",
      shiny::fluidRow(
        shiny::column(
          3,
          shiny::selectInput(
            "question_class_filter", "Question class",
            choices = c("All" = "all", "Open" = "open", "Closed" = "closed")
          ),
          shiny::checkboxInput("repeat_only", "Only questions in repeat groups"),
          shiny::textInput("question_search", "Search caption, field, or option"),
          shiny::p(
            "Select a row to inspect its nested answer options and complete ",
            "novaTagger projection."
          )
        ),
        shiny::column(9, DT::DTOutput("question_table"))
      ),
      shiny::fluidRow(
        shiny::column(
          6, shiny::h4("Nested answer options"), DT::DTOutput("option_table")
        ),
        shiny::column(
          6, shiny::h4("Selected question record"),
          shiny::verbatimTextOutput("question_record")
        )
      )
    ),
    shiny::tabPanel(
      "3. Hierarchy & quality",
      shiny::h3("Hierarchy progress and cluster quality"),
      shiny::p(
        "Node fill shows review status. A red border flags centroid outliers, ",
        "questions outside the proposed tag scope, or a materially better ",
        "alternative cluster. Select a node or use the cluster list below."
      ),
      visNetwork::visNetworkOutput("hierarchy", height = "520px"),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::h4("Selected cluster"),
          shiny::selectInput("quality_cluster", "Cluster", choices = character()),
          shiny::verbatimTextOutput("hierarchy_summary"),
          shiny::h4("Quality summary"),
          DT::DTOutput("cluster_quality_summary"),
          shiny::h4("Parent, children, and siblings"),
          DT::DTOutput("cluster_context")
        ),
        shiny::column(
          8,
          shiny::p(
            "The PCA plot is a two-dimensional navigation view only. All ",
            "quality flags and placement recommendations use the original ",
            "embedding dimensions."
          ),
          shiny::plotOutput("cluster_pca", height = "480px"),
          shiny::h4("Questions ranked by concern"),
          shiny::p(
            "Low centroid or tag similarity appears first. A positive placement ",
            "margin means another cluster centroid is closer."
          ),
          DT::DTOutput("cluster_quality_questions")
        )
      ),
      shiny::h4("All cluster quality flags"),
      DT::DTOutput("all_cluster_quality")
    ),
    shiny::tabPanel(
      "4. Review tags",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::h4("OpenAI provider"),
          shiny::textInput("openai_base_url", "API URL", "https://api.openai.com/v1"),
          shiny::passwordInput(
            "openai_key", "OpenAI API key",
            placeholder = "Uses OPENAI_API_KEY when blank"
          ),
          shiny::textInput("tag_model", "Generation model", "gpt-5.4-mini"),
          shiny::textInput("tag_embed_model", "Embedding model", "text-embedding-3-small"),
          shiny::numericInput("tag_sample_size", "Cluster examples", 8,
                              min = 1, max = 50),
          shiny::actionButton(
            "generate_proposal", "Generate next proposal", class = "btn-primary"
          ),
          shiny::textOutput("model_activity"),
          shiny::hr(),
          shiny::h4("Review proposal"),
          shiny::selectInput("proposal_id", "Proposal", choices = character()),
          shiny::textInput("edited_tag", "Tag label"),
          shiny::textInput("reviewer_id", "Reviewer ID", "reviewer"),
          shiny::textAreaInput("review_rationale", "Reviewer rationale", rows = 3),
          shiny::actionButton("accept_proposal", "Accept", class = "btn-success"),
          shiny::actionButton("edit_proposal", "Save edit", class = "btn-primary"),
          shiny::actionButton("reject_proposal", "Reject", class = "btn-danger"),
          shiny::actionButton("defer_proposal", "Defer")
        ),
        shiny::mainPanel(
          shiny::h3("Next cluster awaiting a proposal"),
          DT::DTOutput("next_cluster"),
          DT::DTOutput("next_cluster_questions"),
          shiny::hr(),
          shiny::h3("Selected proposal"),
          shiny::verbatimTextOutput("proposal_detail"),
          shiny::fluidRow(
            shiny::column(5, shiny::h4("Similarity summary"),
                          DT::DTOutput("proposal_similarity_summary")),
            shiny::column(7, shiny::h4("Review history"),
                          DT::DTOutput("proposal_review_history"))
          ),
          shiny::h4("Questions scored against the proposed tag"),
          DT::DTOutput("proposal_questions"),
          shiny::h4("All proposals in this run"),
          DT::DTOutput("proposal_table")
        )
      )
    )
  )
}

.walkthrough_app_server <- function(input, output, session) {
  rv <- shiny::reactiveValues(
    questions = NULL, workflow = NULL, store = NULL,
    connection_message = "Not connected",
    model_message = "No model request in this session"
  )

  notify_error <- function(expr) {
    tryCatch(expr, error = function(error) {
      shiny::showNotification(
        conditionMessage(error), type = "error", duration = NULL
      )
      NULL
    })
  }

  fluree_config <- shiny::reactive({
    key <- trimws(input$fluree_key %||% "")
    if (!nzchar(key)) key <- Sys.getenv("FLUREE_API_KEY", unset = "")
    if (!nzchar(key)) key <- NULL
    novaRush::setConfig(
      baseUrl = trimws(input$fluree_url), ledger = trimws(input$ledger),
      branch = trimws(input$branch), apiKey = key,
      timeout = as.numeric(input$fluree_timeout)
    )
  })

  openai_provider <- function() {
    key <- trimws(input$openai_key %||% "")
    if (!nzchar(key)) key <- Sys.getenv("OPENAI_API_KEY", unset = "")
    config <- novaTagger::openai_config(
      api_key = key,
      base_url = trimws(input$openai_base_url),
      embed_model = trimws(input$tag_embed_model),
      generation_model = trimws(input$tag_model)
    )
    provider <- novaTagger::openai_model_provider(config)
    if (!novaTagger::model_provider_validate(provider)) {
      stop("OpenAI provider validation failed.", call. = FALSE)
    }
    provider
  }

  model_trace <- function(event) {
    if (!identical(event$system, "openai")) return(invisible(NULL))
    endpoint <- event$endpoint %||% ""
    if (identical(event$direction, "request")) {
      rv$model_message <- paste(
        if (identical(endpoint, "/responses")) "Generating tag with" else
          "Embedding tag with",
        event$body$model
      )
    } else if (identical(event$direction, "response")) {
      usage <- event$body$usage %||% list()
      tokens <- usage$total_tokens %||% usage$prompt_tokens %||% NA_integer_
      rv$model_message <- paste0(
        if (identical(endpoint, "/responses")) "Generation" else "Embedding",
        " completed", if (!is.na(tokens)) paste0("; tokens: ", tokens) else ""
      )
    }
    invisible(NULL)
  }

  reload_workflow <- function() {
    if (is.null(rv$store)) stop("Resume a tagging run first.", call. = FALSE)
    rv$workflow <- novaTagger::resume_tagging_workflow(rv$store)
    rv$workflow
  }

  shiny::observeEvent(input$ledger, {
    ledger <- trimws(input$ledger)
    if (!nzchar(ledger)) return()
    current <- trimws(input$survey_graph %||% "")
    if (!nzchar(current) || grepl("/integration/.+/graph/survey$", current)) {
      shiny::updateTextInput(
        session, "survey_graph", value = .tagger_default_survey_graph(ledger)
      )
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(list(input$ledger, input$run_id), {
    ledger <- trimws(input$ledger)
    run_id <- trimws(input$run_id)
    if (!nzchar(ledger) || !nzchar(run_id)) return()
    current <- trimws(input$tagging_graph_base %||% "")
    if (!nzchar(current) || grepl("/graph/tagging/openai/.+/$", current)) {
      shiny::updateTextInput(
        session, "tagging_graph_base",
        value = .tagger_default_graph_base(ledger, run_id)
      )
    }
  }, ignoreInit = TRUE)

  load_questions <- function() {
    shiny::req(nzchar(trimws(input$ledger)), nzchar(trimws(input$survey_graph)))
    result <- notify_error(shiny::withProgress(
      message = "Reading survey knowledge from Fluree", value = 0.2,
      {
        questions <- novaTagger::query_taggable_questions(
          fluree_config(), graph = trimws(input$survey_graph),
          branch = trimws(input$branch),
          page_size = as.integer(input$query_page_size)
        )
        shiny::setProgress(1, detail = paste(nrow(questions), "questions loaded"))
        questions
      }
    ))
    if (is.null(result)) return(NULL)
    rv$questions <- result
    rv$workflow <- NULL
    rv$store <- NULL
    rv$connection_message <- paste0(
      "Connected to ", trimws(input$ledger), ":", trimws(input$branch)
    )
    result
  }

  shiny::observeEvent(input$load_questions, load_questions())

  shiny::observeEvent(input$resume_run, {
    shiny::req(nzchar(trimws(input$run_id)),
               nzchar(trimws(input$tagging_graph_base)))
    questions <- rv$questions
    if (is.null(questions)) questions <- load_questions()
    if (is.null(questions)) return()
    workflow <- notify_error(shiny::withProgress(
      message = "Reconstructing tagging state from Fluree", value = 0.2,
      {
        repository <- novaTagger::novarush_semantic_repository(
          fluree_config(), graphs = .tagger_graphs(input$tagging_graph_base),
          branch = trimws(input$branch),
          batch_size = as.integer(input$tag_batch_size)
        )
        store <- novaTagger::semantic_tag_store(
          repository, questions, trimws(input$run_id),
          base_iri = "https://data.nova.org/tagger/"
        )
        if (!novaTagger::tag_store_exists(store)) {
          stop("The requested tagging run was not found in these named graphs.",
               call. = FALSE)
        }
        rv$store <- store
        result <- novaTagger::resume_tagging_workflow(store)
        shiny::setProgress(1, detail = paste("Revision", result$state$revision))
        result
      }
    ))
    if (!is.null(workflow)) rv$workflow <- workflow
  })

  shiny::observe({
    if (is.null(rv$workflow)) {
      shiny::updateSelectInput(session, "proposal_id", choices = character())
      return()
    }
    choices <- .proposal_choices(rv$workflow$state)
    current <- shiny::isolate(input$proposal_id %||% "")
    selected <- if (current %in% unname(choices)) {
      current
    } else if (length(choices)) {
      utils::tail(unname(choices), 1L)
    } else character()
    shiny::updateSelectInput(
      session, "proposal_id", choices = choices, selected = selected
    )
  })

  shiny::observe({
    if (is.null(rv$workflow) || is.null(rv$workflow$state$clusters)) {
      shiny::updateSelectInput(session, "quality_cluster", choices = character())
      return()
    }
    choices <- .cluster_choices(rv$workflow$state)
    current <- shiny::isolate(input$quality_cluster %||% "")
    selected <- if (current %in% unname(choices)) current else
      if (length(choices)) unname(choices)[[1]] else character()
    shiny::updateSelectInput(
      session, "quality_cluster", choices = choices, selected = selected
    )
  })

  shiny::observeEvent(input$hierarchy_node_selected, {
    node <- as.character(input$hierarchy_node_selected %||% "")
    matched <- regexec("^L([0-9]+)C(.+)$", node)
    parts <- regmatches(node, matched)[[1]]
    if (length(parts) == 3L) {
      key <- paste(parts[[2]], parts[[3]], sep = ":")
      choices <- .cluster_choices(rv$workflow$state)
      if (key %in% unname(choices)) {
        shiny::updateSelectInput(session, "quality_cluster", selected = key)
      }
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$proposal_id, {
    if (is.null(rv$workflow)) return()
    proposal <- .selected_proposal(rv$workflow$state, input$proposal_id)
    if (!is.null(proposal)) {
      shiny::updateTextInput(session, "edited_tag", value = proposal$tag)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$generate_proposal, {
    shiny::req(rv$store)
    result <- notify_error(shiny::withProgress(
      message = "Generating and persisting tag proposal", value = 0.1,
      {
        workflow <- reload_workflow()
        next_cluster <- novaTagger::workflow_next_cluster(workflow)
        if (!nrow(next_cluster)) {
          stop("No cluster is currently available for a new proposal.",
               call. = FALSE)
        }
        provider <- openai_provider()
        shiny::setProgress(.35, detail = paste0(
          "Level ", next_cluster$level[[1]],
          ", cluster ", next_cluster$cluster_id[[1]]
        ))
        workflow <- novaTagger::workflow_propose_next(
          workflow, provider,
          sample_size = as.integer(input$tag_sample_size),
          event_callback = model_trace
        )
        shiny::setProgress(.8, detail = "Reloading proposal from Fluree")
        persisted <- novaTagger::resume_tagging_workflow(rv$store)
        shiny::setProgress(1, detail = paste("Revision", persisted$state$revision))
        persisted
      }
    ))
    if (!is.null(result)) {
      rv$workflow <- result
      shiny::showNotification(
        "Proposal generated, embedded, persisted, and reloaded.", type = "message"
      )
    }
  })

  review_selected <- function(decision) {
    shiny::req(rv$store, nzchar(input$proposal_id %||% ""))
    result <- notify_error(shiny::withProgress(
      message = paste("Applying reviewer decision:", decision), value = 0.15,
      {
        workflow <- reload_workflow()
        proposal <- .selected_proposal(workflow$state, input$proposal_id)
        if (is.null(proposal)) stop("The selected proposal no longer exists.", call. = FALSE)
        if (!proposal$status %in% c("proposed", "deferred")) {
          stop(
            "Only proposed or deferred tags can be reviewed; current status is ",
            proposal$status, ".", call. = FALSE
          )
        }
        arguments <- list(
          workflow = workflow, proposal_id = input$proposal_id,
          decision = decision, reviewer_id = trimws(input$reviewer_id),
          rationale = input$review_rationale
        )
        if (identical(decision, "edited")) {
          arguments$tag <- trimws(input$edited_tag)
          arguments$provider <- openai_provider()
          arguments$event_callback <- model_trace
        }
        workflow <- do.call(novaTagger::workflow_review_proposal, arguments)
        shiny::setProgress(.8, detail = "Reloading reviewer decision from Fluree")
        persisted <- novaTagger::resume_tagging_workflow(rv$store)
        shiny::setProgress(1, detail = paste("Revision", persisted$state$revision))
        persisted
      }
    ))
    if (!is.null(result)) {
      rv$workflow <- result
      shiny::showNotification(
        paste("Reviewer decision persisted:", decision), type = "message"
      )
    }
  }

  shiny::observeEvent(input$accept_proposal, review_selected("accepted"))
  shiny::observeEvent(input$edit_proposal, review_selected("edited"))
  shiny::observeEvent(input$reject_proposal, review_selected("rejected"))
  shiny::observeEvent(input$defer_proposal, review_selected("deferred"))

  filtered_questions <- shiny::reactive({
    if (is.null(rv$questions)) return(.empty_question_summary())
    .question_summary(
      rv$questions,
      question_class = input$question_class_filter,
      repeat_only = input$repeat_only,
      search = input$question_search
    )
  })

  selected_question_id <- shiny::reactive({
    selected <- input$question_table_rows_selected
    data <- filtered_questions()
    if (length(selected) != 1L || selected > nrow(data)) return(NULL)
    data$id[[selected]]
  })

  next_cluster_data <- shiny::reactive({
    if (is.null(rv$workflow)) {
      return(list(cluster = tibble::tibble(), questions = tibble::tibble()))
    }
    .next_cluster_summary(rv$workflow, as.integer(input$tag_sample_size))
  })

  selected_proposal <- shiny::reactive({
    if (is.null(rv$workflow)) return(NULL)
    .selected_proposal(rv$workflow$state, input$proposal_id)
  })

  selected_proposal_scores <- shiny::reactive({
    proposal <- selected_proposal()
    if (is.null(proposal)) return(tibble::tibble())
    .proposal_question_scores(rv$workflow$state, proposal)
  })

  selected_quality_cluster <- shiny::reactive({
    shiny::req(rv$workflow, nzchar(input$quality_cluster %||% ""))
    cluster <- .parse_cluster_key(input$quality_cluster)
    shiny::req(!is.null(cluster))
    cluster
  })

  selected_cluster_questions <- shiny::reactive({
    cluster <- selected_quality_cluster()
    .cluster_quality_questions(
      rv$workflow$state, cluster$level, cluster$cluster_id
    )
  })

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
  output$walkthrough_progress <- DT::renderDT({
    DT::datatable(
      .tagger_progress_table(rv$workflow, rv$questions), rownames = FALSE,
      options = list(dom = "t", ordering = FALSE)
    )
  })
  output$question_counts <- DT::renderDT({
    if (is.null(rv$questions)) return(DT::datatable(data.frame()))
    DT::datatable(
      .question_projection_counts(rv$questions), rownames = FALSE,
      options = list(dom = "t", ordering = FALSE)
    )
  })
  output$question_table <- DT::renderDT({
    DT::datatable(
      filtered_questions(), selection = "single", rownames = FALSE,
      filter = "top", options = list(pageLength = 20, scrollX = TRUE)
    )
  })
  output$option_table <- DT::renderDT({
    id <- selected_question_id()
    if (is.null(id) || is.null(rv$questions)) return(DT::datatable(data.frame()))
    DT::datatable(
      .question_options(rv$questions, id), rownames = FALSE,
      options = list(pageLength = 15, dom = "tip")
    )
  })
  output$question_record <- shiny::renderText({
    id <- selected_question_id()
    if (is.null(id) || is.null(rv$questions)) return("Select one question.")
    jsonlite::toJSON(
      .question_record(rv$questions, id), auto_unbox = TRUE,
      pretty = TRUE, null = "null", dataframe = "rows", na = "null"
    )
  })
  output$model_activity <- shiny::renderText(rv$model_message)
  output$next_cluster <- DT::renderDT({
    DT::datatable(
      next_cluster_data()$cluster, rownames = FALSE,
      options = list(dom = "t", ordering = FALSE)
    )
  })
  output$next_cluster_questions <- DT::renderDT({
    DT::datatable(
      next_cluster_data()$questions, rownames = FALSE,
      options = list(pageLength = 10, dom = "tip", scrollX = TRUE)
    )
  })
  output$proposal_detail <- shiny::renderText({
    proposal <- selected_proposal()
    if (is.null(proposal)) return("Select or generate a proposal.")
    jsonlite::toJSON(
      .safe_proposal_detail(proposal), auto_unbox = TRUE,
      pretty = TRUE, null = "null", na = "null"
    )
  })
  output$proposal_similarity_summary <- DT::renderDT({
    table <- .similarity_summary(selected_proposal_scores())
    widget <- DT::datatable(
      table, rownames = FALSE, options = list(dom = "t", ordering = FALSE)
    )
    DT::formatRound(widget, "value", digits = 4)
  })
  output$proposal_review_history <- DT::renderDT({
    proposal <- selected_proposal()
    data <- if (is.null(proposal)) tibble::tibble() else
      .proposal_review_events(rv$workflow$state, proposal$proposal_id)
    widget <- DT::datatable(
      data, rownames = FALSE, options = list(pageLength = 8, dom = "tip", scrollX = TRUE)
    )
    numeric <- intersect(c("before_mean", "after_mean", "mean_change"), names(data))
    if (length(numeric)) DT::formatRound(widget, numeric, digits = 4) else widget
  })
  output$proposal_questions <- DT::renderDT({
    data <- selected_proposal_scores()
    widget <- DT::datatable(
      data, rownames = FALSE, filter = "top",
      options = list(pageLength = 20, scrollX = TRUE)
    )
    numeric <- intersect(c("cosine_similarity", "cosine_distance"), names(data))
    if (length(numeric)) DT::formatRound(widget, numeric, digits = 4) else widget
  })
  output$proposal_table <- DT::renderDT({
    data <- if (is.null(rv$workflow)) .empty_proposal_table() else
      .proposal_table(rv$workflow$state)
    widget <- DT::datatable(
      data, rownames = FALSE, filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    )
    if ("confidence" %in% names(data)) {
      DT::formatRound(widget, "confidence", digits = 3)
    } else widget
  })
  output$hierarchy_summary <- shiny::renderText({
    if (is.null(rv$workflow)) return("Resume a tagging run to inspect it.")
    state <- rv$workflow$state
    clusters <- state$clusters %||% data.frame()
    paste0(
      "Run: ", state$run_id, "\n",
      "Revision: ", state$revision, "\n",
      "Stage: ", state$workflow$stage %||% "unknown", "\n",
      "Embedding model: ", state$workflow$embedding_model %||% "unknown", "\n",
      "Clustering method: ", state$workflow$clustering_method %||% "unknown", "\n",
      "Levels: ", paste(state$clusters_by_level %||% integer(), collapse = " -> "), "\n",
      "Cluster records: ", nrow(clusters), "\n",
      "Proposals: ", length(state$proposals %||% list()), "\n",
      "Review decisions: ", length(state$review_events %||% list())
    )
  })
  output$hierarchy <- visNetwork::renderVisNetwork({
    graph <- .tagger_hierarchy_graph(rv$workflow)
    if (!nrow(graph$nodes)) {
      return(visNetwork::visNetwork(
        data.frame(id = "empty", label = "Resume a clustered run"), data.frame()
      ))
    }
    visNetwork::visNetwork(graph$nodes, graph$edges, width = "100%") |>
      visNetwork::visGroups(
        groupname = "pending", color = list(background = "#d9d9d9")
      ) |>
      visNetwork::visGroups(
        groupname = "proposed", color = list(background = "#f4c95d")
      ) |>
      visNetwork::visGroups(
        groupname = "deferred", color = list(background = "#8ecae6")
      ) |>
      visNetwork::visGroups(
        groupname = "accepted", color = list(background = "#70c1b3")
      ) |>
      visNetwork::visGroups(
        groupname = "edited", color = list(background = "#70c1b3")
      ) |>
      visNetwork::visGroups(
        groupname = "rejected", color = list(background = "#ef767a")
      ) |>
      visNetwork::visHierarchicalLayout(direction = "DU", sortMethod = "directed") |>
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
      visNetwork::visEvents(selectNode = paste0(
        "function(properties) {",
        "Shiny.setInputValue('hierarchy_node_selected', properties.nodes[0], ",
        "{priority: 'event'});",
        "}"
      ))
  })
  output$cluster_quality_summary <- DT::renderDT({
    cluster <- selected_quality_cluster()
    data <- .cluster_quality_summary(
      rv$workflow$state, cluster$level, cluster$cluster_id
    )
    widget <- DT::datatable(
      data, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)
    )
    numeric <- intersect(c(
      "mean_centroid_similarity", "minimum_centroid_similarity",
      "mean_tag_similarity", "minimum_tag_similarity"
    ), names(data))
    if (length(numeric)) DT::formatRound(widget, numeric, digits = 4) else widget
  })
  output$cluster_context <- DT::renderDT({
    cluster <- selected_quality_cluster()
    DT::datatable(
      .cluster_hierarchy_context(
        rv$workflow$state, cluster$level, cluster$cluster_id
      ),
      rownames = FALSE, options = list(pageLength = 10, dom = "tip", scrollX = TRUE)
    )
  })
  output$cluster_quality_questions <- DT::renderDT({
    data <- selected_cluster_questions()
    widget <- DT::datatable(
      data, rownames = FALSE, filter = "top",
      options = list(pageLength = 20, scrollX = TRUE)
    )
    numeric <- intersect(c(
      "centroid_similarity", "tag_similarity",
      "best_alternative_similarity", "placement_margin"
    ), names(data))
    if (length(numeric)) DT::formatRound(widget, numeric, digits = 4) else widget
  })
  output$all_cluster_quality <- DT::renderDT({
    data <- if (is.null(rv$workflow)) tibble::tibble() else
      .cluster_node_quality(rv$workflow$state)
    DT::datatable(
      data, rownames = FALSE, filter = "top",
      options = list(pageLength = 15, order = list(list(4, "desc")))
    )
  })
  output$cluster_pca <- shiny::renderPlot({
    cluster <- selected_quality_cluster()
    data <- .cluster_pca_projection(
      rv$workflow$state, cluster$level, cluster$cluster_id
    )
    graphics::plot(
      data$x, data$y,
      col = ifelse(data$selected, "#d1495b", "#c7c7c7"),
      pch = ifelse(data$selected, 19, 16),
      xlab = "PCA dimension 1", ylab = "PCA dimension 2",
      main = paste("Level", cluster$level, "cluster", cluster$cluster_id)
    )
  })
}

#' Construct the survey tagging reviewer application
#'
#' Builds the Shiny application without launching it. The app reads normalized
#' survey knowledge and resumable tagging state from Fluree through novaTagger
#' and novaRush; browser reactive state is never authoritative.
#'
#' @return A Shiny app object.
#' @export
tagger_app <- function() {
  shiny::shinyApp(ui = .walkthrough_app_ui(), server = .walkthrough_app_server)
}

#' Launch the Fluree tagging walkthrough
#'
#' @param launch.browser Passed to [shiny::runApp()].
#' @return A Shiny app object, invisibly when launched.
#' @export
run_fluree_tagger_app <- function(launch.browser = TRUE) {
  app <- tagger_app()
  shiny::runApp(app, launch.browser = launch.browser)
}
