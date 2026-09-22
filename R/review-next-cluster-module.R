.review_next_cluster_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::h4("OpenAI"),
        shiny::textInput(ns("openai_base_url"), "API URL", "https://api.openai.com/v1"),
        shiny::passwordInput(ns("openai_key"), "API key",
                             placeholder = "Uses OPENAI_API_KEY when blank"),
        shiny::textInput(ns("tag_model"), "Generation model", "gpt-5.4-mini"),
        shiny::textInput(ns("tag_embed_model"), "Embedding model", "text-embedding-3-small"),
        shiny::numericInput(ns("tag_sample_size"), "Cluster examples", 8, min = 1, max = 50),
        shiny::actionButton(ns("generate_proposal"), "Generate next tag", class = "btn-primary"),
        shiny::textOutput(ns("model_activity")),
        shiny::hr(),
        shiny::h4("Review current proposal"),
        shiny::selectizeInput(ns("current_proposal"), "Proposal", choices = character(),
                              options = list(maxOptions = 2000)),
        shiny::textInput(ns("edited_tag"), "Tag label"),
        shiny::textInput(ns("reviewer_id"), "Reviewer ID", "reviewer"),
        shiny::textAreaInput(ns("review_rationale"), "Rationale", rows = 3),
        shiny::actionButton(ns("accept_proposal"), "Accept", class = "btn-success"),
        shiny::actionButton(ns("edit_proposal"), "Save modified tag", class = "btn-primary"),
        shiny::actionButton(ns("reject_proposal"), "Reject", class = "btn-danger"),
        shiny::actionButton(ns("defer_proposal"), "Defer")
      ),
      shiny::column(
        9,
        shiny::uiOutput(ns("review_context_heading")),
        DT::DTOutput(ns("next_cluster")),
        shiny::uiOutput(ns("question_table_heading")),
        DT::DTOutput(ns("next_cluster_questions")),
        shiny::hr(),
        shiny::h3("Generated proposal"),
        shiny::uiOutput(ns("proposal_detail")),
        shiny::h4("Question fit with the proposed tag"),
        DT::DTOutput(ns("proposal_similarity_summary"))
      )
    ),
    shiny::hr(),
    shiny::h3("Tagging progress by hierarchy level"),
    shiny::uiOutput(ns("level_progress_bars")),
    DT::DTOutput(ns("level_progress")),
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput(ns("inspect_level"), "Inspect level", choices = character())),
      shiny::column(9, shiny::p(
        "Inspect generated and reviewed tags at every level. This summary does not calculate PCA or cluster diagnostics."
      ))
    ),
    DT::DTOutput(ns("cluster_overview")),
    shiny::tags$details(
      shiny::tags$summary("Previous proposals and review history"),
      DT::DTOutput(ns("proposal_table")), DT::DTOutput(ns("proposal_review_history"))
    )
  )
}

.review_next_cluster_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    notify_error <- function(expr) tryCatch(expr, error = function(error) {
      shiny::showNotification(conditionMessage(error), type = "error", duration = NULL)
      NULL
    })
    require_writable_workspace <- function() {
      if (!isTRUE(rv$demo_mode) && identical(rv$active_branch, "main")) {
        stop("Main is read-only. Create or select a review workspace before saving changes.",
             call. = FALSE)
      }
      invisible(TRUE)
    }
    embedding_dimensions <- function() {
      shiny::req(rv$workflow)
      embeddings <- rv$workflow$state$embeddings
      if (!is.matrix(embeddings) || !ncol(embeddings)) {
        stop("The resumed run does not contain a complete embedding matrix.", call. = FALSE)
      }
      ncol(embeddings)
    }
    openai_provider <- function() {
      if (isTRUE(rv$demo_mode)) return(.demo_model_provider(embedding_dimensions()))
      key <- trimws(input$openai_key %||% "")
      if (!nzchar(key)) key <- Sys.getenv("OPENAI_API_KEY", unset = "")
      config <- novaTagger::openai_config(
        api_key = key, base_url = trimws(input$openai_base_url),
        embed_model = trimws(input$tag_embed_model),
        dimensions = embedding_dimensions(),
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
          if (identical(endpoint, "/responses")) "Generating tag with" else "Embedding tag with",
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

    current_proposal_id <- shiny::reactiveVal(NULL)
    dismissed_proposals <- shiny::reactiveVal(character())
    shiny::observe({
      workflow <- rv$workflow
      if (is.null(workflow)) return()
      current <- current_proposal_id()
      if (!is.null(current) && !is.null(.selected_proposal(workflow$state, current))) return()
      rows <- .proposal_table(workflow$state)
      awaiting <- rows$proposal_id[
        rows$status %in% c("proposed", "deferred") &
          !rows$proposal_id %in% dismissed_proposals()
      ]
      current_proposal_id(if (length(awaiting)) utils::tail(awaiting, 1L) else NULL)
    })
    shiny::observe({
      if (is.null(rv$workflow)) {
        shiny::updateSelectizeInput(session, "current_proposal", choices = character(),
                                    server = TRUE)
        return()
      }
      rows <- .proposal_table(rv$workflow$state)
      rows <- rows[rows$status %in% c("proposed", "deferred"), , drop = FALSE]
      choices <- if (!nrow(rows)) character() else stats::setNames(
        rows$proposal_id,
        paste0("L", rows$level, " / C", rows$cluster_id, ": ", rows$tag,
               " [", rows$status, "]")
      )
      current <- current_proposal_id()
      selected <- if (!is.null(current) && current %in% unname(choices)) current else
        if (length(choices)) utils::tail(unname(choices), 1L) else character()
      shiny::updateSelectizeInput(session, "current_proposal", choices = choices,
                                  selected = selected, server = TRUE)
    })
    shiny::observeEvent(input$current_proposal, {
      selected <- input$current_proposal %||% ""
      current_proposal_id(if (nzchar(selected)) selected else NULL)
    }, ignoreInit = TRUE)
    current_proposal <- shiny::reactive({
      if (is.null(rv$workflow)) return(NULL)
      .selected_proposal(rv$workflow$state, current_proposal_id())
    })
    shiny::observe({
      proposal <- current_proposal()
      shiny::updateTextInput(session, "edited_tag",
                             value = if (is.null(proposal)) "" else proposal$tag)
    })

    shiny::observeEvent(input$generate_proposal, {
      shiny::req(rv$workflow, rv$store)
      require_writable_workspace()
      result <- notify_error(shiny::withProgress(
        message = "Generating and saving the next proposal", value = 0.1, {
          target <- novaTagger::workflow_next_cluster(rv$workflow)
          if (!nrow(target)) stop("No cluster is currently ready for a new proposal.", call. = FALSE)
          shiny::setProgress(.25, detail = paste("Level", target$level[[1]], "cluster", target$cluster_id[[1]]))
          updated <- novaTagger::workflow_propose_next(
            rv$workflow, openai_provider(), sample_size = as.integer(input$tag_sample_size),
            event_callback = model_trace
          )
          shiny::setProgress(1, detail = paste("Saved revision", updated$state$revision))
          updated
        }
      ))
      if (!is.null(result)) {
        rv$workflow <- result
        rows <- .proposal_table(result$state)
        current_proposal_id(utils::tail(rows$proposal_id, 1L))
        shiny::showNotification("Proposal generated and saved.", type = "message")
      }
    })

    review_selected <- function(decision) {
      proposal_id <- current_proposal_id()
      shiny::req(rv$workflow, rv$store, nzchar(proposal_id %||% ""))
      require_writable_workspace()
      result <- notify_error(shiny::withProgress(
        message = paste("Saving reviewer decision:", decision), value = 0.2, {
          proposal <- .selected_proposal(rv$workflow$state, proposal_id)
          if (is.null(proposal)) stop("The current proposal no longer exists.", call. = FALSE)
          if (!proposal$status %in% c("proposed", "deferred")) {
            stop("This proposal has already been reviewed.", call. = FALSE)
          }
          arguments <- list(
            workflow = rv$workflow, proposal_id = proposal_id, decision = decision,
            reviewer_id = trimws(input$reviewer_id), rationale = input$review_rationale
          )
          if (identical(decision, "edited")) {
            arguments$tag <- trimws(input$edited_tag)
            arguments$provider <- openai_provider()
            arguments$event_callback <- model_trace
          }
          updated <- do.call(novaTagger::workflow_review_proposal, arguments)
          shiny::setProgress(1, detail = paste("Saved revision", updated$state$revision))
          updated
        }
      ))
      if (!is.null(result)) {
        rv$workflow <- result
        dismissed_proposals(unique(c(dismissed_proposals(), proposal_id)))
        current_proposal_id(NULL)
        shiny::showNotification(paste("Reviewer decision saved:", decision), type = "message")
      }
    }
    shiny::observeEvent(input$accept_proposal, review_selected("accepted"))
    shiny::observeEvent(input$edit_proposal, review_selected("edited"))
    shiny::observeEvent(input$reject_proposal, review_selected("rejected"))
    shiny::observeEvent(input$defer_proposal, review_selected("deferred"))

    review_cluster_data <- shiny::reactive({
      proposal <- current_proposal()
      if (!is.null(proposal)) return(.proposal_cluster_summary(rv$workflow, proposal))
      .next_cluster_summary(rv$workflow, as.integer(input$tag_sample_size %||% 8L))
    })
    proposal_scores <- shiny::reactive({
      proposal <- current_proposal()
      if (is.null(proposal)) return(tibble::tibble())
      .proposal_question_scores(rv$workflow$state, proposal)
    })
    level_progress <- shiny::reactive(.tagging_level_progress(rv$workflow))

    shiny::observe({
      data <- level_progress()
      choices <- if (!nrow(data)) character() else stats::setNames(data$level, paste("Level", data$level))
      shiny::updateSelectInput(session, "inspect_level", choices = choices)
    })
    output$model_activity <- shiny::renderText(rv$model_message)
    output$review_context_heading <- shiny::renderUI({
      if (is.null(current_proposal())) shiny::h3("Next cluster to tag") else
        shiny::h3("Cluster under review")
    })
    output$question_table_heading <- shiny::renderUI({
      if (is.null(current_proposal())) {
        shiny::h4("Representative and outlying questions used for proposal generation")
      } else shiny::h4("All questions belonging to this proposal, weakest tag fit first")
    })
    output$next_cluster <- DT::renderDT(DT::datatable(
      review_cluster_data()$cluster, rownames = FALSE,
      options = list(dom = "t", ordering = FALSE)
    ))
    output$next_cluster_questions <- DT::renderDT({
      data <- review_cluster_data()$questions
      widget <- DT::datatable(
        data, rownames = FALSE,
        options = list(pageLength = 20, dom = "tip", scrollX = TRUE)
      )
      numeric <- intersect(c("cosine_similarity", "cosine_distance"), names(data))
      if (length(numeric)) DT::formatRound(widget, numeric, 4) else widget
    })
    output$proposal_detail <- shiny::renderUI({
      proposal <- current_proposal()
      if (is.null(proposal)) return(shiny::p("Generate the next proposal or resume one awaiting review."))
      shiny::wellPanel(
        shiny::h3(proposal$tag), shiny::p(proposal$rationale),
        shiny::p(sprintf("Confidence: %.3f", proposal$confidence)),
        shiny::p(paste("Level", proposal$level, "cluster", proposal$cluster_id,
                       "— status", proposal$status))
      )
    })
    output$proposal_similarity_summary <- DT::renderDT({
      data <- .similarity_summary(proposal_scores())
      DT::formatRound(DT::datatable(data, rownames = FALSE,
                                    options = list(dom = "t", ordering = FALSE)), "value", 4)
    })
    output$level_progress <- DT::renderDT(DT::datatable(
      level_progress(), rownames = FALSE, options = list(dom = "t", ordering = FALSE)
    ))
    output$level_progress_bars <- shiny::renderUI({
      data <- level_progress()
      if (!nrow(data)) return(shiny::p("Resume a clustered run to see progress."))
      shiny::tagList(lapply(seq_len(nrow(data)), function(index) {
        percent <- data$progress_percent[[index]]
        shiny::div(
          shiny::strong(paste0("Level ", data$level[[index]], ": ", percent, "% reviewed")),
          shiny::div(class = "progress", shiny::div(
            class = "progress-bar", role = "progressbar",
            style = paste0("width:", percent, "%"),
            paste0(data$reviewed[[index]], " / ", data$clusters[[index]])
          ))
        )
      }))
    })
    output$cluster_overview <- DT::renderDT(DT::datatable(
      .tagging_cluster_overview(rv$workflow, input$inspect_level %||% NULL),
      rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE)
    ))
    output$proposal_table <- DT::renderDT(DT::datatable(
      if (is.null(rv$workflow)) .empty_proposal_table() else .proposal_table(rv$workflow$state),
      rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE)
    ))
    output$proposal_review_history <- DT::renderDT({
      proposal <- current_proposal()
      data <- if (is.null(proposal)) tibble::tibble() else
        .proposal_review_events(rv$workflow$state, proposal$proposal_id)
      DT::datatable(data, rownames = FALSE, options = list(pageLength = 8, dom = "tip", scrollX = TRUE))
    })
  })
}
