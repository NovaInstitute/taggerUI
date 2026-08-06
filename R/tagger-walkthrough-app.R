# Staged from the combined prototype. Cross-package calls are tracked in
# taggerUI/ISSUES.md and will be replaced incrementally with public APIs.
.walkthrough_has_clusters <- function(run) {
  clusters <- run$state$clusters %||% NULL
  is.data.frame(clusters) &&
    nrow(clusters) > 0L &&
    all(c(
      "level", "cluster_id", "parent_cluster", "question_ids", "tag"
    ) %in% names(clusters))
}

.walkthrough_leaf_ids <- function(run) {
  assignments <- run$state$assignments %||% NULL
  if (!is.data.frame(assignments) ||
      !"cluster_level_1" %in% names(assignments)) {
    return(character())
  }
  ids <- unique(as.character(assignments$cluster_level_1))
  ids[!is.na(ids) & nzchar(ids)]
}

.walkthrough_has_hierarchy <- function(run) {
  .walkthrough_has_clusters(run) && length(.walkthrough_leaf_ids(run)) > 0L
}

.walkthrough_graph_data <- function(run) {
  if (!.walkthrough_has_clusters(run)) {
    return(list(nodes = data.frame(), edges = data.frame()))
  }
  clusters <- run$state$clusters
  keys <- paste(clusters$level, clusters$cluster_id, sep = ":")
  status <- vapply(keys, function(key) (run$proposals[[key]] %||% list(status = "pending"))$status %||% "pending", character(1))
  label <- ifelse(is.na(clusters$tag) | !nzchar(clusters$tag),
                  paste0("L", clusters$level, " / C", clusters$cluster_id), clusters$tag)
  nodes <- data.frame(
    id = paste0("L", clusters$level, "C", clusters$cluster_id),
    label = label,
    level = -clusters$level,
    group = status,
    title = paste0("level ", clusters$level, "; cluster ", clusters$cluster_id,
                   "; questions ", lengths(clusters$question_ids), "; status ", status),
    stringsAsFactors = FALSE
  )
  edges <- list()
  k <- 0L
  for (i in seq_len(nrow(clusters))) {
    if (is.na(clusters$parent_cluster[[i]])) next
    k <- k + 1L
    edges[[k]] <- data.frame(
      from = paste0("L", clusters$level[[i]], "C", clusters$cluster_id[[i]]),
      to = paste0("L", clusters$level[[i]] + 1L, "C", clusters$parent_cluster[[i]]),
      stringsAsFactors = FALSE
    )
  }
  list(nodes = nodes, edges = dplyr::bind_rows(edges))
}

.walkthrough_app_ui <- function() {
  shiny::navbarPage(
    "Survey tagger walkthrough",
    shiny::tabPanel(
      "Run",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::textInput("forms_path", "Forms file", path.expand("~/Downloads/forms.Rda")),
          shiny::textInput("run_id", "Run ID", ""),
          shiny::checkboxInput("full_set", "Use complete unique question set", value = TRUE),
          shiny::numericInput("limit_n", "Question limit", value = 3023, min = 1, step = 1),
          shiny::actionButton("load_questions", "Create run + load questions", class = "btn-primary"),
          shiny::actionButton("resume_run", "Resume Fluree run"),
          shiny::hr(),
          shiny::selectInput("model_provider", "Model provider", c("OpenAI" = "openai", "Ollama" = "ollama")),
          shiny::textInput("model_base_url", "Provider URL", "https://api.openai.com/v1"),
          shiny::passwordInput("provider_key", "API key", placeholder = "Uses OPENAI_API_KEY for OpenAI when blank"),
          shiny::textInput("embed_model", "Embedding model", "text-embedding-3-small"),
          shiny::textInput("tag_model", "Tagging model", "gpt-5.4-mini"),
          shiny::numericInput("batch_size", "Embedding batch size", 100, min = 1, max = 500),
          shiny::actionButton("embed", "1. Embed questions"),
          shiny::actionButton("cluster", "2. Infer hierarchy with BERTopic"),
          shiny::hr(),
          shiny::textInput("fluree_url", "Fluree URL", "http://localhost:8090"),
          shiny::textInput("ledger", "Ledger", "survey-tagger"),
          shiny::textInput("ai_branch", "AI branch (blank = timestamped)", ""),
          shiny::numericInput("fluree_batch_size", "Fluree questions per request", 50, min = 1, max = 500),
          shiny::actionButton("prepare_fluree", "3. Publish vectors for evidence"),
          shiny::hr(),
          shiny::numericInput("evidence_limit", "Similar questions per cluster", 5, min = 0, max = 30),
          shiny::numericInput("precedent_limit", "Reviewed precedents per cluster", 6, min = 0, max = 30),
          shiny::numericInput("guidance_limit", "Approved guidance records", 8, min = 0, max = 30),
          shiny::numericInput("sample_size", "Cluster examples", 5, min = 1, max = 20),
          shiny::actionButton("tag_next", "4. Tag next cluster", class = "btn-primary"),
          shiny::actionButton("tag_level", "Tag current level")
        ),
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::column(3, shiny::wellPanel(shiny::h4("Stage"), shiny::textOutput("stage"))),
            shiny::column(3, shiny::wellPanel(shiny::h4("Run / branch"), shiny::textOutput("run_identity"))),
            shiny::column(3, shiny::wellPanel(shiny::h4("Questions"), shiny::textOutput("question_count"))),
            shiny::column(3, shiny::wellPanel(shiny::h4("Tag progress"), shiny::textOutput("tag_progress")))
          ),
          shiny::h4("Automatically inferred hierarchy"),
          shiny::textOutput("hierarchy_summary"),
          visNetwork::visNetworkOutput("hierarchy", height = "620px")
        )
      )
    ),
    shiny::tabPanel(
      "Inspect I/O",
      shiny::p("Every Fluree and OpenAI request and response appears here. Vector values are intentionally replaced by their dimension; prompts, question text, query structure, scores, and model output remain visible."),
      DT::DTOutput("event_table"),
      shiny::h4("Selected event payload"),
      shiny::verbatimTextOutput("event_detail")
    ),
    shiny::tabPanel(
      "Review",
      shiny::fluidRow(
        shiny::column(5,
          shiny::selectInput("review_cluster", "Proposed cluster", choices = character()),
          shiny::textInput("edited_tag", "Tag label"),
          shiny::actionButton("accept_tag", "Accept", class = "btn-primary"),
          shiny::actionButton("edit_tag", "Save edit"),
          shiny::actionButton("reject_tag", "Reject"),
          shiny::actionButton("refresh_similarity", "Recompute tag distances")
        ),
        shiny::column(7,
          shiny::h4("Proposal"),
          shiny::verbatimTextOutput("proposal_detail"),
          shiny::textOutput("review_similarity_summary")
        )
      ),
      shiny::h4("Child tags"),
      DT::DTOutput("review_children"),
      shiny::h4("Accepted or edited precedents"),
      DT::DTOutput("review_positive_precedents"),
      shiny::h4("Rejected precedents"),
      DT::DTOutput("review_negative_precedents"),
      shiny::h4("Applied reviewer-approved guidance"),
      DT::DTOutput("review_guidance"),
      shiny::h4("Questions in this cluster"),
      shiny::p("Farthest questions are shown first. Cosine distance is 0 for identical direction and increases as semantic alignment weakens."),
      DT::DTOutput("review_questions")
    ),
    shiny::tabPanel(
      "Cluster diagnostics",
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectInput("diagnostic_cluster", "Inspect leaf cluster", choices = character()),
          shiny::selectInput("destination_cluster", "Move selected questions to", choices = character()),
          shiny::textInput("structure_reviewer", "Reviewer ID", "reviewer"),
          shiny::textAreaInput("structure_rationale", "Reason for move", rows = 3),
          shiny::actionButton("preview_reclassification", "Preview move", class = "btn-primary"),
          shiny::actionButton("apply_reclassification", "Apply preview"),
          shiny::actionButton("discard_reclassification", "Discard preview")
        ),
        shiny::column(
          8,
          shiny::p("The 2D PCA view is for navigation. Flags and placement recommendations use the original embedding dimensions."),
          shiny::plotOutput(
            "diagnostic_plot", height = "520px",
            click = "diagnostic_plot_click", brush = "diagnostic_plot_brush"
          )
        )
      ),
      shiny::h4("Flagged clusters"),
      DT::DTOutput("diagnostic_clusters"),
      shiny::h4("Questions in selected cluster"),
      shiny::p("Select one or more rows to preview reclassification. Lowest-fit questions appear first."),
      DT::DTOutput("diagnostic_questions"),
      shiny::h4("Ranked placements for selected questions"),
      DT::DTOutput("diagnostic_placements"),
      shiny::h4("Preview metrics"),
      DT::DTOutput("reclassification_metrics")
    ),
    shiny::tabPanel(
      "Guidance",
      shiny::fluidRow(
        shiny::column(
          5,
          shiny::h4("Create candidate"),
          shiny::textAreaInput(
            "guidance_text", "Reusable guidance", rows = 4
          ),
          shiny::selectInput(
            "guidance_kind", "Kind",
            choices = c("Constraint" = "constraint", "Decision" = "decision",
                        "Fact" = "fact")
          ),
          shiny::textInput(
            "guidance_tags", "Tags (comma-separated)", "labels"
          ),
          shiny::selectInput(
            "guidance_severity", "Constraint severity",
            choices = c("None" = "", "Must" = "must", "Should" = "should",
                        "Prefer" = "prefer")
          ),
          shiny::textAreaInput(
            "guidance_candidate_rationale", "Candidate rationale", rows = 2
          ),
          shiny::actionButton(
            "create_guidance", "Save candidate", class = "btn-primary"
          )
        ),
        shiny::column(
          7,
          shiny::h4("Review selected guidance"),
          shiny::textInput("guidance_reviewer", "Reviewer ID", "reviewer"),
          shiny::textAreaInput(
            "guidance_review_rationale", "Review rationale", rows = 3
          ),
          shiny::textAreaInput(
            "guidance_replacement", "Replacement text for supersession", rows = 3
          ),
          shiny::actionButton("approve_guidance", "Approve"),
          shiny::actionButton("reject_guidance", "Reject"),
          shiny::actionButton("retire_guidance", "Retire"),
          shiny::actionButton("supersede_guidance", "Supersede"),
          shiny::actionButton("refresh_guidance", "Refresh")
        )
      ),
      shiny::p(
        "Only approved guidance is supplied to the tagging model. Reject, ",
        "retire, and supersede actions require an explanation."
      ),
      DT::DTOutput("guidance_table"),
      shiny::h4("Selected guidance provenance"),
      shiny::verbatimTextOutput("guidance_detail"),
      shiny::h4("Proposals using selected guidance on the active AI branch"),
      DT::DTOutput("guidance_usage")
    ),
    shiny::tabPanel(
      "Questions & cost",
      shiny::h4("Question preview"),
      DT::DTOutput("questions"),
      shiny::h4("Planning estimate (USD)"),
      shiny::tableOutput("cost")
    )
  )
}

.walkthrough_app_server <- function(input, output, session) {
  rv <- shiny::reactiveValues(
    run = NULL, events = list(), structure_preview = NULL,
    guidance = .empty_tagging_guidance()
  )
  event_callback <- function(event) {
    shiny::isolate(rv$events <- c(rv$events, list(event)))
  }
  provider_config <- shiny::reactive({
    if (identical(input$model_provider, "openai")) {
      key <- input$provider_key
      if (!nzchar(key)) key <- Sys.getenv("OPENAI_API_KEY", unset = "")
      return(openai_model_provider(openai_config(
        api_key = key, base_url = input$model_base_url,
        embed_model = input$embed_model, tagger_model = input$tag_model
      )))
    }
    ollama_model_provider(ollama_config(
      base_url = input$model_base_url,
      embed_model = input$embed_model,
      tagger_model = input$tag_model
    ))
  })
  f_config <- shiny::reactive(fluree_config(
    base_url = input$fluree_url, ledger = input$ledger,
    ai_branch = input$ai_branch, trace_callback = event_callback
  ))
  shiny::observeEvent(input$model_provider, {
    if (identical(input$model_provider, "openai")) {
      shiny::updateTextInput(session, "model_base_url", value = "https://api.openai.com/v1")
      shiny::updateTextInput(session, "embed_model", value = "text-embedding-3-small")
      shiny::updateTextInput(session, "tag_model", value = "gpt-5.4-mini")
    } else {
      config <- ollama_config()
      shiny::updateTextInput(session, "model_base_url", value = config$base_url)
      shiny::updateTextInput(session, "embed_model", value = config$embed_model)
      shiny::updateTextInput(session, "tag_model", value = config$tagger_model)
    }
  }, ignoreInit = TRUE)
  update_run <- function(value) {
    previous <- names((rv$run %||% list(proposals = list()))$proposals %||% list())
    proposals <- names(value$proposals %||% list())
    added <- setdiff(proposals, previous)
    current <- shiny::isolate(input$review_cluster %||% "")
    selected <- if (length(added)) utils::tail(added, 1L) else if (current %in% proposals) current else if (length(proposals)) utils::tail(proposals, 1L) else character()
    rv$run <- value
    shiny::updateSelectInput(session, "review_cluster", choices = proposals, selected = selected)
    leaf_ids <- .walkthrough_leaf_ids(value)
    current_leaf <- shiny::isolate(input$diagnostic_cluster %||% "")
    selected_leaf <- if (current_leaf %in% leaf_ids) {
      current_leaf
    } else if (length(leaf_ids)) {
      leaf_ids[[1]]
    } else {
      character()
    }
    shiny::updateSelectInput(
      session, "diagnostic_cluster",
      choices = leaf_ids, selected = selected_leaf
    )
    shiny::updateSelectInput(
      session, "destination_cluster",
      choices = setdiff(leaf_ids, selected_leaf)
    )
  }
  notify_error <- function(expr) {
    tryCatch(expr, error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
      NULL
    })
  }

  refresh_guidance <- function() {
    result <- notify_error(fluree_list_tagging_guidance(
      f_config(), branch = f_config()$main_branch
    ))
    if (!is.null(result)) rv$guidance <- result
    invisible(result)
  }

  selected_guidance <- shiny::reactive({
    selected <- input$guidance_table_rows_selected
    shiny::req(length(selected) == 1L, nrow(rv$guidance) >= selected)
    .tagging_guidance_from_row(rv$guidance[selected, , drop = FALSE])
  })

  shiny::observeEvent(input$refresh_guidance, refresh_guidance())
  shiny::observeEvent(input$create_guidance, {
    tags <- trimws(strsplit(input$guidance_tags, ",", fixed = TRUE)[[1]])
    severity <- input$guidance_severity
    if (!identical(input$guidance_kind, "constraint") || !nzchar(severity)) {
      severity <- NULL
    }
    guidance <- notify_error(new_tagging_guidance(
      input$guidance_text,
      kind = input$guidance_kind,
      tags = tags,
      rationale = input$guidance_candidate_rationale,
      severity = severity
    ))
    if (is.null(guidance)) return()
    saved <- notify_error({
      fluree_upsert_tagging_guidance(
        guidance, f_config(), branch = f_config()$main_branch
      )
      TRUE
    })
    if (isTRUE(saved)) refresh_guidance()
  })

  review_guidance <- function(decision) {
    guidance <- selected_guidance()
    reviewed <- notify_error(review_tagging_guidance(
      guidance,
      decision,
      reviewer_id = input$guidance_reviewer,
      rationale = input$guidance_review_rationale,
      expected_revision = guidance$revision
    ))
    if (is.null(reviewed)) return()
    notify_error(fluree_upsert_tagging_guidance(
      reviewed, f_config(), branch = f_config()$main_branch,
      expected_revision = guidance$revision
    ))
    refresh_guidance()
  }
  shiny::observeEvent(input$approve_guidance, review_guidance("approve"))
  shiny::observeEvent(input$reject_guidance, review_guidance("reject"))
  shiny::observeEvent(input$retire_guidance, review_guidance("retire"))
  shiny::observeEvent(input$supersede_guidance, {
    guidance <- selected_guidance()
    result <- notify_error(supersede_tagging_guidance(
      guidance,
      replacement_text = input$guidance_replacement,
      reviewer_id = input$guidance_reviewer,
      rationale = input$guidance_review_rationale,
      expected_revision = guidance$revision
    ))
    if (is.null(result)) return()
    notify_error(fluree_upsert_tagging_guidance(
      result, f_config(), branch = f_config()$main_branch,
      expected_revision = guidance$revision
    ))
    refresh_guidance()
  })

  shiny::observeEvent(input$load_questions, {
    run_id <- trimws(input$run_id)
    if (!nzchar(run_id)) run_id <- .new_tag_run_id()
    branch <- trimws(input$ai_branch)
    if (!nzchar(branch)) branch <- fluree_ai_branch_name(run_id)
    config <- f_config()
    config$ai_branch <- branch
    store <- notify_error(prepare_fluree_tag_store(config, run_id, branch))
    if (is.null(store)) return()
    if (tag_store_exists(store)) {
      shiny::updateTextInput(session, "run_id", value = run_id)
      shiny::updateTextInput(session, "ai_branch", value = branch)
      shiny::showNotification(
        paste0(
          "Run '", run_id, "' already exists on branch '", branch,
          "'. Click 'Resume Fluree run' to continue it, or choose a new run ID."
        ),
        type = "warning", duration = NULL
      )
      return()
    }
    result <- notify_error(new_tagger_walkthrough(
      input$forms_path, limit_n = if (isTRUE(input$full_set)) Inf else input$limit_n,
      store = store, run_id = run_id, event_callback = event_callback
    ))
    if (!is.null(result)) {
      result$fluree_config <- config
      shiny::updateTextInput(session, "run_id", value = run_id)
      shiny::updateTextInput(session, "ai_branch", value = branch)
      update_run(result)
    }
  })
  shiny::observeEvent(input$resume_run, {
    run_id <- trimws(input$run_id)
    branch <- trimws(input$ai_branch)
    if (!nzchar(run_id) || !nzchar(branch)) {
      shiny::showNotification(
        "Enter both the run ID and AI branch to resume.", type = "warning"
      )
      return()
    }
    config <- f_config()
    config$ai_branch <- branch
    store <- fluree_tag_store(config, run_id, branch)
    result <- notify_error(load_tagger_walkthrough(store, config))
    if (!is.null(result)) update_run(result)
  })
  shiny::observeEvent(input$embed, {
    shiny::req(rv$run)
    result <- notify_error(shiny::withProgress(message = "Embedding questions", value = 0, {
      walkthrough_embed(
        rv$run, provider_config(), batch_size = input$batch_size,
        event_callback = event_callback,
        progress_callback = function(done, total, label) {
          shiny::setProgress(value = done / total, detail = label)
        }
      )
    }))
    if (!is.null(result)) update_run(result)
  })
  shiny::observeEvent(input$cluster, {
    shiny::req(rv$run)
    result <- notify_error(shiny::withProgress(message = "Running BERTopic", value = 0.1, {
      out <- walkthrough_infer_bertopic(rv$run, event_callback = event_callback)
      shiny::incProgress(0.9)
      out
    }))
    if (!is.null(result)) update_run(result)
  })
  shiny::observeEvent(input$prepare_fluree, {
    shiny::req(rv$run)
    result <- notify_error(shiny::withProgress(message = "Loading questions into Fluree", value = 0, {
      walkthrough_prepare_fluree(
        rv$run, f_config(), ai_branch = if (nzchar(input$ai_branch)) input$ai_branch else NULL,
        event_callback = event_callback, batch_size = input$fluree_batch_size,
        progress_callback = function(done, total, label) {
          shiny::setProgress(value = done / total, detail = label)
        }
      )
    }))
    if (!is.null(result)) update_run(result)
  })
  tag_once <- function(run, level = NULL) walkthrough_tag_next(
    run, provider_config(), level = level, evidence_limit = input$evidence_limit,
    precedent_limit = input$precedent_limit,
    guidance_limit = input$guidance_limit,
    sample_size = input$sample_size, event_callback = event_callback
  )
  shiny::observeEvent(input$tag_next, {
    shiny::req(rv$run)
    result <- notify_error(tag_once(rv$run))
    if (!is.null(result)) update_run(result)
  })
  shiny::observeEvent(input$tag_level, {
    shiny::req(rv$run, rv$run$state)
    next_one <- walkthrough_next_cluster(rv$run)
    if (!nrow(next_one)) return()
    level <- next_one$level[[1]]
    total <- sum(rv$run$state$clusters$level == level &
      (is.na(rv$run$state$clusters$tag) | !nzchar(rv$run$state$clusters$tag) |
         rv$run$state$clusters$tag == "untagged"))
    result <- notify_error(shiny::withProgress(message = paste("Tagging level", level), value = 0, {
      out <- rv$run
      for (i in seq_len(total)) {
        out <- tag_once(out, level)
        shiny::incProgress(1 / total, detail = paste(i, "of", total))
      }
      out
    }))
    if (!is.null(result)) update_run(result)
  })

  review <- function(decision) {
    shiny::req(rv$run, input$review_cluster)
    parts <- strsplit(input$review_cluster, ":", fixed = TRUE)[[1]]
    result <- notify_error(walkthrough_review_tag(
      rv$run, parts[[1]], parts[[2]], decision,
      tag = input$edited_tag, event_callback = event_callback,
      provider = provider_config()
    ))
    if (!is.null(result)) update_run(result)
  }
  shiny::observeEvent(input$accept_tag, review("accepted"))
  shiny::observeEvent(input$edit_tag, review("edited"))
  shiny::observeEvent(input$reject_tag, review("rejected"))
  shiny::observeEvent(input$refresh_similarity, {
    shiny::req(rv$run, input$review_cluster)
    parts <- strsplit(input$review_cluster, ":", fixed = TRUE)[[1]]
    result <- notify_error(walkthrough_refresh_tag_embedding(
      rv$run, parts[[1]], parts[[2]], provider_config(), event_callback
    ))
    if (!is.null(result)) update_run(result)
  })
  shiny::observeEvent(input$review_cluster, {
    proposal <- rv$run$proposals[[input$review_cluster]] %||% NULL
    if (!is.null(proposal)) {
      shiny::updateTextInput(session, "edited_tag", value = proposal$tag)
      if (is.null(proposal$tag_embedding)) {
        parts <- strsplit(input$review_cluster, ":", fixed = TRUE)[[1]]
        result <- notify_error(walkthrough_refresh_tag_embedding(
          rv$run, parts[[1]], parts[[2]], provider_config(), event_callback
        ))
        if (!is.null(result)) update_run(result)
      }
    }
  })

  diagnostics <- shiny::reactive({
    shiny::req(.walkthrough_has_hierarchy(rv$run))
    diagnose_tagging_clusters(rv$run$state)
  })
  projection <- shiny::reactive({
    shiny::req(.walkthrough_has_hierarchy(rv$run))
    question_projection_2d(rv$run$state)
  })
  selected_cluster_questions <- shiny::reactive({
    shiny::req(input$diagnostic_cluster)
    data <- diagnostics()$questions
    data[data$current_cluster == input$diagnostic_cluster, , drop = FALSE] |>
      dplyr::arrange(.data$current_centroid_similarity)
  })
  selected_diagnostic_ids <- shiny::reactive({
    data <- selected_cluster_questions()
    selected_rows <- input$diagnostic_questions_rows_selected
    ids <- if (length(selected_rows)) data$question_id[selected_rows] else character()
    clicked <- input$diagnostic_plot_click
    if (!is.null(clicked)) {
      plotted <- projection()
      nearest <- shiny::nearPoints(
        plotted, clicked, xvar = "x", yvar = "y", maxpoints = 1L
      )
      if (nrow(nearest) && nearest$cluster_id[[1]] == input$diagnostic_cluster) {
        ids <- unique(c(ids, nearest$question_id[[1]]))
      }
    }
    ids
  })

  shiny::observeEvent(input$diagnostic_cluster, {
    shiny::req(.walkthrough_has_hierarchy(rv$run))
    leaf_ids <- .walkthrough_leaf_ids(rv$run)
    shiny::updateSelectInput(
      session, "destination_cluster",
      choices = setdiff(leaf_ids, input$diagnostic_cluster)
    )
    rv$structure_preview <- NULL
  })
  shiny::observeEvent(input$preview_reclassification, {
    shiny::req(rv$run$state, input$destination_cluster)
    ids <- selected_diagnostic_ids()
    if (!length(ids)) {
      shiny::showNotification("Select at least one question.", type = "warning")
      return()
    }
    rv$structure_preview <- notify_error(preview_question_reclassification(
      rv$run$state, ids, input$destination_cluster
    ))
  })
  shiny::observeEvent(input$discard_reclassification, {
    if (!is.null(rv$structure_preview)) {
      discard_structure_change(rv$structure_preview)
      rv$structure_preview <- NULL
    }
  })
  shiny::observeEvent(input$apply_reclassification, {
    shiny::req(rv$run$state, rv$structure_preview)
    state <- notify_error(apply_structure_change(
      rv$run$state,
      rv$structure_preview,
      reviewer_id = input$structure_reviewer,
      rationale = input$structure_rationale
    ))
    if (is.null(state)) return()
    if (!is.null(rv$run$fluree_config)) {
      store <- fluree_tag_store(
        rv$run$fluree_config,
        run_id = state$run_id,
        branch = rv$run$fluree_config$ai_branch
      )
      state <- notify_error(tag_store_save(store, state))
      if (is.null(state)) return()
    }
    rv$run$state <- state
    rv$run$proposals <- lapply(rv$run$proposals, function(proposal) {
      domain <- state$proposals[[proposal$proposal_id %||% ""]] %||% NULL
      domain %||% proposal
    })
    rv$run <- .walkthrough_checkpoint(rv$run)
    rv$structure_preview <- NULL
    update_run(rv$run)
  })

  output$stage <- shiny::renderText(if (is.null(rv$run)) "not started" else rv$run$stage)
  output$run_identity <- shiny::renderText({
    if (is.null(rv$run)) return("not started")
    paste0(
      rv$run$state$run_id, "\n",
      rv$run$fluree_config$ai_branch %||% rv$run$store$metadata$branch %||% "",
      "\nrevision ", rv$run$state$revision
    )
  })
  output$question_count <- shiny::renderText(if (is.null(rv$run)) "0" else format(nrow(rv$run$questions), big.mark = ","))
  output$tag_progress <- shiny::renderText({
    if (!.walkthrough_has_clusters(rv$run)) return("0 / 0")
    tags <- rv$run$state$clusters$tag
    paste(sum(!is.na(tags) & nzchar(tags)), "/", length(tags))
  })
  output$hierarchy_summary <- shiny::renderText({
    if (!.walkthrough_has_clusters(rv$run)) {
      return("Run BERTopic to infer cluster counts.")
    }
    paste("Clusters from leaf to root:", paste(rv$run$state$clusters_by_level, collapse = " -> "))
  })
  output$hierarchy <- visNetwork::renderVisNetwork({
    graph <- .walkthrough_graph_data(rv$run)
    if (!nrow(graph$nodes)) return(visNetwork::visNetwork(data.frame(id = "empty", label = "Hierarchy not built"), data.frame()))
    visNetwork::visNetwork(graph$nodes, graph$edges, width = "100%") |>
      visNetwork::visGroups(groupname = "pending", color = list(background = "#d9d9d9")) |>
      visNetwork::visGroups(groupname = "proposed", color = list(background = "#f4c95d")) |>
      visNetwork::visGroups(groupname = "accepted", color = list(background = "#70c1b3")) |>
      visNetwork::visGroups(groupname = "edited", color = list(background = "#70c1b3")) |>
      visNetwork::visGroups(groupname = "rejected", color = list(background = "#ef767a")) |>
      visNetwork::visHierarchicalLayout(direction = "DU", sortMethod = "directed") |>
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
  })
  output$questions <- DT::renderDT({
    if (is.null(rv$run)) return(DT::datatable(data.frame()))
    DT::datatable(utils::head(rv$run$questions[, c("id", "caption"), drop = FALSE], 500),
                  options = list(pageLength = 15), rownames = FALSE)
  })
  output$guidance_table <- DT::renderDT({
    data <- rv$guidance
    if (nrow(data)) {
      data$tags <- vapply(
        data$tags, paste, collapse = ", ", FUN.VALUE = character(1)
      )
    }
    DT::datatable(
      data,
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 15, order = list(list(6, "asc")))
    )
  })
  output$guidance_detail <- shiny::renderText({
    selected <- input$guidance_table_rows_selected
    if (length(selected) != 1L || nrow(rv$guidance) < selected) {
      return("Select one guidance record.")
    }
    row <- rv$guidance[selected, , drop = FALSE]
    row$tags <- vapply(
      row$tags, paste, collapse = ", ", FUN.VALUE = character(1)
    )
    jsonlite::toJSON(row, auto_unbox = TRUE, pretty = TRUE, null = "null")
  })
  output$guidance_usage <- DT::renderDT({
    selected <- input$guidance_table_rows_selected
    if (length(selected) != 1L || nrow(rv$guidance) < selected) {
      return(DT::datatable(data.frame()))
    }
    data <- fluree_tagging_guidance_usage(
      f_config(),
      rv$guidance$guidance_id[[selected]],
      branch = f_config()$ai_branch
    )
    DT::datatable(
      data, rownames = FALSE, options = list(pageLength = 10, dom = "tip")
    )
  })
  output$event_table <- DT::renderDT({
    if (!length(rv$events)) return(DT::datatable(data.frame()))
    rows <- lapply(seq_along(rv$events), function(i) {
      e <- rv$events[[i]]
      data.frame(index = i, time = e$time %||% "", system = e$system %||% "",
                 direction = e$direction %||% "", operation = e$operation %||% e$endpoint %||% "",
                 stage = e$stage %||% "", stringsAsFactors = FALSE)
    })
    DT::datatable(dplyr::bind_rows(rows), selection = "single", rownames = FALSE,
                  options = list(pageLength = 20, order = list(list(0, "desc"))))
  })
  output$event_detail <- shiny::renderText({
    selected <- input$event_table_rows_selected
    if (!length(selected) || !length(rv$events)) return("Select an event above.")
    jsonlite::toJSON(rv$events[[selected]], auto_unbox = TRUE, pretty = TRUE, null = "null")
  })
  output$proposal_detail <- shiny::renderText({
    if (is.null(rv$run) || !nzchar(input$review_cluster %||% "")) return("Select a proposal.")
    proposal <- rv$run$proposals[[input$review_cluster]]
    summary <- proposal[setdiff(names(proposal), c("tag_embedding", "evidence", "prompt", "raw_response"))]
    summary$embedding_dimension <- length(proposal$tag_embedding %||% numeric())
    jsonlite::toJSON(summary, auto_unbox = TRUE, pretty = TRUE, null = "null")
  })
  review_data <- shiny::reactive({
    shiny::req(rv$run, input$review_cluster)
    parts <- strsplit(input$review_cluster, ":", fixed = TRUE)[[1]]
    walkthrough_cluster_review_data(rv$run, parts[[1]], parts[[2]])
  })
  output$review_similarity_summary <- shiny::renderText({
    data <- review_data()
    if (is.na(data$mean_similarity)) {
      "No tag embedding is stored yet. Click 'Recompute tag distances'."
    } else {
      paste0("Mean question-to-tag similarity: ", format(round(data$mean_similarity, 3), nsmall = 3),
             " across ", data$question_count, " questions.")
    }
  })
  output$review_children <- DT::renderDT({
    data <- review_data()$children
    DT::datatable(data, rownames = FALSE, options = list(pageLength = 10, dom = "tip"))
  })
  proposal_precedents <- shiny::reactive({
    shiny::req(rv$run, input$review_cluster)
    proposal <- rv$run$proposals[[input$review_cluster]]
    evidence <- proposal$evidence
    if (!is.list(evidence) || is.data.frame(evidence)) {
      return(.empty_tag_precedents())
    }
    evidence$precedents %||% .empty_tag_precedents()
  })
  output$review_positive_precedents <- DT::renderDT({
    data <- proposal_precedents()
    DT::datatable(
      data[data$kind == "positive", , drop = FALSE],
      rownames = FALSE,
      options = list(pageLength = 8, dom = "tip")
    )
  })
  output$review_negative_precedents <- DT::renderDT({
    data <- proposal_precedents()
    DT::datatable(
      data[data$kind == "negative", , drop = FALSE],
      rownames = FALSE,
      options = list(pageLength = 8, dom = "tip")
    )
  })
  output$review_guidance <- DT::renderDT({
    shiny::req(rv$run, input$review_cluster)
    proposal <- rv$run$proposals[[input$review_cluster]]
    evidence <- proposal$evidence
    data <- if (is.list(evidence) && !is.data.frame(evidence)) {
      evidence$guidance %||% .empty_tagging_guidance()
    } else .empty_tagging_guidance()
    if (nrow(data)) data$tags <- vapply(data$tags, paste, collapse = ", ", FUN.VALUE = character(1))
    DT::datatable(
      data, rownames = FALSE, options = list(pageLength = 8, dom = "tip")
    )
  })
  output$review_questions <- DT::renderDT({
    data <- review_data()$questions
    table <- DT::datatable(
      data, rownames = FALSE, filter = "top",
      options = list(pageLength = 25, order = list(list(3, "desc")))
    )
    DT::formatRound(table, c("cosine_similarity", "cosine_distance"), digits = 4)
  })
  output$diagnostic_plot <- shiny::renderPlot({
    data <- projection()
    selected <- data$cluster_id == input$diagnostic_cluster
    graphics::plot(
      data$x, data$y,
      col = ifelse(selected, "#d1495b", "#c7c7c7"),
      pch = ifelse(selected, 19, 16),
      xlab = "PCA dimension 1", ylab = "PCA dimension 2",
      main = paste("Leaf cluster", input$diagnostic_cluster)
    )
  })
  output$diagnostic_clusters <- DT::renderDT({
    DT::datatable(
      diagnostics()$clusters,
      rownames = FALSE,
      options = list(pageLength = 10, order = list(list(6, "desc")))
    )
  })
  output$diagnostic_questions <- DT::renderDT({
    table <- DT::datatable(
      selected_cluster_questions(),
      selection = "multiple",
      rownames = FALSE,
      options = list(pageLength = 20, order = list(list(3, "asc")))
    )
    DT::formatRound(
      table,
      c(
        "current_centroid_similarity", "best_alternative_similarity",
        "placement_margin", "neighbour_disagreement"
      ),
      digits = 4
    )
  })
  output$diagnostic_placements <- DT::renderDT({
    ids <- selected_diagnostic_ids()
    if (!length(ids)) return(DT::datatable(data.frame()))
    table <- DT::datatable(
      rank_question_placements(rv$run$state, ids),
      rownames = FALSE,
      options = list(pageLength = 20)
    )
    DT::formatRound(table, c("centroid_similarity", "tag_similarity"), digits = 4)
  })
  output$reclassification_metrics <- DT::renderDT({
    if (is.null(rv$structure_preview)) return(DT::datatable(data.frame()))
    DT::datatable(
      rv$structure_preview$metrics,
      rownames = FALSE,
      options = list(pageLength = 10, dom = "tip")
    )
  })
  output$cost <- shiny::renderTable({
    if (is.null(rv$run)) return(NULL)
    if (!identical(input$model_provider, "openai")) {
      return(data.frame(note = "No pricing estimate is configured for this provider."))
    }
    n_clusters <- if (is.null(rv$run$state)) max(1L, ceiling(nrow(rv$run$questions) / 10)) else nrow(rv$run$state$clusters)
    estimate_openai_tagger_cost(nrow(rv$run$questions), n_clusters,
                                calls_per_cluster = 1,
                                tagger_input_per_1m = 0.75,
                                tagger_output_per_1m = 4.50)
  }, digits = 4)
}

#' Construct the survey tagging reviewer application
#'
#' @description Builds the staged Shiny application without launching it. The
#' current server still contains compatibility calls from the combined
#' prototype; these are tracked in `taggerUI/ISSUES.md`.
#'
#' @return A Shiny app object.
#' @export
tagger_app <- function() {
  required <- c("shiny", "visNetwork", "DT")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Install the suggested UI packages: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  shiny::shinyApp(ui = .walkthrough_app_ui(), server = .walkthrough_app_server)
}

#' Launch the Fluree tagging walkthrough
#'
#' @description Starts a local Shiny interface for loading forms, embedding with
#' a configured model provider, inferring BERTopic hierarchy levels,
#' loading/querying Fluree, tagging
#' bottom-up, inspecting every request/response, and reviewing proposals.
#'
#' @param launch.browser Passed to [shiny::runApp()].
#' @return A Shiny app object, invisibly when launched.
#' @export
run_fluree_tagger_app <- function(launch.browser = TRUE) {
  app <- tagger_app()
  shiny::runApp(app, launch.browser = launch.browser)
}
