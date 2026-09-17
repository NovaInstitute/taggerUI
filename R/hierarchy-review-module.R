# Top-down, reviewer-oriented inspection of hierarchy splits.

.hierarchy_split_parents <- function(state) {
  clusters <- state$clusters %||% data.frame()
  if (!nrow(clusters)) return(clusters)
  has_children <- vapply(seq_len(nrow(clusters)), function(index) {
    any(
      clusters$level == clusters$level[[index]] - 1L &
        as.character(clusters$parent_cluster) ==
          as.character(clusters$cluster_id[[index]])
    )
  }, logical(1))
  clusters[has_children, , drop = FALSE]
}

.hierarchy_split_choices <- function(state) {
  parents <- .hierarchy_split_parents(state)
  if (!nrow(parents)) return(stats::setNames(character(), character()))
  parents <- parents[order(-parents$level, as.character(parents$cluster_id)), , drop = FALSE]
  key <- paste(parents$level, parents$cluster_id, sep = ":")
  label <- paste0(
    "Level ", parents$level, ": ",
    ifelse(is.na(parents$tag) | !nzchar(parents$tag), "(untagged)", parents$tag)
  )
  stats::setNames(key, label)
}

.hierarchy_split_path <- function(state, level, cluster_id) {
  clusters <- state$clusters
  path <- character()
  current_level <- as.integer(level)
  current_id <- as.character(cluster_id)
  repeat {
    hit <- which(
      clusters$level == current_level &
        as.character(clusters$cluster_id) == current_id
    )
    if (length(hit) != 1L) break
    tag <- trimws(as.character(clusters$tag[[hit]] %||% ""))
    path <- c(if (nzchar(tag)) tag else "(untagged)", path)
    parent <- clusters$parent_cluster[[hit]]
    if (is.na(parent)) break
    current_level <- current_level + 1L
    current_id <- as.character(parent)
  }
  paste(path, collapse = " → ")
}

.hierarchy_split_children <- function(state, level, cluster_id,
                                      examples_per_child = 3L) {
  children <- state$clusters[
    state$clusters$level == as.integer(level) - 1L &
      as.character(state$clusters$parent_cluster) == as.character(cluster_id), ,
    drop = FALSE
  ]
  if (!nrow(children)) return(tibble::tibble())
  examples <- vapply(children$question_ids, function(ids) {
    captions <- state$questions$caption[match(as.character(ids), state$questions$id)]
    captions <- captions[!is.na(captions)]
    paste(utils::head(as.character(captions), examples_per_child), collapse = " | ")
  }, character(1))
  tags <- trimws(as.character(children$tag))
  repeated <- !is.na(tags) & nzchar(tags) & duplicated(tolower(tags)) |
    !is.na(tags) & nzchar(tags) & duplicated(tolower(tags), fromLast = TRUE)
  tibble::tibble(
    child_level = as.integer(children$level),
    child_cluster = as.character(children$cluster_id),
    tag = ifelse(is.na(tags) | !nzchar(tags), "(untagged)", tags),
    question_count = lengths(children$question_ids),
    representative_questions = examples,
    repeated_sibling_tag = repeated
  )
}

.hierarchy_review_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Review hierarchy splits"),
    shiny::p(
      "Work from the top down. For each parent, ask whether its child tags are "
        , "meaningfully different and more specific than the parent."
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectizeInput(ns("split_parent"), "Parent node", choices = character(),
                              options = list(maxOptions = 2000)),
        shiny::h4("Current path"),
        shiny::textOutput(ns("parent_path")),
        shiny::hr(),
        shiny::h4("Queue a tag correction"),
        shiny::selectInput(ns("correction_child"), "Child to correct", choices = character()),
        shiny::textInput(ns("correction_tag"), "Replacement tag"),
        shiny::textAreaInput(ns("correction_rationale"), "Why this is clearer", rows = 2),
        shiny::actionButton(ns("queue_correction"), "Queue correction for review",
                            class = "btn-primary"),
        shiny::p("This creates a reviewable proposal; it does not change the tag until accepted.")
      ),
      shiny::column(
        8,
        shiny::h4("How to assess this split"),
        shiny::tags$ul(
          shiny::tags$li("Each child should be more specific than the parent."),
          shiny::tags$li("Sibling tags should describe genuinely different groups."),
          shiny::tags$li("Repeated sibling tags are a signal to inspect or merge later.")
        )
      )
    ),
    shiny::h4("Child groups and representative questions"),
    DT::DTOutput(ns("split_children")),
    shiny::p("Use the correction queue for label changes. Structural merge, collapse, split, and question moves remain intentionally separate from label review.")
  )
}

.hierarchy_review_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      if (is.null(rv$workflow)) {
        shiny::updateSelectizeInput(session, "split_parent", choices = character(), server = TRUE)
        return()
      }
      choices <- .hierarchy_split_choices(rv$workflow$state)
      current <- shiny::isolate(input$split_parent %||% "")
      selected <- if (current %in% unname(choices)) current else
        if (length(choices)) unname(choices)[[1]] else character()
      shiny::updateSelectizeInput(session, "split_parent", choices = choices,
                                  selected = selected, server = TRUE)
    })
    selected_parent <- shiny::reactive({
      shiny::req(rv$workflow, nzchar(input$split_parent %||% ""))
      .parse_cluster_key(input$split_parent)
    })
    shiny::observe({
      if (is.null(rv$workflow) || !nzchar(input$split_parent %||% "")) return()
      parent <- selected_parent()
      children <- .hierarchy_split_children(
        rv$workflow$state, parent$level, parent$cluster_id
      )
      choices <- if (!nrow(children)) character() else stats::setNames(
        paste(children$child_level, children$child_cluster, sep = ":"),
        paste0("Level ", children$child_level, ": ", children$tag)
      )
      shiny::updateSelectInput(session, "correction_child", choices = choices)
    })
    shiny::observeEvent(input$queue_correction, {
      shiny::req(rv$workflow, rv$store, nzchar(input$correction_child %||% ""))
      if (!isTRUE(rv$demo_mode) && identical(rv$active_branch, "main")) {
        shiny::showNotification(
          "Main is read-only. Create or select a review workspace before queuing a correction.",
          type = "error", duration = NULL
        )
        return()
      }
      tag <- trimws(input$correction_tag %||% "")
      if (!nzchar(tag)) {
        shiny::showNotification("Enter a replacement tag.", type = "error")
        return()
      }
      child <- .parse_cluster_key(input$correction_child)
      result <- tryCatch({
        state <- novaTagger::register_tag_proposal(
          rv$workflow$state, child$level, child$cluster_id, tag,
          confidence = NA_real_,
          rationale = trimws(input$correction_rationale %||% ""),
          needs_review = TRUE, provider = "reviewer", model = "manual",
          embedding_model = if (isTRUE(rv$demo_mode)) "guided-demo-embedding" else NA_character_,
          tag_embedding = if (isTRUE(rv$demo_mode)) {
            .demo_embedding(tag, ncol(rv$workflow$state$embeddings))
          } else NULL
        )
        state$workflow$stage <- "review"
        rv$workflow$state <- novaTagger::tag_store_save(rv$store, state)
        shiny::showNotification(
          "Correction queued. Open Generate & review tags to accept or modify it.",
          type = "message", duration = 7
        )
        TRUE
      }, error = function(error) {
        shiny::showNotification(conditionMessage(error), type = "error", duration = NULL)
        FALSE
      })
      invisible(result)
    })
    output$parent_path <- shiny::renderText({
      parent <- selected_parent()
      .hierarchy_split_path(rv$workflow$state, parent$level, parent$cluster_id)
    })
    output$split_children <- DT::renderDT({
      parent <- selected_parent()
      DT::datatable(
        .hierarchy_split_children(rv$workflow$state, parent$level, parent$cluster_id),
        rownames = FALSE, options = list(dom = "t", ordering = FALSE, scrollX = TRUE)
      )
    })
  })
}
