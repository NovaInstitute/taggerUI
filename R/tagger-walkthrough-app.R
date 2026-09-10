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

.tagger_hierarchy_graph <- function(workflow, quality = NULL) {
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
    value = pmax(1L, lengths(clusters$question_ids)),
    title = paste0(
      "Level ", clusters$level, "; cluster ", clusters$cluster_id,
      "; questions ", lengths(clusters$question_ids), "; status ", status
    ),
    stringsAsFactors = FALSE
  )
  if (is.null(quality)) quality <- .cluster_node_quality(workflow$state)
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
      .connect_resume_ui("connect_resume")
    ),
    shiny::tabPanel(
      "2. Inspect questions",
      .question_inspector_ui("question_inspector")
    ),
    shiny::tabPanel(
      "3. Generate & review tags",
      .review_next_cluster_ui("review_next")
    )
  )
}

.walkthrough_app_server <- function(input, output, session) {
  rv <- shiny::reactiveValues(
    questions = NULL, workflow = NULL, store = NULL,
    connection_message = "Not connected",
    model_message = "No model request in this session",
    connected = FALSE, load_message = "No data loaded in this session"
  )

  .connect_resume_server("connect_resume", rv)
  .question_inspector_server("question_inspector", rv)
  .review_next_cluster_server("review_next", rv)
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
