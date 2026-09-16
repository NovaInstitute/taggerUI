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
        shiny::textOutput(ns("parent_path"))
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
    shiny::p(
      "This page is an inspection step. Rename, merge, collapse, and split actions "
        , "will be added as previewed changes in later increments."
    )
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
