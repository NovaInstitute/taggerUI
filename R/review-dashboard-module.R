# Reviewer-oriented overview of work awaiting attention in a prepared run.

.review_dashboard_issues <- function(workflow) {
  if (is.null(workflow) || is.null(workflow$state$clusters) ||
      !nrow(workflow$state$clusters)) {
    return(tibble::tibble(
      priority = character(), issue = character(), count = integer(),
      recommended_action = character()
    ))
  }
  state <- workflow$state
  quality <- .cluster_node_quality(state)
  clusters <- state$clusters
  status <- quality$review_status %||% character()
  tag <- trimws(as.character(quality$tag %||% character()))
  parent_tag <- vapply(seq_len(nrow(clusters)), function(index) {
    parent_id <- clusters$parent_cluster[[index]]
    if (is.na(parent_id)) return(NA_character_)
    parent <- clusters[
      clusters$level == clusters$level[[index]] + 1L &
        as.character(clusters$cluster_id) == as.character(parent_id), , drop = FALSE
    ]
    if (nrow(parent) != 1L) return(NA_character_)
    trimws(as.character(parent$tag[[1]] %||% ""))
  }, character(1))
  repeated_parent_child <- !is.na(parent_tag) & nzchar(parent_tag) &
    !is.na(tag) & nzchar(tag) & tolower(parent_tag) == tolower(tag)
  rows <- list(
    tibble::tibble(
      priority = "High", issue = "Clusters with question-fit warnings",
      count = sum(quality$flagged %||% FALSE),
      recommended_action = "Inspect the flagged cluster and its outlying questions."
    ),
    tibble::tibble(
      priority = "High", issue = "Untagged hierarchy nodes",
      count = sum(is.na(tag) | !nzchar(tag) | tag == "untagged"),
      recommended_action = "Review the node and give it a meaningful tag."
    ),
    tibble::tibble(
      priority = "Medium", issue = "Parent and child with the same tag",
      count = sum(repeated_parent_child),
      recommended_action = "Check whether the child needs a more specific tag or the level should be collapsed."
    ),
    tibble::tibble(
      priority = "Medium", issue = "Hierarchy nodes awaiting review",
      count = sum(status %in% c("pending", "proposed", "deferred")),
      recommended_action = "Review the split and confirm that its child groups make sense."
    )
  )
  result <- dplyr::bind_rows(rows)
  result[result$count > 0L, , drop = FALSE]
}

.review_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Review dashboard"),
    shiny::p(
      "Start with the highest-priority issue. The dashboard does not change the hierarchy."
    ),
    shiny::uiOutput(ns("next_action")),
    shiny::fluidRow(
      shiny::column(3, shiny::wellPanel(
        shiny::h4("Questions"), shiny::textOutput(ns("question_count"))
      )),
      shiny::column(3, shiny::wellPanel(
        shiny::h4("Hierarchy nodes"), shiny::textOutput(ns("cluster_count"))
      )),
      shiny::column(3, shiny::wellPanel(
        shiny::h4("Review decisions"), shiny::textOutput(ns("review_count"))
      )),
      shiny::column(3, shiny::wellPanel(
        shiny::h4("Queued questions"), shiny::textOutput(ns("queue_count"))
      ))
    ),
    shiny::h4("Work needing attention"),
    DT::DTOutput(ns("issues")),
    shiny::h4("Hierarchy at a glance"),
    DT::DTOutput(ns("level_summary")),
    shiny::h4("Question review queue"),
    shiny::p("These are questions whose cluster placement needs a human decision after taxonomy review."),
    DT::DTOutput(ns("question_queue"))
  )
}

.review_dashboard_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    issues <- shiny::reactive(.review_dashboard_issues(rv$workflow))
    output$next_action <- shiny::renderUI({
      data <- issues()
      if (!nrow(data)) {
        return(shiny::div(
          class = "alert alert-success",
          "No automated review issues are currently outstanding."
        ))
      }
      shiny::div(
        class = "alert alert-info",
        shiny::strong("Recommended next step: "),
        data$recommended_action[[1]]
      )
    })
    output$question_count <- shiny::renderText({
      if (is.null(rv$questions)) "No questions loaded" else nrow(rv$questions)
    })
    output$cluster_count <- shiny::renderText({
      clusters <- rv$workflow$state$clusters %||% NULL
      if (is.null(clusters)) "No hierarchy resumed" else nrow(clusters)
    })
    output$review_count <- shiny::renderText({
      if (is.null(rv$workflow)) "No run resumed" else
        length(rv$workflow$state$review_events %||% list())
    })
    output$queue_count <- shiny::renderText({
      queue <- rv$question_queue %||% data.frame()
      if (!nrow(queue)) "No queued questions" else nrow(queue)
    })
    output$issues <- DT::renderDT(DT::datatable(
      issues(), rownames = FALSE, options = list(dom = "t", ordering = FALSE)
    ))
    output$level_summary <- DT::renderDT({
      if (is.null(rv$workflow)) return(DT::datatable(data.frame()))
      data <- .tagging_level_progress(rv$workflow)
      DT::datatable(data, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
    })
    output$question_queue <- DT::renderDT({
      queue <- rv$question_queue %||% data.frame()
      DT::datatable(queue, rownames = FALSE, filter = "top",
                    options = list(pageLength = 10, dom = "tip", scrollX = TRUE))
    })
  })
}
