.hierarchy_quality_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Hierarchy progress and cluster quality"),
    shiny::p(
      "Node size represents question count. Node fill shows review status and a red ",
      "border marks a quality warning. Select a node to inspect that cluster."
    ),
    shiny::fluidRow(
      shiny::column(9, visNetwork::visNetworkOutput(ns("hierarchy"), height = "560px")),
      shiny::column(
        3, shiny::h4("Run summary"), DT::DTOutput(ns("hierarchy_summary")),
        shiny::h4("Select cluster"),
        shiny::selectizeInput(ns("quality_cluster"), "Cluster", choices = character(),
                              options = list(maxOptions = 2000)),
        shiny::p("Cluster diagnostics use the original embedding dimensions.")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        4, shiny::h4("Selected-cluster quality"),
        DT::DTOutput(ns("cluster_quality_summary")),
        shiny::h4("Parent, children, and siblings"), DT::DTOutput(ns("cluster_context"))
      ),
      shiny::column(
        8, shiny::h4("2D navigation view"),
        shiny::p(
          "PCA compresses the embeddings into two dimensions and may distort distances. ",
          "It is a navigation aid; flags and placement recommendations below use the full vectors."
        ),
        shiny::plotOutput(ns("cluster_pca"), height = "450px")
      )
    ),
    shiny::h4("Questions in the selected cluster"),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(
          ns("question_concern"), "Show questions",
          choices = c(
            "All, weakest first" = "all", "Centroid outliers" = "centroid_outlier",
            "Outside proposed tag scope" = "outside_tag_scope",
            "Better alternative cluster" = "alternative_better",
            "Representative questions" = "representative"
          )
        )
      ),
      shiny::column(
        8,
        shiny::p(
          "Centroid similarity measures fit with the current cluster. Tag similarity ",
          "measures fit with the proposed label. A positive placement margin means ",
          "another cluster centroid is closer."
        )
      )
    ),
    DT::DTOutput(ns("cluster_quality_questions")),
    shiny::h4("All cluster quality flags"),
    shiny::p("Use this overview to find clusters that warrant closer inspection."),
    DT::DTOutput(ns("all_cluster_quality"))
  )
}

.hierarchy_quality_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    quality_overview <- shiny::reactive({
      if (is.null(rv$workflow)) return(tibble::tibble())
      .cluster_node_quality(rv$workflow$state)
    })
    pca_projection <- shiny::reactive({
      if (is.null(rv$workflow)) return(tibble::tibble())
      novaTagger::question_projection_2d(rv$workflow$state)
    })

    shiny::observe({
      if (is.null(rv$workflow) || is.null(rv$workflow$state$clusters)) {
        shiny::updateSelectizeInput(session, "quality_cluster", choices = character(), server = TRUE)
        return()
      }
      choices <- .cluster_choices(rv$workflow$state, quality_overview())
      current <- shiny::isolate(input$quality_cluster %||% "")
      selected <- if (current %in% unname(choices)) current else
        if (length(choices)) unname(choices)[[1]] else character()
      shiny::updateSelectizeInput(session, "quality_cluster", choices = choices,
                                  selected = selected, server = TRUE)
    })

    shiny::observeEvent(input$hierarchy_node_selected, {
      if (is.null(rv$workflow)) return()
      node <- as.character(input$hierarchy_node_selected %||% "")
      matched <- regexec("^L([0-9]+)C(.+)$", node)
      parts <- regmatches(node, matched)[[1]]
      if (length(parts) != 3L) return()
      key <- paste(parts[[2]], parts[[3]], sep = ":")
      choices <- .cluster_choices(rv$workflow$state, quality_overview())
      if (key %in% unname(choices)) {
        shiny::updateSelectizeInput(session, "quality_cluster", selected = key)
      }
    }, ignoreInit = TRUE)

    selected_cluster <- shiny::reactive({
      shiny::req(rv$workflow, nzchar(input$quality_cluster %||% ""))
      cluster <- .parse_cluster_key(input$quality_cluster)
      shiny::req(!is.null(cluster))
      cluster
    })
    selected_questions <- shiny::reactive({
      cluster <- selected_cluster()
      .cluster_quality_questions(rv$workflow$state, cluster$level, cluster$cluster_id)
    })

    output$hierarchy_summary <- DT::renderDT({
      DT::datatable(.hierarchy_run_summary(rv$workflow), rownames = FALSE,
                    options = list(dom = "t", ordering = FALSE))
    })
    output$hierarchy <- visNetwork::renderVisNetwork({
      graph <- .tagger_hierarchy_graph(rv$workflow, quality_overview())
      if (!nrow(graph$nodes)) {
        return(visNetwork::visNetwork(
          data.frame(id = "empty", label = "Resume a clustered run"), data.frame()
        ))
      }
      selection_input <- ns("hierarchy_node_selected")
      visNetwork::visNetwork(graph$nodes, graph$edges, width = "100%") |>
        visNetwork::visGroups(groupname = "pending", color = list(background = "#d9d9d9")) |>
        visNetwork::visGroups(groupname = "proposed", color = list(background = "#f4c95d")) |>
        visNetwork::visGroups(groupname = "deferred", color = list(background = "#8ecae6")) |>
        visNetwork::visGroups(groupname = "accepted", color = list(background = "#70c1b3")) |>
        visNetwork::visGroups(groupname = "edited", color = list(background = "#70c1b3")) |>
        visNetwork::visGroups(groupname = "rejected", color = list(background = "#ef767a")) |>
        visNetwork::visNodes(scaling = list(min = 10, max = 45)) |>
        visNetwork::visHierarchicalLayout(direction = "DU", sortMethod = "directed") |>
        visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
        visNetwork::visEvents(selectNode = paste0(
          "function(properties) { Shiny.setInputValue('", selection_input,
          "', properties.nodes[0], {priority: 'event'}); }"
        ))
    })
    output$cluster_quality_summary <- DT::renderDT({
      cluster <- selected_cluster()
      data <- .cluster_quality_summary(rv$workflow$state, cluster$level, cluster$cluster_id)
      widget <- DT::datatable(data, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
      numeric <- intersect(c("mean_centroid_similarity", "minimum_centroid_similarity",
                             "mean_tag_similarity", "minimum_tag_similarity"), names(data))
      if (length(numeric)) DT::formatRound(widget, numeric, digits = 4) else widget
    })
    output$cluster_context <- DT::renderDT({
      cluster <- selected_cluster()
      DT::datatable(.cluster_hierarchy_context(rv$workflow$state, cluster$level,
                                                cluster$cluster_id),
                    rownames = FALSE, options = list(pageLength = 10, dom = "tip", scrollX = TRUE))
    })
    output$cluster_quality_questions <- DT::renderDT({
      data <- .filter_cluster_quality_questions(
        selected_questions(), input$question_concern %||% "all"
      )
      widget <- DT::datatable(data, rownames = FALSE, filter = "top",
                              options = list(pageLength = 20, scrollX = TRUE))
      numeric <- intersect(c("centroid_similarity", "tag_similarity",
                             "best_alternative_similarity", "placement_margin"), names(data))
      if (length(numeric)) DT::formatRound(widget, numeric, digits = 4) else widget
    })
    output$all_cluster_quality <- DT::renderDT({
      DT::datatable(quality_overview(), rownames = FALSE, filter = "top",
                    options = list(pageLength = 15, order = list(list(4, "desc"))))
    })
    output$cluster_pca <- shiny::renderPlot({
      cluster <- selected_cluster()
      data <- .cluster_pca_projection(
        rv$workflow$state, cluster$level, cluster$cluster_id, pca_projection()
      )
      graphics::plot(
        data$x, data$y, col = ifelse(data$selected, "#d1495b", "#c7c7c7"),
        pch = ifelse(data$selected, 19, 16), xlab = "PCA dimension 1",
        ylab = "PCA dimension 2",
        main = paste("Level", cluster$level, "cluster", cluster$cluster_id)
      )
    })
    invisible(list(selected_cluster = selected_cluster, selected_questions = selected_questions))
  })
}
