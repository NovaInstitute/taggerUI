# UI projections for inspecting cluster, tag, and hierarchy quality.

.quality_cosine <- function(matrix, reference) {
  matrix <- as.matrix(matrix)
  reference <- as.numeric(reference)
  denominator <- sqrt(rowSums(matrix^2)) * sqrt(sum(reference^2))
  ifelse(denominator == 0, NA_real_, as.numeric(matrix %*% reference) / denominator)
}

.quality_assignment_column <- function(level) paste0("cluster_level_", as.integer(level))

.quality_cluster_row <- function(state, level, cluster_id) {
  rows <- state$clusters[
    state$clusters$level == as.integer(level) &
      as.character(state$clusters$cluster_id) == as.character(cluster_id),
    , drop = FALSE
  ]
  if (nrow(rows) != 1L) {
    stop("Cluster selection must identify exactly one cluster.", call. = FALSE)
  }
  rows
}

.quality_latest_proposal <- function(state, level, cluster_id) {
  proposals <- Filter(function(proposal) {
    identical(as.integer(proposal$level), as.integer(level)) &&
      identical(as.character(proposal$cluster_id), as.character(cluster_id))
  }, state$proposals %||% list())
  if (!length(proposals)) return(NULL)
  created <- vapply(proposals, function(proposal) {
    as.numeric(as.POSIXct(proposal$created_at %||% NA))
  }, numeric(1))
  if (all(is.na(created))) return(proposals[[length(proposals)]])
  proposals[[which.max(replace(created, is.na(created), -Inf))]]
}

.cluster_quality_questions <- function(state, level, cluster_id,
                                       outlier_similarity = 0.5,
                                       placement_margin = 0.1) {
  state <- novaTagger::validate_tag_state(state)
  level <- as.integer(level)
  cluster_id <- as.character(cluster_id)
  column <- .quality_assignment_column(level)
  if (!column %in% names(state$assignments)) {
    stop("Hierarchy assignments do not contain level ", level, ".", call. = FALSE)
  }
  embeddings <- as.matrix(state$embeddings)
  current_ids <- as.character(state$assignments[[column]])
  rows <- which(current_ids == cluster_id)
  if (!length(rows)) stop("Selected cluster has no questions.", call. = FALSE)

  level_clusters <- unique(current_ids[!is.na(current_ids) & nzchar(current_ids)])
  centroids <- lapply(level_clusters, function(id) {
    colMeans(embeddings[current_ids == id, , drop = FALSE])
  })
  names(centroids) <- level_clusters
  similarity_matrix <- vapply(centroids, function(centroid) {
    .quality_cosine(embeddings[rows, , drop = FALSE], centroid)
  }, numeric(length(rows)))
  if (is.null(dim(similarity_matrix))) {
    similarity_matrix <- matrix(similarity_matrix, ncol = 1L)
  }
  colnames(similarity_matrix) <- level_clusters
  current_column <- match(cluster_id, level_clusters)
  centroid_similarity <- similarity_matrix[, current_column]
  alternatives <- similarity_matrix
  alternatives[, current_column] <- -Inf
  if (length(level_clusters) > 1L) {
    best_column <- max.col(alternatives, ties.method = "first")
    best_alternative <- level_clusters[best_column]
    best_similarity <- alternatives[cbind(seq_along(rows), best_column)]
  } else {
    best_alternative <- rep(NA_character_, length(rows))
    best_similarity <- rep(NA_real_, length(rows))
  }

  proposal <- .quality_latest_proposal(state, level, cluster_id)
  tag_similarity <- rep(NA_real_, length(rows))
  if (!is.null(proposal) && !is.null(proposal$tag_embedding)) {
    tag_similarity <- .quality_cosine(
      embeddings[rows, , drop = FALSE], proposal$tag_embedding
    )
  }
  question_class <- if ("question_class" %in% names(state$questions)) {
    as.character(state$questions$question_class[rows])
  } else rep(NA_character_, length(rows))
  result <- tibble::tibble(
    question_id = as.character(state$questions$id[rows]),
    question = as.character(state$questions$caption[rows]),
    question_class = question_class,
    centroid_similarity = centroid_similarity,
    tag_similarity = tag_similarity,
    best_alternative_cluster = best_alternative,
    best_alternative_similarity = best_similarity,
    placement_margin = best_similarity - centroid_similarity,
    centroid_outlier = is.na(centroid_similarity) |
      centroid_similarity < outlier_similarity,
    outside_tag_scope = !is.na(tag_similarity) & tag_similarity < outlier_similarity,
    alternative_better = !is.na(best_similarity) &
      best_similarity - centroid_similarity >= placement_margin
  )
  concern <- pmin(result$centroid_similarity, result$tag_similarity, na.rm = TRUE)
  concern[!is.finite(concern)] <- result$centroid_similarity[!is.finite(concern)]
  result[order(concern, -result$placement_margin, na.last = TRUE), , drop = FALSE]
}

.cluster_quality_summary <- function(state, level, cluster_id) {
  questions <- .cluster_quality_questions(state, level, cluster_id)
  proposal <- .quality_latest_proposal(state, level, cluster_id)
  row <- .quality_cluster_row(state, level, cluster_id)
  tibble::tibble(
    level = as.integer(level),
    cluster_id = as.character(cluster_id),
    tag = if (is.null(proposal)) as.character(row$tag[[1]]) else proposal$tag,
    review_status = if (is.null(proposal)) "pending" else proposal$status,
    question_count = nrow(questions),
    mean_centroid_similarity = mean(questions$centroid_similarity, na.rm = TRUE),
    minimum_centroid_similarity = min(questions$centroid_similarity, na.rm = TRUE),
    centroid_outliers = sum(questions$centroid_outlier),
    mean_tag_similarity = if (all(is.na(questions$tag_similarity))) NA_real_ else
      mean(questions$tag_similarity, na.rm = TRUE),
    minimum_tag_similarity = if (all(is.na(questions$tag_similarity))) NA_real_ else
      min(questions$tag_similarity, na.rm = TRUE),
    outside_tag_scope = sum(questions$outside_tag_scope),
    alternative_better = sum(questions$alternative_better),
    flagged = any(questions$centroid_outlier | questions$outside_tag_scope |
                    questions$alternative_better)
  )
}

.cluster_hierarchy_context <- function(state, level, cluster_id) {
  selected <- .quality_cluster_row(state, level, cluster_id)
  parent <- if (is.na(selected$parent_cluster[[1]])) {
    state$clusters[0, , drop = FALSE]
  } else state$clusters[
    state$clusters$level == as.integer(level) + 1L &
      as.character(state$clusters$cluster_id) ==
        as.character(selected$parent_cluster[[1]]), , drop = FALSE
  ]
  children <- state$clusters[
    state$clusters$level == as.integer(level) - 1L &
      as.character(state$clusters$parent_cluster) == as.character(cluster_id),
    , drop = FALSE
  ]
  siblings <- if (!nrow(parent)) state$clusters[0, , drop = FALSE] else
    state$clusters[
      state$clusters$level == as.integer(level) &
        as.character(state$clusters$parent_cluster) ==
          as.character(selected$parent_cluster[[1]]) &
        as.character(state$clusters$cluster_id) != as.character(cluster_id),
      , drop = FALSE
    ]
  present <- function(rows, relationship) {
    if (!nrow(rows)) return(tibble::tibble())
    tibble::tibble(
      relationship = relationship,
      level = as.integer(rows$level),
      cluster_id = as.character(rows$cluster_id),
      tag = as.character(rows$tag),
      question_count = lengths(rows$question_ids),
      review_status = vapply(seq_len(nrow(rows)), function(i) {
        proposal <- .quality_latest_proposal(
          state, rows$level[[i]], rows$cluster_id[[i]]
        )
        if (is.null(proposal)) "pending" else proposal$status
      }, character(1))
    )
  }
  dplyr::bind_rows(
    present(parent, "parent"), present(selected, "selected"),
    present(children, "child"), present(siblings, "sibling")
  )
}

.cluster_node_quality <- function(state) {
  if (is.null(state$clusters) || !nrow(state$clusters)) return(tibble::tibble())
  dplyr::bind_rows(lapply(seq_len(nrow(state$clusters)), function(i) {
    row <- state$clusters[i, , drop = FALSE]
    summary <- .cluster_quality_summary(state, row$level[[1]], row$cluster_id[[1]])
    tibble::tibble(
      key = paste(row$level[[1]], row$cluster_id[[1]], sep = ":"),
      level = as.integer(row$level[[1]]),
      cluster_id = as.character(row$cluster_id[[1]]),
      review_status = summary$review_status,
      flagged = summary$flagged,
      warning_count = summary$centroid_outliers + summary$outside_tag_scope +
        summary$alternative_better
    )
  }))
}

.cluster_choices <- function(state) {
  quality <- .cluster_node_quality(state)
  if (!nrow(quality)) return(stats::setNames(character(), character()))
  labels <- paste0(
    "L", quality$level, " / C", quality$cluster_id,
    " [", quality$review_status, "]",
    ifelse(quality$flagged, paste0(" ⚠ ", quality$warning_count), "")
  )
  stats::setNames(quality$key, labels)
}

.parse_cluster_key <- function(key) {
  parts <- strsplit(as.character(key), ":", fixed = TRUE)[[1]]
  if (length(parts) != 2L || is.na(suppressWarnings(as.integer(parts[[1]]))) ||
      !nzchar(parts[[2]])) return(NULL)
  list(level = as.integer(parts[[1]]), cluster_id = parts[[2]])
}

.cluster_pca_projection <- function(state, level, cluster_id) {
  projection <- novaTagger::question_projection_2d(state)
  column <- .quality_assignment_column(level)
  assigned <- as.character(state$assignments[[column]][
    match(projection$question_id, state$assignments$id)
  ])
  projection$selected <- assigned == as.character(cluster_id)
  projection$selected_cluster <- as.character(cluster_id)
  projection
}
