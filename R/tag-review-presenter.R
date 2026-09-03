# Reviewer-facing projections of novaTagger proposal state.

.empty_proposal_table <- function() {
  tibble::tibble(
    proposal_id = character(), level = integer(), cluster_id = character(),
    tag = character(), confidence = numeric(), needs_review = logical(),
    status = character(), provider = character(), model = character(),
    embedding_model = character(), embedding_dimension = integer(),
    created_at = as.POSIXct(character())
  )
}

.proposal_table <- function(state) {
  proposals <- state$proposals %||% list()
  if (!length(proposals)) return(.empty_proposal_table())
  rows <- lapply(proposals, function(proposal) tibble::tibble(
    proposal_id = as.character(proposal$proposal_id),
    level = as.integer(proposal$level),
    cluster_id = as.character(proposal$cluster_id),
    tag = as.character(proposal$tag),
    confidence = as.numeric(proposal$confidence),
    needs_review = isTRUE(proposal$needs_review),
    status = as.character(proposal$status),
    provider = as.character(proposal$provider),
    model = as.character(proposal$model),
    embedding_model = as.character(proposal$embedding_model),
    embedding_dimension = length(proposal$tag_embedding %||% numeric()),
    created_at = as.POSIXct(proposal$created_at)
  ))
  result <- dplyr::bind_rows(rows)
  result[order(result$created_at, result$proposal_id, na.last = TRUE), , drop = FALSE]
}

.proposal_choices <- function(state) {
  rows <- .proposal_table(state)
  if (!nrow(rows)) return(stats::setNames(character(), character()))
  labels <- paste0(
    "L", rows$level, " / C", rows$cluster_id, " — ", rows$tag,
    " [", rows$status, "]"
  )
  stats::setNames(rows$proposal_id, labels)
}

.selected_proposal <- function(state, proposal_id) {
  if (is.null(state) || !nzchar(as.character(proposal_id %||% ""))) return(NULL)
  state$proposals[[as.character(proposal_id)]] %||% NULL
}

.safe_proposal_detail <- function(proposal) {
  if (is.null(proposal)) return(NULL)
  list(
    proposal_id = proposal$proposal_id,
    level = proposal$level,
    cluster_id = proposal$cluster_id,
    tag = proposal$tag,
    confidence = proposal$confidence,
    rationale = proposal$rationale,
    needs_review = proposal$needs_review,
    status = proposal$status,
    provider = proposal$provider,
    generation_model = proposal$model,
    embedding_model = proposal$embedding_model,
    embedding_dimension = length(proposal$tag_embedding %||% numeric()),
    created_at = proposal$created_at
  )
}

.proposal_question_scores <- function(state, proposal) {
  if (is.null(proposal)) return(tibble::tibble())
  if (is.null(proposal$tag_embedding)) {
    profile <- novaTagger::get_cluster_profile(
      state, proposal$cluster_id, proposal$level, sample_size = 1000L
    )
    questions <- unique(dplyr::bind_rows(
      profile$representative_questions, profile$outlier_questions
    ))
    if (!nrow(questions)) return(tibble::tibble())
    return(tibble::tibble(
      question_id = questions$question_id,
      question = questions$question_text,
      cosine_similarity = NA_real_, cosine_distance = NA_real_,
      low_similarity = NA
    ))
  }
  novaTagger::score_cluster_tag_similarity(
    state, proposal$level, proposal$cluster_id, proposal$tag_embedding
  )
}

.similarity_summary <- function(scores) {
  if (!is.data.frame(scores) || !nrow(scores) ||
      !"cosine_similarity" %in% names(scores) ||
      all(is.na(scores$cosine_similarity))) {
    return(tibble::tibble(
      metric = c("questions", "mean similarity", "minimum similarity", "low similarity"),
      value = c(if (is.data.frame(scores)) nrow(scores) else 0L, NA, NA, NA)
    ))
  }
  tibble::tibble(
    metric = c("questions", "mean similarity", "minimum similarity", "low similarity"),
    value = c(
      nrow(scores), mean(scores$cosine_similarity, na.rm = TRUE),
      min(scores$cosine_similarity, na.rm = TRUE),
      sum(scores$low_similarity, na.rm = TRUE)
    )
  )
}

.proposal_review_events <- function(state, proposal_id) {
  events <- Filter(function(event) {
    identical(as.character(event$proposal_id), as.character(proposal_id))
  }, state$review_events %||% list())
  if (!length(events)) return(tibble::tibble(
    decision = character(), reviewer_id = character(), rationale = character(),
    original_tag = character(), resulting_tag = character(),
    before_mean = numeric(), after_mean = numeric(), mean_change = numeric(),
    created_at = as.POSIXct(character())
  ))
  dplyr::bind_rows(lapply(events, function(event) {
    similarity <- event$similarity %||% list()
    tibble::tibble(
      decision = as.character(event$decision),
      reviewer_id = as.character(event$reviewer_id),
      rationale = as.character(event$rationale),
      original_tag = as.character(event$original_tag),
      resulting_tag = as.character(event$resulting_tag),
      before_mean = as.numeric(similarity$before$mean %||% NA_real_),
      after_mean = as.numeric(similarity$after$mean %||% NA_real_),
      mean_change = as.numeric(similarity$mean_change %||% NA_real_),
      created_at = as.POSIXct(event$created_at)
    )
  }))
}

.next_cluster_summary <- function(workflow, sample_size = 8L) {
  if (is.null(workflow)) return(list(cluster = tibble::tibble(), questions = tibble::tibble()))
  target <- novaTagger::workflow_next_cluster(workflow)
  if (!nrow(target)) return(list(cluster = target, questions = tibble::tibble()))
  profile <- novaTagger::get_cluster_profile(
    workflow$state, target$cluster_id[[1]], target$level[[1]], sample_size
  )
  questions <- dplyr::bind_rows(
    dplyr::mutate(profile$representative_questions, sample_role = "representative"),
    dplyr::mutate(profile$outlier_questions, sample_role = "outlier")
  )
  questions <- questions[!duplicated(questions$question_id), , drop = FALSE]
  list(
    cluster = tibble::tibble(
      level = as.integer(target$level[[1]]),
      cluster_id = as.character(target$cluster_id[[1]]),
      question_count = length(target$question_ids[[1]]),
      current_tag = as.character(target$tag[[1]])
    ),
    questions = questions
  )
}
