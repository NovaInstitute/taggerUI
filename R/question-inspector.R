# Presentation helpers for novaTagger's question projection.

.empty_question_summary <- function() {
  tibble::tibble(
    id = character(), caption = character(), question_class = character(),
    question_type = character(), response_cardinality = character(),
    response_datatype = character(), answer_count = integer(),
    option_preview = character(), source_form_id = character(),
    in_repeat_group = logical(), repeat_group_id = character(),
    source_element_name = character(), element_order = integer(),
    potential_duplicate = logical(), quality_warning = logical(),
    quality_issues = character()
  )
}

.validate_question_projection <- function(questions) {
  if (!is.data.frame(questions)) stop("`questions` must be a data frame.", call. = FALSE)
  required <- c(
    "id", "caption", "question_type", "question_class",
    "response_cardinality", "response_datatype", "source_element_name",
    "element_order", "repeat_group_id", "answer_options", "answer_count"
  )
  missing <- setdiff(required, names(questions))
  if (length(missing)) {
    stop("Question projection is missing: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.list(questions$answer_options)) stop("`answer_options` must be a list-column.", call. = FALSE)
  invisible(questions)
}

.option_preview <- function(options, limit = 4L) {
  if (!is.data.frame(options) || !nrow(options)) return("")
  labels <- as.character(options$option_text)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (!length(labels)) return("")
  shown <- utils::head(labels, limit)
  suffix <- if (length(labels) > limit) paste0(" … +", length(labels) - limit) else ""
  paste0(paste(shown, collapse = "; "), suffix)
}

.normalise_question_caption <- function(caption) {
  value <- tolower(trimws(as.character(caption)))
  value[is.na(value)] <- ""
  value <- gsub("[[:punct:]]+", " ", value)
  value <- gsub("[[:space:]]+", " ", value)
  trimws(value)
}

.question_source_forms <- function(questions) {
  if (!"source_form_id" %in% names(questions)) return(rep(NA_character_, nrow(questions)))
  as.character(questions$source_form_id)
}

.question_quality_issues <- function(questions) {
  .validate_question_projection(questions)
  if (!nrow(questions)) return(character())
  source_forms <- .question_source_forms(questions)
  captions <- trimws(as.character(questions$caption))
  normalised <- .normalise_question_caption(captions)
  duplicate <- nzchar(normalised) &
    (duplicated(normalised) | duplicated(normalised, fromLast = TRUE))
  vapply(seq_len(nrow(questions)), function(index) {
    issues <- character()
    if (is.na(captions[[index]]) || !nzchar(captions[[index]])) issues <- c(issues, "missing caption")
    if ("source_form_id" %in% names(questions) &&
        (is.na(source_forms[[index]]) || !nzchar(source_forms[[index]]))) {
      issues <- c(issues, "missing source form")
    }
    count <- as.integer(questions$answer_count[[index]])
    if (is.na(count)) count <- 0L
    if (identical(questions$question_class[[index]], "closed") && count == 0L) {
      issues <- c(issues, "closed question without options")
    }
    if (identical(questions$question_class[[index]], "open") && count > 0L) {
      issues <- c(issues, "open question with options")
    }
    if (duplicate[[index]]) issues <- c(issues, "repeated caption")
    paste(issues, collapse = "; ")
  }, character(1))
}

.question_summary <- function(questions, question_class = "all",
                              question_type = "all", source_form = "all",
                              repeat_membership = "all", duplicate_filter = "all",
                              warning_filter = "all", search = "",
                              repeat_only = FALSE) {
  .validate_question_projection(questions)
  if (!nrow(questions)) return(.empty_question_summary())
  keep <- rep(TRUE, nrow(questions))
  if (!identical(question_class, "all")) keep <- keep & questions$question_class == question_class
  if (!identical(question_type, "all")) keep <- keep & questions$question_type == question_type
  source_forms <- .question_source_forms(questions)
  if (!identical(source_form, "all")) {
    keep <- keep & !is.na(source_forms) & source_forms == source_form
  }
  repeat_group <- as.character(questions$repeat_group_id)
  in_repeat <- !is.na(repeat_group) & nzchar(repeat_group)
  if (isTRUE(repeat_only)) repeat_membership <- "in_repeat"
  if (identical(repeat_membership, "in_repeat")) keep <- keep & in_repeat
  if (identical(repeat_membership, "not_repeat")) keep <- keep & !in_repeat
  normalised <- .normalise_question_caption(questions$caption)
  duplicate <- nzchar(normalised) &
    (duplicated(normalised) | duplicated(normalised, fromLast = TRUE))
  if (identical(duplicate_filter, "duplicates")) keep <- keep & duplicate
  issues <- .question_quality_issues(questions)
  has_warning <- nzchar(issues)
  if (identical(warning_filter, "warnings")) keep <- keep & has_warning
  search <- trimws(as.character(search %||% ""))
  if (nzchar(search)) {
    haystack <- paste(
      questions$caption, questions$source_element_name, source_forms,
      vapply(questions$answer_options, .option_preview, character(1), limit = 20L)
    )
    keep <- keep & grepl(tolower(search), tolower(haystack), fixed = TRUE)
  }
  selected <- questions[keep, , drop = FALSE]
  if (!nrow(selected)) return(.empty_question_summary())
  selected_repeat <- as.character(selected$repeat_group_id)
  tibble::tibble(
    id = as.character(selected$id), caption = as.character(selected$caption),
    question_class = as.character(selected$question_class),
    question_type = as.character(selected$question_type),
    response_cardinality = as.character(selected$response_cardinality),
    response_datatype = as.character(selected$response_datatype),
    answer_count = as.integer(selected$answer_count),
    option_preview = vapply(selected$answer_options, .option_preview, character(1)),
    source_form_id = source_forms[keep],
    in_repeat_group = !is.na(selected_repeat) & nzchar(selected_repeat),
    repeat_group_id = selected_repeat,
    source_element_name = as.character(selected$source_element_name),
    element_order = as.integer(selected$element_order),
    potential_duplicate = duplicate[keep], quality_warning = has_warning[keep],
    quality_issues = issues[keep]
  )
}

.question_options <- function(questions, question_id) {
  .validate_question_projection(questions)
  hit <- match(as.character(question_id), as.character(questions$id))
  if (is.na(hit)) return(tibble::tibble(
    option_id = character(), option_text = character(),
    option_code = character(), option_order = integer()
  ))
  tibble::as_tibble(questions$answer_options[[hit]])
}

.question_record <- function(questions, question_id) {
  .validate_question_projection(questions)
  hit <- match(as.character(question_id), as.character(questions$id))
  if (is.na(hit)) return(NULL)
  row <- questions[hit, , drop = FALSE]
  record <- lapply(row, function(column) column[[1L]])
  names(record) <- names(row)
  record
}

.question_projection_counts <- function(questions) {
  .validate_question_projection(questions)
  repeat_group <- as.character(questions$repeat_group_id)
  source_forms <- .question_source_forms(questions)
  tibble::tibble(
    metric = c("questions", "open questions", "closed questions", "answer options",
               "questions in repeat groups", "source forms"),
    value = c(
      nrow(questions), sum(questions$question_class == "open", na.rm = TRUE),
      sum(questions$question_class == "closed", na.rm = TRUE),
      sum(as.integer(questions$answer_count), na.rm = TRUE),
      sum(!is.na(repeat_group) & nzchar(repeat_group)),
      length(unique(source_forms[!is.na(source_forms) & nzchar(source_forms)]))
    )
  )
}

.question_quality_summary <- function(questions) {
  .validate_question_projection(questions)
  issues <- .question_quality_issues(questions)
  labels <- c("questions with warnings", "missing captions", "missing source forms",
              "closed questions without options", "open questions with options",
              "questions sharing a caption")
  patterns <- c("", "missing caption", "missing source form",
                "closed question without options", "open question with options",
                "repeated caption")
  values <- vapply(patterns, function(pattern) {
    if (!nzchar(pattern)) sum(nzchar(issues)) else sum(grepl(pattern, issues, fixed = TRUE))
  }, integer(1))
  tibble::tibble(check = labels, question_count = unname(values))
}
