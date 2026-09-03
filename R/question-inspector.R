# Presentation helpers for novaTagger's question projection.

.empty_question_summary <- function() {
  tibble::tibble(
    id = character(), caption = character(), question_class = character(),
    question_type = character(), response_cardinality = character(),
    response_datatype = character(), answer_count = integer(),
    option_preview = character(), in_repeat_group = logical(),
    repeat_group_id = character(), source_element_name = character(),
    element_order = integer()
  )
}

.validate_question_projection <- function(questions) {
  if (!is.data.frame(questions)) {
    stop("`questions` must be a data frame.", call. = FALSE)
  }
  required <- c(
    "id", "caption", "question_type", "question_class",
    "response_cardinality", "response_datatype", "source_element_name",
    "element_order", "repeat_group_id", "answer_options", "answer_count"
  )
  missing <- setdiff(required, names(questions))
  if (length(missing)) {
    stop(
      "Question projection is missing: ", paste(missing, collapse = ", "),
      ".", call. = FALSE
    )
  }
  if (!is.list(questions$answer_options)) {
    stop("`answer_options` must be a list-column.", call. = FALSE)
  }
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

.question_summary <- function(questions, question_class = "all",
                              repeat_only = FALSE, search = "") {
  .validate_question_projection(questions)
  if (!nrow(questions)) return(.empty_question_summary())
  keep <- rep(TRUE, nrow(questions))
  if (!identical(question_class, "all")) {
    keep <- keep & questions$question_class == question_class
  }
  repeat_group <- as.character(questions$repeat_group_id)
  in_repeat <- !is.na(repeat_group) & nzchar(repeat_group)
  if (isTRUE(repeat_only)) keep <- keep & in_repeat
  search <- trimws(as.character(search %||% ""))
  if (nzchar(search)) {
    haystack <- paste(
      questions$caption, questions$source_element_name,
      vapply(questions$answer_options, .option_preview, character(1), limit = 20L)
    )
    keep <- keep & grepl(tolower(search), tolower(haystack), fixed = TRUE)
  }
  selected <- questions[keep, , drop = FALSE]
  if (!nrow(selected)) return(.empty_question_summary())
  selected_repeat <- as.character(selected$repeat_group_id)
  tibble::tibble(
    id = as.character(selected$id),
    caption = as.character(selected$caption),
    question_class = as.character(selected$question_class),
    question_type = as.character(selected$question_type),
    response_cardinality = as.character(selected$response_cardinality),
    response_datatype = as.character(selected$response_datatype),
    answer_count = as.integer(selected$answer_count),
    option_preview = vapply(selected$answer_options, .option_preview, character(1)),
    in_repeat_group = !is.na(selected_repeat) & nzchar(selected_repeat),
    repeat_group_id = selected_repeat,
    source_element_name = as.character(selected$source_element_name),
    element_order = as.integer(selected$element_order)
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
  tibble::tibble(
    metric = c(
      "questions", "open questions", "closed questions",
      "answer options", "questions in repeat groups"
    ),
    value = c(
      nrow(questions),
      sum(questions$question_class == "open"),
      sum(questions$question_class == "closed"),
      sum(as.integer(questions$answer_count)),
      sum(!is.na(repeat_group) & nzchar(repeat_group))
    )
  )
}
