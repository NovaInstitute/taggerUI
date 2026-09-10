question_fixture <- function() {
  empty_options <- tibble::tibble(
    option_id = character(), option_text = character(),
    option_code = character(), option_order = integer()
  )
  closed_options <- tibble::tibble(
    option_id = c("a1", "a2"), option_text = c("Yes", "No"),
    option_code = c("1", "0"), option_order = 1:2
  )
  tibble::tibble(
    id = c("q1", "q2"), iri = c("q1", "q2"),
    caption = c("Describe your home", "Do you use gas?"),
    question_type = c("OpenQuestion", "ClosedQuestion"),
    question_class = c("open", "closed"),
    response_cardinality = c("single", "single"),
    response_datatype = c("string", "code"),
    procedure_id = "procedure", source_form_id = "form",
    source_element_name = c("home_description", "gas_use"),
    element_order = 1:2,
    repeat_group_id = c(NA_character_, "household_members"),
    answer_options = list(empty_options, closed_options),
    answer_count = c(0L, 2L)
  )
}

test_that("question summary distinguishes open, closed, and repeat questions", {
  questions <- question_fixture()
  summary <- taggerUI:::.question_summary(questions)
  expect_equal(summary$question_class, c("open", "closed"))
  expect_equal(summary$answer_count, c(0L, 2L))
  expect_equal(summary$in_repeat_group, c(FALSE, TRUE))

  closed <- taggerUI:::.question_summary(questions, "closed")
  expect_equal(closed$id, "q2")
  expect_match(closed$option_preview, "Yes; No", fixed = TRUE)

  repeated <- taggerUI:::.question_summary(questions, repeat_only = TRUE)
  expect_equal(repeated$id, "q2")
})

test_that("answer options remain nested under their owning question", {
  questions <- question_fixture()
  expect_equal(nrow(taggerUI:::.question_options(questions, "q1")), 0L)
  options <- taggerUI:::.question_options(questions, "q2")
  expect_equal(options$option_text, c("Yes", "No"))
  expect_false(any(options$option_id %in% questions$id))

  record <- taggerUI:::.question_record(questions, "q2")
  expect_equal(record$question_class, "closed")
  expect_s3_class(record$answer_options, "data.frame")
  expect_equal(nrow(record$answer_options), 2L)
})

test_that("question search includes captions, fields, and nested options", {
  questions <- question_fixture()
  expect_equal(taggerUI:::.question_summary(questions, search = "describe")$id, "q1")
  expect_equal(taggerUI:::.question_summary(questions, search = "gas_use")$id, "q2")
  expect_equal(taggerUI:::.question_summary(questions, search = "yes")$id, "q2")
  expect_equal(nrow(taggerUI:::.question_summary(questions, search = "missing")), 0L)
})

test_that("projection counts expose question and option semantics", {
  counts <- taggerUI:::.question_projection_counts(question_fixture())
  expect_equal(counts$value, c(2L, 1L, 1L, 2L, 1L))
})
