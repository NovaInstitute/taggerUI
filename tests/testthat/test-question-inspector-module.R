test_that("question inspector exposes filters and read-only inspection outputs", {
  html <- as.character(taggerUI:::.question_inspector_ui("inspector"))
  expected <- c(
    "inspector-question_class", "inspector-question_type", "inspector-source_form",
    "inspector-repeat_membership", "inspector-duplicate_filter",
    "inspector-warning_filter", "inspector-search", "inspector-question_table",
    "inspector-quality_summary", "inspector-question_detail",
    "inspector-option_table", "inspector-question_record"
  )
  expect_true(all(vapply(expected, grepl, logical(1), x = html, fixed = TRUE)))
  expect_match(html, "This page is read-only", fixed = TRUE)
})

test_that("question inspector exposes all corpus summary cards", {
  html <- as.character(taggerUI:::.question_inspector_ui("inspector"))
  expected <- c("Questions", "Open", "Closed", "Options", "Repeat groups", "Source forms")
  expect_true(all(vapply(expected, grepl, logical(1), x = html, fixed = TRUE)))
})
