test_that("dashboard reports an empty state before a run is resumed", {
  issues <- taggerUI:::.review_dashboard_issues(NULL)
  expect_s3_class(issues, "tbl_df")
  expect_equal(nrow(issues), 0L)
})

test_that("dashboard UI presents a prioritised review queue", {
  html <- as.character(taggerUI:::.review_dashboard_ui("dashboard"))
  expected <- c(
    "dashboard-next_action", "dashboard-question_count",
    "dashboard-cluster_count", "dashboard-review_count", "dashboard-issues",
    "dashboard-level_summary"
  )
  expect_true(all(vapply(expected, grepl, logical(1), x = html, fixed = TRUE)))
  expect_match(html, "Work needing attention", fixed = TRUE)
})
