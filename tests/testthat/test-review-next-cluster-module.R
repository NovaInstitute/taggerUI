test_that("review module exposes the minimal tagging workflow", {
  html <- as.character(taggerUI:::.review_next_cluster_ui("review"))
  expected <- c(
    "review-generate_proposal", "review-edited_tag", "review-reviewer_id",
    "review-accept_proposal", "review-edit_proposal", "review-reject_proposal",
    "review-defer_proposal", "review-next_cluster", "review-proposal_detail",
    "review-review_context_heading", "review-question_table_heading",
    "review-level_progress_bars",
    "review-cluster_overview"
  )
  expect_true(all(vapply(expected, grepl, logical(1), x = html, fixed = TRUE)))
})

test_that("review module explains its lightweight progress view", {
  html <- as.character(taggerUI:::.review_next_cluster_ui("review"))
  expect_match(html, "does not calculate PCA or cluster diagnostics", fixed = TRUE)
})
