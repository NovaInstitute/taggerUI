test_that("hierarchy quality module exposes linked inspection controls", {
  html <- as.character(taggerUI:::.hierarchy_quality_ui("quality"))
  expected <- c(
    "quality-hierarchy", "quality-hierarchy_summary", "quality-quality_cluster",
    "quality-cluster_quality_summary", "quality-cluster_context",
    "quality-cluster_pca", "quality-question_concern",
    "quality-cluster_quality_questions", "quality-all_cluster_quality"
  )
  expect_true(all(vapply(expected, grepl, logical(1), x = html, fixed = TRUE)))
})

test_that("hierarchy page distinguishes PCA navigation from quality calculations", {
  html <- as.character(taggerUI:::.hierarchy_quality_ui("quality"))
  expect_match(html, "PCA compresses", fixed = TRUE)
  expect_match(html, "original embedding dimensions", fixed = TRUE)
  expect_match(html, "Better alternative cluster", fixed = TRUE)
})
