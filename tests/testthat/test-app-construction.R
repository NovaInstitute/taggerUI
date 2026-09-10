test_that("walkthrough UI constructs", {
  app <- tagger_app()
  expect_s3_class(app, "shiny.appobj")
  expect_true(is.function(app$serverFuncSource()))
})

test_that("graph defaults reproduce the integration named-graph layout", {
  base <- taggerUI:::.tagger_default_graph_base("demo-ledger", "demo-run")
  expect_equal(
    base,
    "https://data.nova.org/integration/demo-ledger/graph/tagging/openai/demo-run/"
  )
  expect_equal(
    names(taggerUI:::.tagger_graphs(base)),
    c("run", "embedding", "hierarchy", "review")
  )
})

test_that("review workflow controls are present in the UI", {
  html <- as.character(taggerUI:::.walkthrough_app_ui())
  expected <- c(
    "generate_proposal", "proposal_id", "edited_tag", "reviewer_id",
    "accept_proposal", "edit_proposal", "reject_proposal", "defer_proposal"
  )
  expect_true(all(vapply(expected, grepl, logical(1), x = html, fixed = TRUE)))
})

test_that("linked hierarchy quality controls are present in the UI", {
  html <- as.character(taggerUI:::.walkthrough_app_ui())
  expected <- c(
    "quality_cluster", "cluster_pca", "cluster_quality_summary",
    "cluster_context", "cluster_quality_questions", "all_cluster_quality"
  )
  expect_true(all(vapply(expected, grepl, logical(1), x = html, fixed = TRUE)))
})

test_that("visNetwork groups use the installed groupname API", {
  graph <- visNetwork::visNetwork(
    data.frame(id = "node", group = "pending"), data.frame()
  )
  expect_silent(
    visNetwork::visGroups(
      graph, groupname = "pending", color = list(background = "#d9d9d9")
    )
  )
})

test_that("cost estimate is presentation-ready", {
  estimate <- estimate_openai_tagger_cost(100L, 10L)
  expect_s3_class(estimate, "tbl_df")
  expect_equal(estimate$component[[nrow(estimate)]], "total")
  expect_true(estimate$estimated_usd[[nrow(estimate)]] > 0)
})
