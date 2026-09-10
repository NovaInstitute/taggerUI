test_that("connect and resume page exposes the staged workflow", {
  html <- as.character(taggerUI:::.connect_resume_ui("connection"))
  expected <- c(
    "connection-fluree_url", "connection-ledger", "connection-branch",
    "connection-connect", "connection-survey_graph", "connection-run_id",
    "connection-tagging_graph_base", "connection-load_questions",
    "connection-resume_run", "connection-reload_run",
    "connection-graph_locations", "connection-load_activity"
  )
  expect_true(all(vapply(expected, grepl, logical(1), x = html, fixed = TRUE)))
})

test_that("connect page hides operational tuning under advanced controls", {
  html <- as.character(taggerUI:::.connect_resume_ui("connection"))
  expect_match(html, "Advanced connection settings", fixed = TRUE)
  expect_match(html, "Advanced loading settings", fixed = TRUE)
  expect_match(html, "Persistence batch size", fixed = TRUE)
})

test_that("resolved graph locations retain separate persistence roles", {
  graphs <- taggerUI:::.tagger_graphs(
    taggerUI:::.tagger_default_graph_base("ledger-one", "run-one")
  )
  expect_equal(names(graphs), c("run", "embedding", "hierarchy", "review"))
  expect_true(all(startsWith(
    unlist(graphs),
    "https://data.nova.org/integration/ledger-one/graph/tagging/openai/run-one/"
  )))
})
