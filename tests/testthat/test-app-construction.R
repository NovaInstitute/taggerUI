test_that("walkthrough UI constructs", {
  skip_if_not_installed("DT")
  skip_if_not_installed("visNetwork")
  app <- tagger_app()
  expect_s3_class(app, "shiny.appobj")
  expect_true(is.function(app$serverFuncSource()))
})

test_that("cost estimate is presentation-ready", {
  estimate <- estimate_openai_tagger_cost(100L, 10L)
  expect_s3_class(estimate, "tbl_df")
  expect_equal(estimate$component[[nrow(estimate)]], "total")
  expect_true(estimate$estimated_usd[[nrow(estimate)]] > 0)
})
