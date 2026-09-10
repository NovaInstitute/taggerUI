quality_state_fixture <- function() {
  questions <- tibble::tibble(
    id = paste0("q", 1:4),
    caption = c("Cooking fuel", "Heating fuel", "Household income", "Monthly income"),
    question_class = c("closed", "closed", "open", "open")
  )
  state <- novaTagger:::new_tag_state(questions, "quality-ui")
  state$embeddings <- rbind(c(1, 0), c(.9, .1), c(0, 1), c(.1, .9))
  state$assignments <- dplyr::mutate(
    questions, cluster_level_1 = c(1L, 1L, 2L, 2L), cluster_level_2 = 1L
  )
  state$clusters <- tibble::tibble(
    level = c(1L, 1L, 2L), cluster_id = c(1L, 2L, 1L),
    parent_cluster = c(1L, 1L, NA_integer_),
    question_ids = list(c("q1", "q2"), c("q3", "q4"), paste0("q", 1:4)),
    tag = c("fuel use", "income", "household circumstances")
  )
  state$clusters_by_level <- c(2L, 1L)
  state$workflow <- list(stage = "review")
  state <- novaTagger::register_tag_proposal(
    state, 1L, 1L, "fuel use", .95, "fuel questions", FALSE,
    "openai", "gpt-test", "embed-test", c(1, 0)
  )
  proposal <- names(state$proposals)[[1]]
  novaTagger::review_tag_proposal(state, proposal, "accepted", "reviewer")
}

test_that("cluster quality keeps centroid and tag fit separate", {
  state <- quality_state_fixture()
  questions <- taggerUI:::.cluster_quality_questions(state, 1L, 1L)
  expect_true(all(c("centroid_similarity", "tag_similarity") %in% names(questions)))
  expect_true(all(c("centroid_rank", "representative", "source_form_id") %in% names(questions)))
  expect_false(identical(questions$centroid_similarity, questions$tag_similarity))
  expect_equal(questions$question_id, c("q2", "q1"))

  summary <- taggerUI:::.cluster_quality_summary(state, 1L, 1L)
  expect_equal(summary$review_status, "accepted")
  expect_true(is.finite(summary$mean_centroid_similarity))
  expect_true(is.finite(summary$mean_tag_similarity))
})

test_that("hierarchy context identifies parent, children, and siblings", {
  state <- quality_state_fixture()
  leaf <- taggerUI:::.cluster_hierarchy_context(state, 1L, 1L)
  expect_true(all(c("parent", "selected", "sibling") %in% leaf$relationship))
  parent <- taggerUI:::.cluster_hierarchy_context(state, 2L, 1L)
  expect_true(all(c("selected", "child") %in% parent$relationship))
})

test_that("node metadata combines review status with quality flags", {
  quality <- taggerUI:::.cluster_node_quality(quality_state_fixture())
  expect_equal(nrow(quality), 3L)
  expect_equal(quality$review_status[quality$key == "1:1"], "accepted")
  expect_type(quality$flagged, "logical")
  choices <- taggerUI:::.cluster_choices(quality_state_fixture())
  expect_true("1:1" %in% unname(choices))
  expect_true(all(c("question_count", "centroid_outliers", "alternative_better") %in%
                    names(quality)))
})

test_that("question concern filters expose outliers and representatives", {
  questions <- taggerUI:::.cluster_quality_questions(quality_state_fixture(), 1L, 1L)
  representatives <- taggerUI:::.filter_cluster_quality_questions(
    questions, "representative"
  )
  expect_true(nrow(representatives) > 0L)
  expect_true(all(representatives$representative))
  expect_equal(taggerUI:::.filter_cluster_quality_questions(questions, "all"), questions)
})

test_that("hierarchy run summary is presentation-ready", {
  summary <- taggerUI:::.hierarchy_run_summary(list(state = quality_state_fixture()))
  expect_equal(names(summary), c("item", "value"))
  expect_true(all(c("Run", "Cluster records") %in% summary$item))
})

test_that("current cluster status uses proposal time rather than query order", {
  state <- quality_state_fixture()
  existing <- state$proposals[[1]]
  older <- existing
  older$proposal_id <- "older"
  older$status <- "rejected"
  older$created_at <- as.POSIXct("2026-01-01", tz = "UTC")
  existing$created_at <- as.POSIXct("2026-01-02", tz = "UTC")
  state$proposals <- list(current = existing, older = older)
  expect_equal(
    taggerUI:::.quality_latest_proposal(state, 1L, 1L)$status,
    "accepted"
  )
})

test_that("PCA selection follows the selected hierarchy level", {
  projection <- taggerUI:::.cluster_pca_projection(
    quality_state_fixture(), 1L, 2L
  )
  expect_equal(sum(projection$selected), 2L)
  expect_equal(projection$question_id[projection$selected], c("q3", "q4"))
})
