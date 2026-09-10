review_state_fixture <- function() {
  questions <- tibble::tibble(
    id = c("q1", "q2"), caption = c("Cooking fuel?", "Heating fuel?")
  )
  state <- novaTagger:::new_tag_state(questions, "ui-review")
  state$embeddings <- rbind(c(1, 0), c(.8, .2))
  state$assignments <- dplyr::mutate(questions, cluster_level_1 = 1L)
  state$clusters <- tibble::tibble(
    level = 1L, cluster_id = 1L, parent_cluster = NA_integer_,
    question_ids = list(c("q1", "q2")), tag = NA_character_
  )
  state$clusters_by_level <- 1L
  state$workflow <- list(stage = "clustered")
  novaTagger::register_tag_proposal(
    state, 1L, 1L, "fuel use", confidence = .91,
    rationale = "Both questions concern fuel", needs_review = FALSE,
    provider = "openai", model = "gpt-test", embedding_model = "embed-test",
    tag_embedding = c(1, 0), prompt = "sensitive prompt",
    raw_response = "sensitive response"
  )
}

test_that("proposal presentation exposes provenance but not prompts or vectors", {
  state <- review_state_fixture()
  table <- taggerUI:::.proposal_table(state)
  expect_equal(table$tag, "fuel use")
  expect_equal(table$embedding_dimension, 2L)
  expect_false(any(c("prompt", "raw_response", "tag_embedding") %in% names(table)))

  proposal <- state$proposals[[1]]
  detail <- taggerUI:::.safe_proposal_detail(proposal)
  expect_equal(detail$generation_model, "gpt-test")
  expect_equal(detail$embedding_dimension, 2L)
  expect_false(any(c("prompt", "raw_response", "tag_embedding") %in% names(detail)))
})

test_that("proposal similarities are reviewer-ready", {
  state <- review_state_fixture()
  scores <- taggerUI:::.proposal_question_scores(state, state$proposals[[1]])
  expect_equal(nrow(scores), 2L)
  expect_true(all(c("cosine_similarity", "cosine_distance") %in% names(scores)))
  summary <- taggerUI:::.similarity_summary(scores)
  expect_equal(summary$value[[1]], 2)
  expect_true(summary$value[[2]] > .9)
})

test_that("review history presents all decision types and edit improvement", {
  decisions <- c("accepted", "edited", "rejected", "deferred")
  for (decision in decisions) {
    state <- review_state_fixture()
    proposal_id <- names(state$proposals)[[1]]
    if (identical(decision, "edited")) {
      state <- novaTagger::review_tag_proposal(
        state, proposal_id, decision, "reviewer", "more precise",
        tag = "household fuel",
        embed_tag = function(value) c(.9, .1)
      )
    } else {
      state <- novaTagger::review_tag_proposal(
        state, proposal_id, decision, "reviewer", paste(decision, "test")
      )
    }
    history <- taggerUI:::.proposal_review_events(state, proposal_id)
    expect_equal(history$decision, decision)
    expect_equal(history$reviewer_id, "reviewer")
    if (identical(decision, "edited")) {
      expect_true(is.finite(history$before_mean))
      expect_true(is.finite(history$after_mean))
      expect_true(is.finite(history$mean_change))
    }
  }
})
