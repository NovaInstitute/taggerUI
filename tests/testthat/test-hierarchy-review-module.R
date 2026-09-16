hierarchy_review_state_fixture <- function() {
  questions <- tibble::tibble(
    id = paste0("q", 1:4),
    caption = c("Cooking fuel", "Heating fuel", "Household income", "Monthly income")
  )
  state <- novaTagger:::new_tag_state(questions, "hierarchy-review")
  state$assignments <- dplyr::mutate(
    questions, cluster_level_1 = c(1L, 1L, 2L, 2L), cluster_level_2 = 1L
  )
  state$clusters <- tibble::tibble(
    level = c(1L, 1L, 2L), cluster_id = c(1L, 2L, 1L),
    parent_cluster = c(1L, 1L, NA_integer_),
    question_ids = list(c("q1", "q2"), c("q3", "q4"), paste0("q", 1:4)),
    tag = c("fuel use", "fuel use", "household circumstances")
  )
  state
}

test_that("hierarchy review identifies top-down split parents", {
  state <- hierarchy_review_state_fixture()
  choices <- taggerUI:::.hierarchy_split_choices(state)
  expect_equal(unname(choices), "2:1")
  expect_match(names(choices), "household circumstances", fixed = TRUE)
  expect_equal(
    taggerUI:::.hierarchy_split_path(state, 2L, 1L),
    "household circumstances"
  )
})

test_that("hierarchy review presents child examples and repeated tags", {
  children <- taggerUI:::.hierarchy_split_children(
    hierarchy_review_state_fixture(), 2L, 1L
  )
  expect_equal(nrow(children), 2L)
  expect_true(all(children$repeated_sibling_tag))
  expect_match(children$representative_questions[[1]], "Cooking fuel", fixed = TRUE)
})

test_that("hierarchy review UI exposes a split selector and child table", {
  html <- as.character(taggerUI:::.hierarchy_review_ui("hierarchy"))
  expected <- c("hierarchy-split_parent", "hierarchy-parent_path", "hierarchy-split_children")
  expect_true(all(vapply(expected, grepl, logical(1), x = html, fixed = TRUE)))
  expect_match(html, "more specific than the parent", fixed = TRUE)
})
