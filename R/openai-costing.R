# Presentation-only cost helpers for OpenAI-backed tagging runs.

#' Approximate Token Count
#'
#' @description
#' Rough token estimate for planning API cost. This uses the common heuristic of
#' one token per four characters, so it is intentionally approximate.
#'
#' @param text Character vector.
#'
#' @return Integer estimated token count.
#' @export
approx_token_count <- function(text) {
  text <- paste(as.character(text), collapse = "\n")
  as.integer(ceiling(nchar(text, type = "chars") / 4))
}

#' Estimate OpenAI Tagger Cost
#'
#' @description
#' Estimates OpenAI API cost for the tagging pipeline. The estimate separates
#' one-time question embedding cost from per-cluster tagging cost.
#'
#' @param n_questions Number of unique questions embedded.
#' @param n_clusters Total number of clusters tagged across all hierarchy levels.
#' @param avg_question_tokens Average tokens per question caption.
#' @param avg_prompt_input_tokens Average input tokens per model call.
#' @param avg_prompt_output_tokens Average output tokens per model call.
#' @param calls_per_cluster Expected generation calls per cluster.
#' @param tagger_input_per_1m Input token price per 1M tokens.
#' @param tagger_output_per_1m Output token price per 1M tokens.
#' @param embedding_input_per_1m Embedding input token price per 1M tokens.
#'
#' @return A tibble with component and total estimated costs.
#' @export
estimate_openai_tagger_cost <- function(
    n_questions,
    n_clusters,
    avg_question_tokens = 35,
    avg_prompt_input_tokens = 900,
    avg_prompt_output_tokens = 80,
    calls_per_cluster = 1,
    tagger_input_per_1m = 0.75,
    tagger_output_per_1m = 4.50,
    embedding_input_per_1m = 0.02) {
  embedding_tokens <- n_questions * avg_question_tokens
  generation_input_tokens <- n_clusters * calls_per_cluster * avg_prompt_input_tokens
  generation_output_tokens <- n_clusters * calls_per_cluster * avg_prompt_output_tokens

  embedding_cost <- embedding_tokens / 1e6 * embedding_input_per_1m
  generation_input_cost <- generation_input_tokens / 1e6 * tagger_input_per_1m
  generation_output_cost <- generation_output_tokens / 1e6 * tagger_output_per_1m

  tibble::tibble(
    component = c("embeddings", "tagging_input", "tagging_output", "total"),
    tokens = c(
      embedding_tokens,
      generation_input_tokens,
      generation_output_tokens,
      embedding_tokens + generation_input_tokens + generation_output_tokens
    ),
    estimated_usd = c(
      embedding_cost,
      generation_input_cost,
      generation_output_cost,
      embedding_cost + generation_input_cost + generation_output_cost
    )
  )
}
