# Self-contained reviewer demo data. This never contacts Fluree or OpenAI.

.demo_topics <- function() list(
  list("Age and birth details", "personal", c("What is your age?", "What year were you born?", "Which age range applies to you?")),
  list("Education", "personal", c("What is your highest education level?", "Are you currently studying?", "Which qualification did you complete?")),
  list("Employment", "personal", c("Are you currently employed?", "What is your employment status?", "How many hours do you work each week?")),
  list("Income and earnings", "personal", c("What is your monthly income?", "What is your main source of income?", "Has your income changed recently?")),
  list("Tobacco and alcohol", "health", c("Do you currently smoke tobacco?", "How often do you drink alcohol?", "Have you used tobacco in the last month?")),
  list("Diet and nutrition", "health", c("How often do you eat fruit?", "How often do you eat vegetables?", "Do you have enough food each week?")),
  list("Physical activity", "health", c("How many days do you exercise?", "How often do you walk for transport?", "How active is your daily work?")),
  list("Chronic conditions", "health", c("Have you been diagnosed with diabetes?", "Do you have a long-term health condition?", "Are you taking long-term medication?")),
  list("Anxiety and depression", "wellbeing", c("How often have you felt anxious?", "How often have you felt depressed?", "How often have you felt hopeless?")),
  list("Sleep and fatigue", "wellbeing", c("How well have you been sleeping?", "How often do you feel tired?", "Do you wake up rested?")),
  list("Social support", "wellbeing", c("Do you have someone to rely on?", "How often do you feel lonely?", "Can you ask family for help?")),
  list("Safety", "wellbeing", c("Do you feel safe at home?", "Do you feel safe in your neighbourhood?", "Have you experienced violence recently?")),
  list("Healthcare access", "access", c("Do you have a regular healthcare provider?", "How long does it take to reach a clinic?", "Did cost stop you seeking care?")),
  list("Service use", "access", c("Have you used counselling services?", "Have you used social support services?", "Did you receive the service you needed?")),
  list("Housing", "access", c("Is your housing stable?", "How many people share your home?", "Do you have reliable electricity?")),
  list("Transport", "access", c("Do you have reliable transport?", "How long is your usual commute?", "Did transport stop you attending care?"))
)

.demo_questions <- function() {
  rows <- lapply(seq_along(.demo_topics()), function(index) {
    topic <- .demo_topics()[[index]]
    tibble::tibble(
      id = paste0("demo-q", index, "-", seq_along(topic[[3L]])),
      caption = topic[[3L]], demo_topic = topic[[1L]], demo_domain = topic[[2L]],
      question_class = "closed", question_type = "single_choice",
      response_cardinality = "one", response_datatype = "string",
      source_element_name = paste0("field_", index, "_", seq_along(topic[[3L]])),
      element_order = seq_along(topic[[3L]]), repeat_group_id = NA_character_,
      source_form_id = ifelse(topic[[2L]] %in% c("personal", "health"), "intake", "follow-up"),
      answer_options = rep(list(tibble::tibble(option_id = c("yes", "no"), option_text = c("Yes", "No"), option_code = c("1", "0"), option_order = 1:2)), length(topic[[3L]])),
      answer_count = 2L
    )
  })
  dplyr::bind_rows(rows)
}

.demo_embedding <- function(text, dimensions = 2L) {
  seed <- sum(utf8ToInt(tolower(as.character(text))))
  base <- c((seed %% 97L) / 97, ((seed %/% 97L) %% 97L) / 97)
  if (dimensions <= 2L) return(base[seq_len(dimensions)])
  c(base, rep(0, dimensions - 2L))
}

.demo_model_provider <- function(dimensions) novaTagger::new_model_provider(
  name = "guided-demo", embedding_model = "guided-demo-embedding",
  generation_model = "guided-demo-generator",
  embed_one = function(text, trace_callback = NULL) .demo_embedding(text, dimensions),
  generate = function(prompt, max_output_tokens, temperature, format, trace_callback = NULL) {
    '{"tag":"Health habits","confidence":0.78,"rationale":"The questions describe related everyday health behaviours.","needs_review":true}'
  }, validate = function() TRUE
)

.demo_cluster_tag <- function(questions, level) {
  topic <- as.character(questions$demo_topic[[1L]])
  index <- match(topic, vapply(.demo_topics(), `[[`, character(1), 1L))
  if (level == 1L) {
    if (identical(topic, "Income and earnings")) return("Work and income")
    return(topic)
  }
  if (level == 2L) return(c("Demographics and education", "Work and income", "Health habits", "Physical health", "Emotional wellbeing", "Social wellbeing and safety", "Healthcare and services", "Housing and transport")[[ceiling(index / 2)]])
  if (level == 3L) return(c("Personal profile", "Physical health", "Wellbeing and safety", "Access and living conditions")[[ceiling(index / 4)]])
  c("Personal and health context", "Wellbeing and access context")[[ceiling(index / 8)]]
}

.demo_question_queue <- function(workflow) {
  state <- workflow$state
  ids <- c("demo-q4-3", "demo-q8-2", "demo-q11-2", "demo-q16-3")
  reasons <- c("Income-change wording may fit employment rather than earnings.", "Long-term medication may need a chronic-condition check.", "Loneliness may belong with emotional wellbeing rather than social support.", "Transport barrier to care may fit healthcare access rather than transport.")
  suggested <- c("Employment", "Chronic conditions", "Anxiety and depression", "Healthcare access")
  dplyr::bind_rows(lapply(seq_along(ids), function(index) {
    question <- state$questions[match(ids[[index]], state$questions$id), , drop = FALSE]
    tibble::tibble(question_id = ids[[index]], question = question$caption[[1L]],
                   reason = reasons[[index]], suggested_destination = suggested[[index]],
                   status = if (index == 1L) "needs review" else "queued")
  }))
}

.guided_demo_workspace <- function() {
  questions <- .demo_questions(); store <- novaTagger::memory_tag_store()
  workflow <- novaTagger::new_tagging_workflow(questions, store = store, run_id = "guided-review-demo")
  topic_index <- rep(seq_along(.demo_topics()), each = 3L)
  workflow$state$embeddings <- cbind(topic_index * 10 + rep(c(-.05, 0, .05), length(.demo_topics())), rep(c(-.02, 0, .02), length(.demo_topics())))
  workflow$state$workflow$stage <- "embedded"
  workflow$state <- novaTagger::tag_store_save(store, workflow$state)
  workflow <- novaTagger::workflow_cluster_hierarchical(workflow, c(16L, 8L, 4L, 2L))
  for (index in seq_len(nrow(workflow$state$clusters))) {
    cluster <- workflow$state$clusters[index, , drop = FALSE]
    cluster_questions <- workflow$state$questions[match(unlist(cluster$question_ids[[1L]]), workflow$state$questions$id), , drop = FALSE]
    tag <- .demo_cluster_tag(cluster_questions, cluster$level[[1L]])
    workflow$state <- novaTagger::register_tag_proposal(workflow$state, cluster$level[[1L]], cluster$cluster_id[[1L]], tag, confidence = .82, rationale = "Seeded guided-demo tag.", provider = "guided-demo", model = "guided-demo-generator", embedding_model = "guided-demo-embedding", tag_embedding = .demo_embedding(tag))
    proposal_id <- utils::tail(names(workflow$state$proposals), 1L)
    workflow$state <- novaTagger::review_tag_proposal(workflow$state, proposal_id, "accepted", "demo-seed", "Prepared for reviewer walkthrough.")
    workflow$state <- novaTagger::tag_store_save(store, workflow$state)
  }
  target <- workflow$state$clusters[workflow$state$clusters$level == 1L, , drop = FALSE][1L, , drop = FALSE]
  workflow$state <- novaTagger::register_tag_proposal(workflow$state, target$level[[1L]], target$cluster_id[[1L]], "Personal profile", confidence = .58, rationale = "This deliberately broad label should be checked against the questions.", provider = "guided-demo", model = "guided-demo-generator", embedding_model = "guided-demo-embedding", tag_embedding = .demo_embedding("Personal profile"))
  workflow$state$workflow$stage <- "review"
  workflow$state <- novaTagger::tag_store_save(store, workflow$state)
  list(questions = questions, workflow = workflow, store = store, question_queue = .demo_question_queue(workflow))
}
