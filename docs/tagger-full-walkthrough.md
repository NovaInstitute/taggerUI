# Full Fluree and model-provider tagging walkthrough

This interface reviews questions, embeddings, BERTopic hierarchies, proposals,
and decisions that have already been persisted in Fluree. Initial form
ingestion and taxonomy construction are handled outside `taggerUI`.

## 1. Prerequisites

- Fluree is reachable at `http://localhost:8090`.
- The target ledger contains a survey graph and a novaTagger run.
- OpenAI credentials are available when generating or editing tags.

From the package directory:

```r
devtools::load_all("taggerUI")

```

## 2. Launch the visual walkthrough

```r
devtools::load_all("taggerUI")
run_fluree_tagger_app()
```

On **Connect & resume**:

1. Enter the Fluree URL, ledger, and branch and select **Connect to Fluree**.
   This is a lightweight health and ledger check.
2. Enter the survey graph and tagging run ID. Their graph locations are derived
   and displayed before any data is loaded.
3. Select **Resume tagging run**. The application loads questions and
   reconstructs the persisted run once, reports elapsed time, and retains it in
   the Shiny session.
4. Use **Reload from Fluree** only when an explicit cold refresh is required.

## 3. Inspect the stored question projection

Open **Inspect questions** after loading or resuming a run. The summary cards
show the corpus size, open and closed question counts, nested answer options,
repeat-group questions, and source forms. Filters allow a reviewer to inspect a
particular ontology question type or source form and search captions, source
field names, answer-option labels, and form identifiers.

Selecting a question shows its response semantics, source provenance,
repeat-group membership, and nested answer options. The raw projection remains
available under an expandable advanced section. This page never writes to
Fluree.

Quality warnings identify missing captions or source forms, closed questions
without options, open questions carrying options, and captions repeated after
case, punctuation, and whitespace normalisation. A repeated caption is only a
candidate for review; embedding-based semantic deduplication belongs to the
hierarchy and cluster-review workflow.

The **Review** page lets a person accept, edit, reject, or defer every
   proposal. Meaningful decisions are persisted as immutable review events on
   the selected branch, alongside a convenient current-state projection. Selecting a
   proposal shows every question in the cluster, its child tags, and the cosine
   similarity/distance between each question embedding and the proposed-tag
   embedding. Questions are ordered farthest-first to make weakly represented
   members easy to spot. Saving an edited tag recomputes these values.

The graph updates after every cluster. Grey nodes are pending, amber nodes are
proposed, green nodes are accepted/edited, and red nodes are rejected.

## 3. What happens during one tagging cycle

For the next pending bottom-up cluster, the controller:

1. calculates the centroid of the cluster's provider embeddings;
2. sends a Fluree query containing that centroid, its dimension, the embedding
   model constraint, the cosine-similarity expression, ordering, and limit;
3. receives similar questions and excludes questions already in the cluster;
4. builds a model prompt containing representative/outlier questions, child
   tags, and the similar questions returned by Fluree;
5. sends the prompt through the model-provider adapter and parses the JSON
   proposal;
6. writes the proposal, evidence, tag embedding, and resumable state together
   through one authoritative tag-store transaction on the isolated Fluree AI
   branch.

The **Inspect I/O** tab records these as separate request and response events.
It shows prompts, question text, Fluree query structure, similarity scores,
model outputs, HTTP statuses, and returned transaction data. Raw embedding
arrays are deliberately represented as `{ "__vector__": true, "dimension": N }`
because printing thousands of full vectors would make the audit log unusable.

No API key or Authorization header is recorded.

## 4. Equivalent step-by-step R code

Use this version to pause and inspect objects directly in the console:

```r
devtools::load_all("taggerUI")

events <- list()
capture_event <- function(event) {
  events[[length(events) + 1L]] <<- event
  cat("\n", event$system, event$direction,
      event$operation %||% event$endpoint, "\n")
  cat(jsonlite::toJSON(event, auto_unbox = TRUE, pretty = TRUE), "\n")
}

run_id <- "survey-review-2026-01"
branch <- fluree_ai_branch_name(run_id)

fluree <- fluree_config(
  base_url = "http://localhost:8090",
  ledger = "survey-tagger",
  ai_branch = branch,
  trace_callback = capture_event
)
store <- prepare_fluree_tag_store(fluree, run_id, branch)

run <- new_tagger_walkthrough(
  forms_path = "~/Downloads/forms.Rda",
  limit_n = Inf,
  store = store,
  run_id = run_id,
  event_callback = capture_event
)

provider <- openai_model_provider(openai_config(
  embed_model = "text-embedding-3-small",
  tagger_model = "gpt-5.4-mini"
))

# For local testing, the rest of the workflow is unchanged:
# provider <- ollama_model_provider(ollama_config(
#   embed_model = "nomic-embed-text",
#   tagger_model = "llama3.1:8b"
# ))

run <- walkthrough_embed(
  run,
  provider = provider,
  batch_size = 100,
  event_callback = capture_event,
  progress_callback = function(done, total, label) message(label)
)

run <- walkthrough_infer_bertopic(
  run,
  event_callback = capture_event
)

run$state$clusters_by_level
run$state$clusters

run <- walkthrough_prepare_fluree(
  run,
  config = fluree,
  event_callback = capture_event
)

run$fluree_config$ai_branch
walkthrough_next_cluster(run)

# Run exactly one Fluree retrieval + model proposal cycle.
run <- walkthrough_tag_next(
  run,
  provider = provider,
  evidence_limit = 5,
  precedent_limit = 6,
  guidance_limit = 8,
  sample_size = 5,
  event_callback = capture_event
)

run$proposals[[1]]
walkthrough_next_cluster(run)

# Human review example (use the key shown in names(run$proposals)).
proposal_key <- names(run$proposals)[[1]]
parts <- strsplit(proposal_key, ":", fixed = TRUE)[[1]]
run <- walkthrough_review_tag(
  run,
  level = parts[[1]],
  cluster_id = parts[[2]],
  decision = "accepted",
  event_callback = capture_event
)
```

The proposal contains `evidence$questions` and `evidence$precedents`.
Precedents are reviewed proposals retrieved from Fluree with the same embedding
model and dimension. Accepted and edited labels guide the model positively;
rejected labels are explicit counterexamples. Setting `precedent_limit = 0`
disables this retrieval without changing the rest of the workflow.

Reusable guidance is deliberately more controlled than precedents:

```r
guidance <- new_tagging_guidance(
  "Use a measured concept rather than the survey's procedural wording.",
  kind = "constraint",
  tags = c("labels", "concepts"),
  rationale = "The same concept appears under several legacy captions.",
  severity = "should"
)
guidance <- approve_tagging_guidance(guidance, reviewer_id = "reviewer-1")
fluree_upsert_tagging_guidance(
  guidance, fluree, branch = fluree$main_branch
)
```

Only approved guidance is recalled. The app shows the applied records
read-only, and every proposal stores those records in `evidence$guidance`.
Set `guidance_limit = 0` to disable guidance recall.

The app's **Guidance** tab is the normal review surface:

1. create a candidate with text, kind, tags, and rationale;
2. select it and approve or reject it with a stable reviewer ID;
3. retire an approved rule when it should no longer affect new proposals; or
4. supersede it to preserve the old rule and create a replacement candidate.

Reject, retire, and supersede require a rationale. A replacement is not active
until it receives its own approval. The tab also lists tag proposals on the
active AI branch that used the selected guidance.

Resume later without repeating completed embedding batches, clustering, or
review work:

```r
fluree <- fluree_config(
  ledger = "survey-tagger",
  ai_branch = "ai-run-survey-review-2026-01"
)
store <- fluree_tag_store(
  fluree,
  run_id = "survey-review-2026-01",
  branch = fluree$ai_branch
)
run <- load_tagger_walkthrough(store, fluree)
```

`fluree_list_tag_runs(fluree, branch)` lists resumable runs on a known branch.
A local RDS checkpoint is available only as an explicit debugging option and
is not the authoritative project record.

`isolate = TRUE` is the default for `walkthrough_infer_bertopic()` and is
recommended for full runs. Set `isolate = FALSE` only when debugging Python
interactively on a small sample.

## 5. Agent and MCP boundary

The UI is only a controller. The reusable operations are already separate:

- `walkthrough_next_cluster()` selects work;
- `fluree_search_cluster_evidence()` is the semantic retrieval tool;
- `fluree_search_tag_precedents()` retrieves reviewer-approved examples and
  rejected counterexamples;
- `fluree_search_tagging_guidance()` retrieves explicitly approved reusable
  rules from the stable project branch;
- `cluster_tag_prompt()` builds agent context;
- `walkthrough_tag_next()` proposes and persists a tag; and
- `walkthrough_review_tag()` applies human review.

An MCP server can later expose these operations as tools without moving the
clustering, Fluree, or review logic into the UI. The agent can then perform the
same single-cluster loop while the app continues to display the shared event
stream and checkpoint state.
