# Taxonomy reviewer

The taxonomy reviewer opens in **live mode** and loads the complete tagged dataset from Fluree. Taxonomy edits are applied immediately in the browser session and are written to the selected review branch only when the reviewer clicks **Save changes**. Closing the browser before saving discards those unsaved edits.

## Launch with the verified full dataset

From the workspace root, run in R:

```r
Sys.unsetenv("NOVA_REVIEW_MODE")
Sys.setenv(NOVA_REVIEWER_ID = "your-name")
shiny::runApp("taggerUI/reviewerWalkthrough")
```

The app is fixed to Fluree at `http://localhost:8090`, ledger `nova-survey-baseline-20260916`, and tagging run `survey-baseline-v1`. This baseline was verified on 20 September 2026: **10,801 question occurrences and 895 tagged groups**. Set `FLUREE_API_KEY` in the R environment if your Fluree server needs authentication; never put credentials in the source files.

Required installed packages: `shiny`, `novaRush`, `novaTagger`, `jsonlite` and `digest`, with their normal dependencies. No package is installed automatically. Review does not call OpenAI or regenerate embeddings.

## Start and resume a review

1. On **Workspace**, enter the reviewer name. The app lists Fluree branches with a lightweight administration request; it does not load the survey or taxonomy yet.
2. Select an existing review branch and click **Load selected workspace**. The questions and taxonomy are hydrated once, directly from that branch. Loading `main` first is not required.
3. To start a new review, enter a name such as `review-jaco` and click **Create from main and load**. Branch creation records the `main` commit with lightweight requests and then hydrates only the new branch.
4. Edit the taxonomy. Repeated same-labelled siblings and safe repeated parent/child levels are cleaned up automatically when the workspace loads; this cleanup remains unsaved and undoable until you click **Save changes**.
5. Click **Save changes** to persist all pending editor operations as one journal event. Wait for the **Saved** message before closing the app.
6. To resume later, launch again, choose the same review branch and load it. The saved taxonomy and question assignments are restored.

Changing the branch selector does not load data automatically; click **Load selected workspace** to switch. A failed or unconfirmed save keeps the in-memory draft visible but locks further writes until reload, because the server may already have accepted the event. Loading the branch again restores any event that Fluree accepted.

There is no Reset button in live mode. Start an independent review by creating a new branch from main. On Summary, **Prepare and validate publication** replaces the canonical taxonomy and assignments on the isolated review branch and reads them back. **Merge this taxonomy into main** is enabled only for that prepared state and refuses to publish if main advanced since the review began. Branch deletion is not automated.

## Data and persistence contract

The app reads the pipeline's persisted survey questions, hierarchy, labels, assignments and fit assessments using the installed `novaTagger` and `novaRush` APIs. Stable original question IDs and form/procedure provenance are retained; duplicate captions are not deduplicated. Missing leaf assignments are retained and queued for placement.

The fixed input locations match `integration/run-full-tagging.R`:

- Survey: `https://data.nova.org/integration/nova-survey-baseline-20260916/graph/survey`
- Tagging base: `https://data.nova.org/integration/nova-survey-baseline-20260916/graph/tagging/openai/survey-baseline-v1/`

Review decisions are immutable JSON event records in a dedicated named graph on the selected branch:

```text
<tagging-base>/reviewer/<branch>
```

Each event contains the command, reviewer identity, timestamp, predecessor and a fingerprint of the input baseline. Replaying these events reconstructs the full reviewer draft. One explicit editor save stores the pending commands as a single batched event rather than retransmitting thousands of questions or embeddings. No local file cache is needed to resume.

**The reviewer draft is a separate persisted review layer while it is being edited.** Preparing publication converts that journal into the canonical current taxonomy: explicit parent-linked clusters and direct question-to-leaf assignments. The review branch is verified before it is merged, so pipeline consumers on main continue to see the previous accepted state until publication succeeds.

A changed source baseline is rejected on resume rather than silently applying old decisions to different data. Stale sessions are rejected before saving. Simultaneous conflicting events are retained and detected as a fork; loading stops for reconciliation rather than silently choosing a winner. Use a separate branch per reviewer. Publication also records the base main commit and rejects a merge when main has advanced.

## Fit labels and suggestions

The app uses the latest saved accepted/edited leaf-label assessments: score below 0.5 is red, 0.5–0.65 amber, and at least 0.65 green. Missing assessments are amber, not invented green ratings. Missing leaf placements are red. The verified baseline produces 6,674 red, 3,434 amber and 693 green question occurrences.

For red questions, alternative destinations are calculated locally from the already-persisted question vectors and leaf centroids, in bounded batches. A suggestion is shown only when an alternative improves the score by at least 0.1. The verified baseline produces 1,518 suggestions. No external model request is made and no question-by-question square matrix is constructed.

These are baseline assessments and suggestions, not fresh model judgements after reviewer edits. Reviewers choose the final destination. Removed destinations are rejected; new leaves do not automatically receive new model suggestions.

## Original demo

The original self-contained mock mode is still available explicitly:

```r
Sys.setenv(NOVA_REVIEW_MODE = "demo")
shiny::runApp("taggerUI/reviewerWalkthrough")
```

Only this mode uses the 48 mock questions and session-only reset. The [presenter walkthrough](DEMO_WALKTHROUGH.md) applies to this mock mode; its scripted question order does not apply to the full dataset.

## Verification

From the workspace root:

```sh
Rscript taggerUI/reviewerWalkthrough/tests/smoke.R
Rscript taggerUI/reviewerWalkthrough/tests/persistence.R
```

The first checks all original demo actions. The second covers source mapping, saved fit assessments, provenance, JSON reload, persistent taxonomy and placement changes, stale writers, conflict detection, baseline changes, read-only protection, failed-save behaviour and live app construction.

Explicit live acceptance check against a running local Fluree:

```sh
Rscript taggerUI/reviewerWalkthrough/tests/live-persistence.R
Rscript taggerUI/reviewerWalkthrough/tests/live-persistence.R resume
```

This creates a uniquely named isolated test ledger and review branch, saves a rename, then verifies it in another R process and confirms main is unchanged. It retains the test ledger for inspection. Test connection information is stored at `/tmp/nova-review-live-test.rds` (override with `NOVA_REVIEW_TEST_INFO`). The completed dataset is not modified by this test.

To test the actual fixed 10,801-question corpus and reviewer journal, run:

```sh
Rscript taggerUI/reviewerWalkthrough/tests/live-full-corpus-persistence.R
Rscript taggerUI/reviewerWalkthrough/tests/live-full-corpus-persistence.R resume
```

The first command creates a uniquely named review branch from main, loads the full
configured corpus, and saves automatic cleanup plus one acceptance-test rename as
a single editor batch. The second reconnects in a fresh R process and verifies the
exact event head, stable renamed category, question count, branch, and unchanged
main head. The branch is retained for inspection; the test never prepares a
publication or merges into main. Resume information is stored at
`/tmp/nova-review-full-corpus-test.rds` by default.

On 20 September 2026 both automated suites and the separate-process live persistence check passed. Full-dataset loading was also verified against the baseline above. Test ledger retained: `nova-reviewer-test-20260920T213057`; an earlier empty test ledger `nova-reviewer-test-20260920T213042` was retained after discovering that Fluree requires an initial commit before creating a branch.

## Files

- `app.R`: entry point.
- `R/model.R`: taxonomy and placement transformations plus optional mock fixtures.
- `R/persistence.R`: pipeline adapter, Fluree journal, replay and save verification.
- `R/ui.R`: page shell and presentation helpers.
- `R/server.R`: session state, actions, saved-state handling and export.
- `www/style.css`: responsive styling.
- `tests/`: offline and live acceptance checks.

Deferred splits remain recorded for later discussion and may be published as an incremental checkpoint. There is no app-level authentication: the local app uses the configured Fluree credentials for write and merge authority, while the entered reviewer name is attribution only.

## Direct taxonomy editor

The Taxonomy editor tab supports inline rename (pencil, Enter to save, Escape to cancel),
dragging a whole subtree onto a new parent, a searchable Move to alternative,
combining categories, removing intermediate levels, adding categories, flagging,
and undo/redo. Structural operations show a preview. Question examples and source
information appear alongside the selected category. Search does not change data.

Edit validation belongs to novaTagger::edit_review_tree. Commands accumulate in
the Shiny session and an explicit save persists them through the existing review
journal as one batch. Automatic cleanup normalizes whitespace and case when it
merges same-labelled siblings, preserves their children and assignments, and
collapses a repeated parent/child level only when doing so is unambiguous.
Removing a category with direct questions
marks those questions pending rather than assigning them to an internal node.
Moving into a category with direct questions is rejected. Legacy placement or
walkthrough actions clear the direct editor's undo stack. Publication starts a
new baseline, so undo history is not carried through a published checkpoint.

Launch the matching source packages with `Rscript taggerUI/run-taxonomy-editor.R`.
For an isolated, durable local draft use `--snapshot /absolute/path/baseline.rds`.
The local journal is stored beside the snapshot as `.editor-events.rds`; preserve
both files. Run only one editor process per local snapshot. Local drafts do not
publish to Fluree. Download reviewed dataset exports the resulting draft.

The local snapshot mode does not bypass a Fluree journal fingerprint mismatch:
it is an explicitly separate baseline draft, without the incompatible events.

Run `Rscript taggerUI/reviewerWalkthrough/tests/editor.R` for direct editing and
replay tests, plus the existing smoke.R and persistence.R scripts.
