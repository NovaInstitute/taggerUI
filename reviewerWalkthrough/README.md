# Taxonomy reviewer

The taxonomy reviewer opens in **live mode**. Taxonomy edits are applied immediately in the R/Shiny session and are written to the selected review branch journal only when the reviewer clicks **Save progress**. Closing the browser before saving discards those unsaved edits.

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
2. Select an existing review branch and click **Load selected workspace**. The fixed question corpus is reused from the app server cache after its first load. The app reads only the branch's current reviewed taxonomy and recent journal; embeddings and historical proposal records are skipped. Loading `main` first is not required.
3. To start a new review, enter a name such as `review-jaco` and click **Create from main and load**. Branch creation records the `main` commit with lightweight requests and then hydrates only the new branch.
4. Edit the taxonomy. Repeated same-labelled siblings and safe repeated parent/child levels are cleaned up automatically when the workspace loads; this cleanup remains unsaved and undoable until you click **Save progress**.
5. Click **Save progress** to persist all pending editor operations as one journal event. Wait for the **Saved** message before closing the app.
6. Click **Create branch checkpoint** periodically to materialize and verify the complete current taxonomy on the review branch. The journal is cleared only after verification, and the branch remains editable.
7. To resume later, choose the same review branch. The latest checkpoint is loaded directly and only events saved after it are replayed.

Changing the branch selector does not load data automatically; click **Load selected workspace** to switch. A failed or unconfirmed save keeps the in-memory draft visible but locks further writes until reload, because the server may already have accepted the event. Loading the branch again restores any event that Fluree accepted.

There is no Reset button in live mode. Start an independent review by creating a new branch from main. **Prepare for publication** is available only when the journal is empty after a verified checkpoint; it records the publication and locks the branch. **Publish checkpoint to main** refuses to merge if main advanced since the review began. Branch deletion is not automated.

## Data and persistence contract

The app reads the pipeline's persisted survey questions, hierarchy, labels, assignments and fit assessments using the installed `novaTagger` and `novaRush` APIs. Stable original question IDs and form/procedure provenance are retained; duplicate captions are not deduplicated. Missing leaf assignments are retained and queued for placement.

The fixed input locations match `integration/run-full-tagging.R`:

- Survey: `https://data.nova.org/integration/nova-survey-baseline-20260916/graph/survey`
- Tagging base: `https://data.nova.org/integration/nova-survey-baseline-20260916/graph/tagging/openai/survey-baseline-v1/`

Review decisions are immutable JSON event records in a dedicated named graph on the selected branch:

```text
<tagging-base>/reviewer/<branch>
```

Each event contains the command, reviewer identity, timestamp, predecessor and a fingerprint of the input baseline. Replaying the events since the latest checkpoint reconstructs the reviewer draft. One explicit progress save stores the pending commands as a single batched event rather than retransmitting thousands of questions or embeddings.

**The journal is a short-term recovery layer, while a checkpoint is the materialized branch state.** Checkpointing converts the journal into explicit parent-linked clusters and direct question-to-leaf assignments, verifies the read-back checksums, clears only the journal events, and keeps the branch editable. Publication is a later, separate operation.

A changed source baseline is rejected on resume rather than silently applying old decisions to different data. Stale sessions are rejected before saving. Simultaneous conflicting events are retained and detected as a fork; loading stops for reconciliation rather than silently choosing a winner. Use a separate branch per reviewer. Publication also records the base main commit and rejects a merge when main has advanced.

## Reviewer load profile

The taxonomy editor does not load question embeddings or historical proposal/review records. It needs question identifiers and captions, the current hierarchy, current assignments, and the short journal since the latest checkpoint. No external model request is made during review.

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
