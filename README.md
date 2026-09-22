# taggerUI

`taggerUI` is the Shiny reviewer application. It presents workflows implemented
by `novaTagger`, optionally initiates imports through `novaGraphDB`, and uses
`novaRush` only for application-level connection and run selection concerns.

Reactive state is never authoritative project state.

Construct the application with `tagger_app()` or launch it with
`run_fluree_tagger_app()`. The first page separates a lightweight Fluree
connection check from loading survey questions and reconstructing a persisted
tagging run. A resumed run is retained in the Shiny session; ordinary page
navigation does not repeatedly hydrate all vectors and hierarchy records.

Changing the ledger, branch, run, or graph locations invalidates the hydrated
session state. `Reload from Fluree` is the explicit recovery operation. The
page warns when reviewers select `main` and identifies non-main review branches
as isolated.

The **Inspect questions** page is a read-only view of the novaTagger question
projection loaded from Fluree. It separates open and closed questions, displays
nested answer options and survey provenance, and filters by ontology type,
source form, and repeat-group membership. Extraction-quality checks highlight
missing metadata, inconsistent answer-option semantics, and captions repeated
across the corpus. Repeated captions are candidates for inspection, not an
automatic declaration that two survey questions are semantically identical.

The active tagging page intentionally uses a lightweight hierarchy-progress
view rather than calculating PCA and full cluster diagnostics. It shows how
many clusters at each level have been reviewed and lets reviewers inspect the
generated tags and statuses at any level. Full hierarchy cleanup remains
available for a later workflow phase.

Tag proposals are checkpointed for resumability. The Shiny session uses the
workflow returned by each focused proposal or review write instead of
rehydrating all question embeddings and hierarchy records after every action.
`Reload from Fluree` remains the explicit synchronization operation. Tag-label
embeddings are requested at the same dimension as the resumed question
embeddings.

Before generation, the review page previews the next cluster. Once a proposal
exists, that cluster remains the active context and all of its questions are
shown weakest tag match first. The page advances only after the reviewer
accepts, modifies, rejects, or defers the proposal.
