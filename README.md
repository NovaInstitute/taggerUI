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
