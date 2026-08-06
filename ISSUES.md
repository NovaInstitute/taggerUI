# taggerUI implementation backlog

## Required for the architecture

- [x] Stage the Shiny walkthrough and reviewer tutorial in this package.
- [ ] Use only public `novaTagger` APIs.
- [ ] Add project, ledger, branch, run, and model selection.
- [ ] Support starting, resuming, and recovering workflows.
- [ ] Keep reactive values limited to presentation state.

## Staged compatibility calls

- [ ] Replace legacy `new_tagger_walkthrough()` and
  `load_tagger_walkthrough()` calls with public `novaGraphDB` extraction plus
  `novaTagger` workflow construction/resume APIs.
- [ ] Replace legacy `prepare_fluree_tag_store()` and `fluree_tag_store()`
  calls with a composition adapter built from public `novaRush` operations and
  `novaTagger` store contracts.
- [ ] Map evidence, precedent, and approved-guidance retrieval onto the new
  callback-based evidence repository.
- [ ] Replace the remaining combined-prototype guidance persistence helpers
  after their domain/transport ownership is finalized.
- [ ] Decide whether OpenAI price configuration belongs in `novaTagger` or an
  application configuration layer; only the display calculator is staged here.

## Reviewer workflow

- [ ] Split the monolithic application into testable Shiny modules.
- [ ] Show embedding, clustering, persistence, and tagging progress separately.
- [ ] Present questions, outliers, evidence, precedents, and guidance.
- [ ] Support accept, edit, reject, defer, and rationale capture.
- [ ] Support preview/apply/discard question reclassification.
- [ ] Add split, merge, and scoped-reclustering interfaces later.
- [ ] Add PCA and UMAP navigation with diagnostic context.
- [ ] Show before/after similarity changes.

## Resilience and usability

- [ ] Explain uncertain writes and provide safe status checks.
- [ ] Resume after browser, R, model-provider, or Fluree interruption.
- [ ] Never expose API keys in logs or persisted state.
- [ ] Add accessible controls and large-table pagination.
- [ ] Add Shiny module and end-to-end browser tests.
- [x] Add a lightweight application-construction test.
- [ ] Add documentation, deployment guidance, CI, and `R CMD check`.
