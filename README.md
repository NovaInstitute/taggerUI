# taggerUI

`taggerUI` is the Shiny reviewer application. It presents workflows implemented
by `novaTagger`, optionally initiates imports through `novaGraphDB`, and uses
`novaRush` only for application-level connection and run selection concerns.

Reactive state is never authoritative project state.

The combined prototype walkthrough is now staged here so the interface can be
reviewed and decomposed in its destination package. Construct it with
`tagger_app()` or launch it with `run_fluree_tagger_app()`. Its server still
uses compatibility calls that must be mapped onto the public APIs of the other
three packages before the complete workflow is expected to run.
