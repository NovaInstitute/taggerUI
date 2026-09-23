#!/usr/bin/env Rscript

# Launch the standalone taxonomy-reviewer app bundled with this repository.
# Usage: Rscript run-reviewer-app.R

script_file <- sub(
  "^--file=", "",
  commandArgs(trailingOnly = FALSE)[grepl("^--file=", commandArgs(trailingOnly = FALSE))][[1L]]
)
project_dir <- dirname(normalizePath(script_file, mustWork = TRUE))
app_dir <- file.path(project_dir, "reviewerWalkthrough")

if (!dir.exists(app_dir) || !file.exists(file.path(app_dir, "app.R"))) {
  stop("Could not find reviewerWalkthrough/app.R next to this launcher.", call. = FALSE)
}
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install the R package 'shiny' before launching the reviewer app.", call. = FALSE)
}

if ("--check" %in% commandArgs(trailingOnly = TRUE)) {
  message("Reviewer app launcher is ready: ", app_dir)
  quit(status = 0L)
}

shiny::runApp(app_dir, launch.browser = interactive())
