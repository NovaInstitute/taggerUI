# Root entry point for the taxonomy-reviewer Shiny app.
#
# Lets the app be launched from the project root in any of these ways:
#   - shiny::runApp()            from an R session whose working dir is this folder
#   - RStudio "Run App" button   with this file open
#   - Rscript run-reviewer-app.R from the command line
#
# The app itself lives in reviewerWalkthrough/; this file only delegates to it.
# Shiny sets the working directory to this folder before sourcing app.R, so the
# relative path below is reliable.
#
# Shiny would also auto-source every file in R/ next to an app.R. Here R/ holds
# the *package* source, not app code, so R/_disable_autoload.R turns that off.

if (!file.exists(file.path("reviewerWalkthrough", "app.R"))) {
  stop("Could not find reviewerWalkthrough/app.R next to this file.", call. = FALSE)
}

shiny::shinyAppDir("reviewerWalkthrough")
