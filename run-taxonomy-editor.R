#!/usr/bin/env Rscript
# Usage: Rscript run-taxonomy-editor.R [--snapshot /absolute/baseline.rds] [--port 3847]
args <- commandArgs(TRUE)
script <- sub('^--file=','',commandArgs(FALSE)[grepl('^--file=',commandArgs(FALSE))])
root <- dirname(normalizePath(script))
option <- function(key,default=NULL) {i<-match(key,args);if(is.na(i)) default else args[i+1L]}
if(!requireNamespace('pkgload',quietly=TRUE)) stop('This source launcher requires pkgload.')
# Use the matching checkout APIs instead of a potentially stale installed package.
if(dir.exists(file.path(root,'../novaRush'))) pkgload::load_all(file.path(root,'../novaRush'),quiet=TRUE)
tagger_candidates <- file.path(root,c('../novaTagger','../semanticTagger'))
tagger_path <- tagger_candidates[dir.exists(tagger_candidates)][1L]
if(is.na(tagger_path)) stop('Could not find a sibling novaTagger or semanticTagger checkout.')
pkgload::load_all(tagger_path,quiet=TRUE)
library(shiny)
setwd(file.path(root,'reviewerWalkthrough'))
addResourcePath('editor-assets',normalizePath('www'))
source('R/model.R');source('R/editor.R');source('R/ui.R');source('R/server.R');source('R/persistence.R')
snapshot <- option('--snapshot')
if(is.null(snapshot)) {
  shiny::runApp(shinyApp(app_ui(),app_server),host='127.0.0.1',port=as.integer(option('--port','3847')),launch.browser=FALSE)
} else {
  snapshot <- normalizePath(snapshot,mustWork=TRUE)
  baseline <- readRDS(snapshot)
  journal_path <- paste0(snapshot,'.editor-events.rds')
  read_events <- function() if(file.exists(journal_path)) readRDS(journal_path) else list()
  append_event <- function(e) {
    events <- read_events();events[[length(events)+1L]] <- e
    tmp <- tempfile(tmpdir=dirname(journal_path));saveRDS(events,tmp)
    if(!file.rename(tmp,journal_path)) stop('Could not save the local draft journal.')
  }
  # One local editor process owns the journal; stale sessions remain guarded by event heads.
  backend <- list(
    list_branches=function(settings) 'local-draft',
    create_branch=function(settings,name) stop('Use the live app to create Fluree branches.'),
    connect=function(settings) {
      if(settings$branch!='local-draft') stop('This snapshot has one local-draft workspace.')
      x <- new_review_session(baseline,read_events,append_event,'local-reviewer')
      c(x,list(branches='local-draft',settings=settings))
    }
  )
  Sys.setenv(FLUREE_BASE_URL='local snapshot',FLUREE_TEST_LEDGER='Downloaded baseline',NOVA_TAGGING_RUN_ID='survey-baseline-v1',NOVA_REVIEWER_ID='local-reviewer',FLUREE_TEST_BRANCH='local-draft')
  ui <- app_ui(FALSE,local=TRUE)
  ui <- tagList(tags$div(class='notice',style='margin:16px 30px',strong('Local baseline draft'),
    ' Edits are saved on this computer. The three incompatible Fluree review events are not applied. No changes are sent to Fluree.'),ui)
  server <- function(input,output,session) {
    app_server(input,output,session,demo=FALSE,backend=backend)
    session$onFlushed(function() session$sendCustomMessage('openLocalDraft',TRUE),once=TRUE)
  }
  ui <- tagList(ui,tags$script(HTML("$(document).on('shiny:connected',function(){Shiny.addCustomMessageHandler('openLocalDraft',function(message){document.getElementById('connect').click();document.getElementById('start').click();});});")))
  shiny::runApp(shinyApp(ui,server),host='127.0.0.1',port=as.integer(option('--port','3847')),launch.browser=FALSE)
}
