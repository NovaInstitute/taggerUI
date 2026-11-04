args <- commandArgs(FALSE);path <- sub('^--file=','',args[grepl('^--file=',args)])
setwd(normalizePath(file.path(dirname(path),'..')))
tagger_paths <- c('../../novaTagger','../../semanticTagger')
tagger_path <- tagger_paths[dir.exists(tagger_paths)][1L]
if(is.na(tagger_path)) stop('Could not find a sibling novaTagger or semanticTagger checkout.')
pkgload::load_all(tagger_path,quiet=TRUE)
library(shiny)
source('R/model.R');source('R/editor.R');source('R/ui.R');source('R/server.R');source('R/persistence.R')
expect_error <- function(expr,pattern) {
  e <- tryCatch({force(expr);NULL},error=identity)
  stopifnot(inherits(e,'error'),grepl(pattern,conditionMessage(e)))
}
raw <- list(run_id='real-fixture',revision=17L,
  questions=data.frame(id=c('question-b','question-a','question-c','question-outlier'),caption=c('B','A','C','Unplaced'),source_form_id=c('form-2','form-1','form-1','form-3')),
  clusters=data.frame(level=c(1L,2L,1L),cluster_id=c(20L,1L,10L),parent_cluster=c(1L,NA,1L),tag=c('Travel','Life','Health')),
  assignments=data.frame(id=c('question-b','question-a','question-c','question-outlier'),cluster_level_1=c(20L,10L,10L,NA)),
  review_events=list(list(level=1L,cluster_id=10L,decision='accepted',created_at='2026-09-01',similarity=list(questions=data.frame(question_id=c('question-a','question-c'),cosine_similarity=c(.8,.4))))))
baseline <- review_from_pipeline(raw)
stopifnot(identical(baseline$questions$id,sort(raw$questions$id)),sum(baseline$questions$fit=='red')==2,baseline$questions$fit[1]=='green')
stopifnot(baseline$questions$leaf[4]==1L,length(queue_ids(baseline))==2)
# The lightweight server projection retains every field that contributes to a
# review baseline fingerprint. Old branches therefore replay the same journal
# even though response metadata and answer options are no longer hydrated.
rich_raw <- raw
rich_raw$questions$iri <- paste0('https://example.org/',rich_raw$questions$id)
rich_raw$questions$procedure_id <- 'https://example.org/procedure'
rich_raw$questions$element_order <- seq_len(nrow(rich_raw$questions))
rich_raw$questions$unused_response_metadata <- 'not consumed by review state'
lean_raw <- rich_raw
lean_raw$questions <- lean_raw$questions[,c('id','caption','procedure_id','source_form_id','element_order','iri')]
stopifnot(identical(review_hash(review_from_pipeline(rich_raw)),review_hash(review_from_pipeline(lean_raw))))
# The canonical publication projection round-trips stable tree identities,
# assignments, and progress independently of the journal.
published <- review_state_to_taxonomy(baseline,raw$run_id,raw$revision+1L)
restored <- review_state_from_taxonomy(published,raw$questions)
stopifnot(nrow(restored$nodes)==nrow(baseline$nodes),
  nrow(restored$questions)==nrow(baseline$questions),
  setequal(restored$nodes$stable_id,baseline$nodes$stable_id))
# Publication change counts tolerate duplicate siblings in the pipeline source;
# only the cleaned checkpoint must satisfy canonical publication validation.
duplicate_source <- baseline
parent_counts <- table(duplicate_source$nodes$parent)
duplicate_parent <- as.integer(names(parent_counts)[which.max(parent_counts)])
siblings <- duplicate_source$nodes$id[!is.na(duplicate_source$nodes$parent) & duplicate_source$nodes$parent==duplicate_parent]
duplicate_source$nodes$label[match(siblings[1:2],duplicate_source$nodes$id)] <- c('Repeated label','repeated LABEL')
expect_error(review_state_to_taxonomy(duplicate_source,raw$run_id,raw$revision),'Sibling taxonomy labels')
clean_checkpoint <- apply_review_command(duplicate_source,list(type='edit',action='deduplicate'))
checkpoint <- review_state_to_taxonomy(clean_checkpoint,raw$run_id,raw$revision+1L)
changes <- review_state_change_counts(duplicate_source,clean_checkpoint)
stopifnot(inherits(checkpoint,'reviewed_taxonomy'),changes[['clusters_removed']]==1L)
# Persist JSON to disk, then construct fresh sessions to verify restart behaviour.
folder <- tempfile();dir.create(folder)
read_events <- function() lapply(list.files(folder,full.names=TRUE),function(p) jsonlite::fromJSON(p,simplifyVector=TRUE))
append_event <- function(e) writeLines(review_json(e),file.path(folder,paste0(e$id,'.json')))
a <- new_review_session(baseline,read_events,append_event,'reviewer-a')
x <- a$save(list(type='review',target=1L,action='rename',child=2L,label='Daily life'),a$head)
b <- new_review_session(baseline,read_events,append_event,'reviewer-b')
stopifnot(identical(b$head,x$head),node_label(b$state,2L)=='Daily life',length(b$state$reviewed)==1)
expect_error(a$save(list(type='review',target=1L,action='approve'),a$head),'Another session')
x <- b$save(list(type='review',target=current_split(b$state),action='collapse',child=3L),b$head)
b <- new_review_session(baseline,read_events,append_event,'reviewer-b')
stopifnot(!3L %in% b$state$nodes$id,length(b$state$affected)==2)
x <- b$save(list(type='place',target=queue_item(b$state),decision='new',name='New health home',parent=2L,rationale='Specific home'),b$head)
b <- new_review_session(baseline,read_events,append_event,'reviewer-b')
stopifnot(b$state$new_count==1,length(b$state$resolved)==1)
x <- b$save(list(type='place',target=queue_item(b$state),decision='unresolved'),b$head)
b <- new_review_session(baseline,read_events,append_event,'reviewer-b')
stopifnot(length(b$state$unresolved)==1)
ro <- new_review_session(baseline,read_events,append_event,'reader',read_only=TRUE)
expect_error(ro$save(list(),ro$head),'read only')
changed <- baseline;changed$source$revision <- 18L
expect_error(new_review_session(changed,read_events,append_event,'x'),'baseline changed')
# Forks are retained and reported instead of silently choosing one writer.
e <- read_events()[[1]];e$id <- 'conflicting-id';append_event(e)
expect_error(new_review_session(baseline,read_events,append_event,'x'),'Conflicting')
unlink(file.path(folder,'conflicting-id.json'))
# A failing write must not advance the UI. Use an injected backend and fresh journal.
folder <- tempfile();dir.create(folder)
fail <- FALSE;checkpoint_calls <- 0L;prepared_calls <- 0L;merge_calls <- 0L;connect_calls <- 0L;progress_calls <- 0L
list_calls <- 0L;create_calls <- 0L;connected_branches <- character()
mock_branches <- c('main','review-test')
backend <- list(
  list_branches=function(settings) {list_calls <<- list_calls+1L;mock_branches},
  create_branch=function(settings,name) {
    create_calls <<- create_calls+1L;mock_branches <<- c(mock_branches,name)
    list(created=TRUE)
  },
  connect=function(settings,progress=function(value,message,detail=NULL) invisible(NULL)) {
    connect_calls <<- connect_calls+1L;connected_branches <<- c(connected_branches,settings$branch)
    progress_calls <<- progress_calls+1L;progress(0.5,'Loading test workspace','Testing progress reporting…')
    x <- new_review_session(baseline,read_events,function(e) {if(fail) stop('Network unavailable');append_event(e)},settings$reviewer,settings$branch=='main')
    c(x,list(branches=mock_branches,settings=settings,journal_events=length(read_events()),
      create_checkpoint=function(state,head,progress=NULL) {
        checkpoint_calls <<- checkpoint_calls+1L
        if(is.function(progress)) progress(1,'Checkpoint verified','Test checkpoint complete.')
        list(id='checkpoint-test',state=state,head='baseline',changes=c(clusters_renamed=1L))
      },
      prepare_publication=function(state,head,note,progress=NULL) {
        prepared_calls <<- prepared_calls+1L
        if(is.function(progress)) progress(1,'Publication prepared','Test publication complete.')
        list(id='publish-test',changes=c(clusters_renamed=1L))
      },
      merge_publish=function(prepared,progress=NULL) {merge_calls <<- merge_calls+1L;list(main_head='head-2',publication_id=prepared$id)}))
  }
)
Sys.unsetenv('NOVA_REVIEW_MODE')
live_test_server <- app_server
formals(live_test_server) <- formals(app_server)[1:3]
environment(live_test_server) <- list2env(list(demo=FALSE,backend=backend),parent=environment(app_server))
testServer(live_test_server, {
  session$flushReact()
  stopifnot(!connected(),connect_calls==0L,list_calls>=1L)
  session$setInputs(reviewer='A',branch_name='review-created',create_branch=1)
  stopifnot(connected(),nrow(state()$questions)==4,create_calls==1L,connect_calls==1L,progress_calls==1L,
    identical(connected_branches,'review-created'),active()=='review-created')
  # testServer does not apply updateSelectizeInput messages back to input.
  session$setInputs(branch='review-created')
  # Editor changes are local and reactive until the reviewer explicitly saves.
  session$setInputs(editor_select=2L,editor_label='Daily living',editor_rename_save=1)
  stopifnot(node_label(state(),2L)=='Daily living',length(read_events())==0L,length(draft_commands())==1L)
  session$setInputs(editor_save=1)
  saved <- read_events()
  stopifnot(length(saved)==1L,saved[[1]]$command$type=='edit_batch',length(draft_commands())==0L)
  fail <<- TRUE
  session$setInputs(approve=1)
  stopifnot(locked(),length(state()$reviewed)==0,length(read_events())==1L)
  fail <<- FALSE
  session$setInputs(connect=2,approve=2)
  stopifnot(length(state()$reviewed)==1,!locked(),length(read_events())==2L)
  session$setInputs(connect=3)
  stopifnot(length(state()$reviewed)==1)
  session$setInputs(editor_select=2L,editor_label='Life areas',editor_rename_save=2)
  session$setInputs(create_checkpoint=1)
  stopifnot(checkpoint_calls==0L,is.null(prepared()))
  session$setInputs(editor_save=2,prepare_publication=1)
  stopifnot(prepared_calls==0L)
  session$setInputs(create_checkpoint=2)
  stopifnot(checkpoint_calls==1L,!checkpoint_dirty(),!locked())
  session$setInputs(prepare_publication=2)
  stopifnot(prepared_calls==1L,!is.null(prepared()),locked())
  session$setInputs(merge_publish=1)
  stopifnot(merge_calls==1L,active()=='main',connected(),is.null(prepared()))
  session$setInputs(reset=1);stopifnot(length(state()$reviewed)==1)
})
stopifnot(inherits(shinyApp(app_ui(FALSE),app_server),'shiny.appobj'))
live_html <- as.character(app_ui(FALSE))
stopifnot(grepl('nova-survey-baseline-20260916',live_html,fixed=TRUE),
  grepl('survey-baseline-v1',live_html,fixed=TRUE),
  !grepl('id="url"',live_html,fixed=TRUE),!grepl('id="ledger"',live_html,fixed=TRUE),
  !grepl('id="run_id"',live_html,fixed=TRUE))
cat('PASS: lightweight branch discovery, direct branch creation, one-load workspace selection, browser-local editor drafts, explicit batched save, publication dirty guard, real data mapping, canonical projection, saved fit, provenance, JSON restart, rename/collapse/new leaf/unresolved persistence, stale writers, fork detection, baseline change, read-only, failed-save UI, prepare/merge handoff, live construction.\n')
