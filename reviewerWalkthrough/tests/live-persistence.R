# Explicit live acceptance test. Creates a uniquely named test ledger, retained for inspection.
args <- commandArgs(FALSE);path <- sub('^--file=','',args[grepl('^--file=',args)])
setwd(normalizePath(file.path(dirname(path),'..')))
source('R/model.R');source('R/server.R');source('R/persistence.R')
mode <- commandArgs(TRUE)
info_path <- Sys.getenv('NOVA_REVIEW_TEST_INFO','/tmp/nova-review-live-test.rds')
if(identical(mode,'resume')) {
  info <- readRDS(info_path)
} else {
  info <- list(ledger=paste0('nova-reviewer-test-',format(Sys.time(),'%Y%m%dT%H%M%S')),branch='review-test',graph='https://data.nova.org/tests/reviewer-journal')
}
config <- novaRush::setConfig(baseUrl=Sys.getenv('FLUREE_BASE_URL','http://localhost:8090'),ledger=info$ledger,branch='main',timeout=300)
if(!identical(mode,'resume')) {
  novaRush::createLedger(config)
  novaRush::upsertNamedGraph(list('@id'='https://data.nova.org/tests/seed','https://schema.org/name'='Reviewer persistence test'), 'https://data.nova.org/tests/seed-graph',config,branch='main')
  novaRush::createBranch(config,branch=info$branch,from='main')
  saveRDS(info,info_path)
}
baseline <- mock_state()
journal <- fluree_event_journal(config,info$graph,info$branch)
x <- new_review_session(baseline,journal$read,journal$append,'integration-test')
if(identical(mode,'resume')) {
  stopifnot(length(x$state$reviewed)==1,node_label(x$state,2L)=='Verified health')
  main <- fluree_event_journal(config,info$graph,'main')
  stopifnot(length(main$read())==0L)
  cat('PASS: separate R process restored saved rename; main unchanged.\n')
} else {
  saved <- x$save(list(type='review',target=1L,action='rename',child=2L,label='Verified health'),x$head)
  stopifnot(length(saved$state$reviewed)==1L)
  cat('PASS: Fluree write and read-back on isolated test branch.\n')
}
cat('Test ledger retained:',info$ledger,'\n')
