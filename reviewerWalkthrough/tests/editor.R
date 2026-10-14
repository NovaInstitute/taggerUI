args <- commandArgs(FALSE);path <- sub('^--file=','',args[grepl('^--file=',args)])
setwd(normalizePath(file.path(dirname(path),'..')))
tagger_paths <- c('../../novaTagger','../../semanticTagger')
tagger_path <- tagger_paths[dir.exists(tagger_paths)][1L]
if(is.na(tagger_path)) stop('Could not find a sibling novaTagger or semanticTagger checkout.')
pkgload::load_all(tagger_path,quiet=TRUE)
library(shiny)
source('R/model.R');source('R/editor.R');source('R/ui.R');source('R/server.R');source('R/persistence.R')
edit <- novaTagger::edit_review_tree
bad <- function(x) stopifnot(inherits(tryCatch({force(x);NULL},error=identity),'error'))
s <- mock_state()
# Automatic cleanup merges duplicate siblings deterministically and is idempotent.
clean <- edit(s,list(action='deduplicate'))
stopifnot(!21L %in% clean$nodes$id,all(clean$questions$leaf[clean$questions$id %in% 13:18]==20L),
  !anyDuplicated(tolower(trimws(clean$nodes$label[clean$nodes$parent==10L]))))
clean_again <- edit(clean,list(action='deduplicate'))
stopifnot(identical(clean_again,clean))
# A same-labelled intermediate child can be collapsed without changing membership.
repeated <- s;repeated$nodes$label[repeated$nodes$id==2L] <- repeated$nodes$label[repeated$nodes$id==1L]
collapsed <- edit(repeated,list(action='deduplicate'))
stopifnot(!2L %in% collapsed$nodes$id,all(collapsed$nodes$parent[collapsed$nodes$id %in% c(4L,5L)]==1L),
  identical(collapsed$questions$leaf[!collapsed$questions$id %in% 13:18],repeated$questions$leaf[!repeated$questions$id %in% 13:18]))
bad(edit(s,list(action='move',target=2,destination=16)))
bad(edit(s,list(action='move',target=16,destination=17)))
x <- edit(s,list(action='move',target=16,destination=9))
stopifnot(x$nodes$parent[x$nodes$id==16]==9,identical(x$questions,s$questions))
y <- edit(x,list(action='undo')); stopifnot(identical(y$nodes,s$nodes),identical(y$questions,s$questions))
z <- edit(y,list(action='redo'));stopifnot(identical(z$nodes,x$nodes))
x <- edit(s,list(action='remove_level',target=2));stopifnot(all(x$nodes$parent[x$nodes$id %in% c(4,5)]==1),identical(x$questions,s$questions))
x <- edit(s,list(action='remove_level',target=16));stopifnot(all(x$questions$leaf[1:3]==1),all(1:3 %in% queue_ids(x)))
x <- edit(s,list(action='merge',target=16,destination=17));stopifnot(all(x$questions$leaf[1:6]==17),!16 %in% x$nodes$id)
x <- edit(s,list(action='merge',target=16,destination=9));stopifnot(all(x$questions$leaf[1:3]==1),!16 %in% x$nodes$id)
bad(edit(s,list(action='rename',target=16,label='Alcohol use')))
bad(edit(s,list(action='remove_level',target=1)))
x <- edit(s,list(action='add',target=1,label='New domain'));stopifnot(nrow(x$nodes)==32)
# Durable replay, undo, redo and stale-session protection use the same command journal.
events <- list();read <- function()events;append <- function(e)events[[length(events)+1L]] <<- e
session <- new_review_session(s,read,append,'test')
r <- session$save(list(type='edit',action='move',target=16,destination=9),session$head)
a <- new_review_session(s,read,append,'test');stopifnot(identical(a$state$nodes,r$state$nodes))
r <- a$save(list(type='edit',action='undo'),a$head)
b <- new_review_session(s,read,append,'test');stopifnot(identical(b$state$nodes,s$nodes))
r <- b$save(list(type='edit',action='redo'),b$head)
stopifnot(r$state$nodes$parent[r$state$nodes$id==16]==9)
bad(session$save(list(type='edit',action='rename',target=2,label='X'),session$head))
# A set of browser-local edits is durable as one journal command.
events <- list();session <- new_review_session(s,function()events,function(e)events[[length(events)+1L]] <<- e,'batch')
r <- session$save(list(type='edit_batch',commands=list(
  list(type='edit',action='deduplicate'),
  list(type='edit',action='rename',target=16L,label='Smoking'))),session$head)
stopifnot(length(events)==1L,events[[1]]$command$type=='edit_batch',node_label(r$state,16L)=='Smoking',!21L %in% r$state$nodes$id)
# Actual baseline retains all question identities through edits and undo.
if(nzchar(Sys.getenv('EDITOR_BASELINE'))) {
 real <- readRDS(Sys.getenv('EDITOR_BASELINE'))
 x <- edit(real,list(action='remove_level',target=4))
 stopifnot(identical(x$questions,real$questions),identical(edit(x,list(action='undo'))$nodes,real$nodes))
}
Sys.setenv(NOVA_REVIEW_MODE='demo')
testServer(app_server,{
 session$flushReact();session$setInputs(editor_select=16)
 session$setInputs(editor_label='Smoking',editor_rename_save=1)
 stopifnot(node_label(state(),16)=='Smoking')
 stopifnot(length(draft_commands())>=2L)
 session$setInputs(editor_undo=1);stopifnot(node_label(state(),16)=='Tobacco use')
 session$setInputs(editor_redo=1);stopifnot(node_label(state(),16)=='Smoking')
 session$setInputs(editor_drop=list(source='16',destination='9'))
 stopifnot(state()$nodes$parent[state()$nodes$id==16]==8)
 session$setInputs(editor_confirm=1)
 stopifnot(state()$nodes$parent[state()$nodes$id==16]==9)
 session$setInputs(editor_search='Smoking');stopifnot(grepl('Smoking',output$editor_tree$html))
 # Selecting a tree category drives the bottom question browser automatically.
 session$flushReact()
 stopifnot(grepl('currently smoke',output$editor_inspect_questions$html,ignore.case=TRUE),
   grepl('of 3',output$editor_inspect_status,fixed=TRUE))
 session$setInputs(editor_inspect_search='tried to stop');session$flushReact()
 stopifnot(grepl('of 1',output$editor_inspect_status,fixed=TRUE),
   grepl('tried to stop',output$editor_inspect_questions$html,ignore.case=TRUE))
})
cat('PASS: automatic duplicate cleanup, direct edits, cycles, sibling collisions, pending questions, stable membership, undo/redo, batched journal restart, stale sessions, real baseline and editor handlers.\n')
