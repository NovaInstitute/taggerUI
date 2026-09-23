# Reviewer decisions are an append-only journal in a dedicated Fluree graph.
# The pipeline run is the immutable input; replay produces the reviewer draft.
review_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox=TRUE, null='null', na='null', digits=NA))
review_hash <- function(x) digest::digest(review_json(x), algo='sha256', serialize=FALSE)
empty_review_state <- function() list(
  nodes=data.frame(id=1L,parent=NA_integer_,label='Survey questions'),
  questions=data.frame(id=character(),text=character(),leaf=integer(),fit=character(),suggested=integer(),reason=character()),
  reviewed=integer(),history=data.frame(action=character(),detail=character()),
  affected=character(),resolved=character(),unresolved=character(),placements=data.frame(id=character(),decision=character()),new_count=0L)

review_from_pipeline <- function(raw) {
  s <- empty_review_state(); c <- as.data.frame(raw$clusters); q <- as.data.frame(raw$questions)
  if(!nrow(q) || !nrow(c)) stop('The selected run has no questions or taxonomy.')
  if(anyDuplicated(q$id)) stop('The source contains duplicate question identifiers.')
  q <- q[order(as.character(q$id)),,drop=FALSE]
  c <- c[order(-c$level,c$cluster_id),,drop=FALSE]
  key <- paste(c$level,c$cluster_id,sep=':')
  if(anyDuplicated(key)) stop('The source contains duplicate taxonomy identifiers.')
  if(any(is.na(c$tag)|!nzchar(trimws(c$tag))|c$tag=='untagged')) stop('The selected run still has untagged groups. Complete tagging before review.')
  ids <- seq_len(nrow(c))+1L
  parents <- match(paste(c$level+1L,c$parent_cluster,sep=':'),key)+1L
  if(any(!is.na(c$parent_cluster) & is.na(parents))) stop('The stored taxonomy contains missing parent groups.')
  parents[is.na(c$parent_cluster)] <- 1L
  s$nodes <- rbind(s$nodes,data.frame(id=ids,parent=parents,label=as.character(c$tag)))
  s$nodes$source_key <- c('root',key)
  a <- raw$assignments; memberships <- a$cluster_level_1[match(q$id,a$id)]
  loc <- match(paste(1L,memberships,sep=':'),key)+1L
  # Preserve outliers/unassigned questions in the queue, not a fabricated leaf.
  loc[is.na(loc)] <- 1L
  s$questions <- data.frame(id=as.character(q$id),text=as.character(q$caption),leaf=loc,
    fit='amber',suggested=NA_integer_,reason='No saved fit assessment is available for this question.',stringsAsFactors=FALSE)
  # Use persisted label-fit assessments; never invent demo colours or call a model.
  events <- raw$review_events
  if(length(events)) {
    times <- vapply(events,function(e) as.character(e$created_at),character(1))
    events <- events[order(times)]
    for(e in events) {
      if(is.null(e$level)||e$level!=1L||!e$decision %in% c('accepted','edited')) next
      scores <- e$similarity$questions
      if(!is.data.frame(scores)) next
      id_column <- intersect(c('question_id','id'),names(scores))
      if(!length(id_column)) next
      column <- intersect(c('after_similarity','cosine_similarity'),names(scores))
      if(!length(column)) next
      ix <- match(as.character(scores[[id_column[1]]]),s$questions$id)
      leaf <- match(paste(1L,e$cluster_id,sep=':'),key)+1L
      valid <- !is.na(ix) & !is.na(leaf)
      valid[valid] <- s$questions$leaf[ix[valid]]==leaf
      ix <- ix[valid]; score <- as.numeric(scores[[column[1]]][valid])
      s$questions$fit[ix] <- ifelse(is.na(score),'amber',ifelse(score<0.5,'red',ifelse(score<0.65,'amber','green')))
      s$questions$reason[ix] <- ifelse(is.na(score),'No saved fit assessment is available for this question.',
        ifelse(score<0.5,'The saved assessment indicates this question may not fit its current tag.',''))
    }
  }
  missing <- !s$questions$leaf %in% leaf_ids(s)
  s$questions$fit[missing] <- 'red'
  s$questions$reason[missing] <- 'This question has no existing leaf placement.'
  # Keep provenance for disambiguating repeated questions across forms.
  for(col in intersect(c('procedure_id','source_form_id','element_order','iri'),names(q))) s$questions[[col]] <- as.character(q[[col]])
  # Suggested destinations use existing question vectors in bounded batches.
  # This is local arithmetic, with no embedding/model requests and no N-by-N matrix.
  vectors <- raw$embeddings
  if(is.matrix(vectors) && nrow(vectors)==nrow(raw$questions)) {
    vectors <- vectors[match(q$id,raw$questions$id),,drop=FALSE]
    norms <- sqrt(rowSums(vectors^2)); good <- is.finite(norms)&norms>0
    vectors[good,] <- vectors[good,,drop=FALSE]/norms[good]
    vectors[!good,] <- 0
    leaves <- leaf_ids(s)
    usable <- leaves[vapply(leaves,function(id) any(s$questions$leaf==id & good),logical(1))]
    if(length(usable)>1L) {
      centers <- t(vapply(usable,function(id) colMeans(vectors[s$questions$leaf==id & good,,drop=FALSE]),numeric(ncol(vectors))))
      norms <- sqrt(rowSums(centers^2)); centers <- centers/pmax(norms,1e-12)
      rows <- which(s$questions$fit=='red' & good)
      for(batch in split(rows,ceiling(seq_along(rows)/500L))) {
        sim <- vectors[batch,,drop=FALSE] %*% t(centers)
        own <- match(s$questions$leaf[batch],usable)
        own_score <- rep(-1,length(batch));hit <- which(!is.na(own))
        own_score[hit] <- sim[cbind(hit,own[hit])]
        sim[cbind(hit,own[hit])] <- -Inf
        best <- max.col(sim,ties.method='first')
        better <- sim[cbind(seq_along(batch),best)]>=own_score+0.1
        s$questions$suggested[batch[better]] <- usable[best[better]]
      }
    }
  }
  s$source <- list(run_id=raw$run_id,revision=as.integer(raw$revision))
  s
}

apply_review_command <- function(s,command) {
  if(command$type=='review') {
    if(!identical(as.integer(current_split(s)),as.integer(command$target))) stop('This split has changed. Reconnect to reload the saved draft.')
    return(review_split(s,command$action,command$child,command$label %||% ''))
  }
  if(command$type=='place') {
    if(!identical(as.character(queue_item(s)),as.character(command$target))) stop('This question has changed. Reconnect to reload the saved draft.')
    return(place_question(s,command$decision,command$destination,command$name %||% '',command$parent,command$rationale %||% ''))
  }
  stop('Unrecognised saved review action.')
}
replay_review <- function(baseline,events) {
  signature <- review_hash(baseline); head <- 'baseline'; s <- baseline
  if(length(events)) {
    ids <- vapply(events,`[[`,character(1),'id')
    if(anyDuplicated(ids)) stop('Duplicate review event identifiers.')
    remaining <- events
    while(length(remaining)) {
      ix <- which(vapply(remaining,function(e) identical(e$parent,head),logical(1)))
      if(length(ix)!=1L) stop('Conflicting or incomplete review history. No changes have been overwritten; use a separate review branch and ask the maintainer to reconcile this history.')
      e <- remaining[[ix]]
      if(!identical(e$source,signature)) stop('The pipeline baseline changed since this review started. Create a new review branch for the new baseline.')
      if(!identical(as.integer(e$schema),1L)) stop('Unsupported review history format.')
      s <- apply_review_command(s,e$command);head <- e$id;remaining <- remaining[-ix]
    }
  }
  list(state=s,head=head,signature=signature)
}
new_review_session <- function(baseline,read_events,append_event,reviewer,read_only=FALSE) {
  events <- read_events(); loaded <- replay_review(baseline,events)
  save <- function(command,expected_head) {
    if(read_only) stop('The published baseline is read only. Create a review branch.')
    current <- replay_review(baseline,read_events())
    if(!identical(current$head,expected_head)) stop('Another session saved a decision. Reconnect to load it before continuing.')
    candidate <- apply_review_command(current$state,command)
    event <- list(schema=1L,parent=expected_head,source=current$signature,reviewer=reviewer,
      saved_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),command=command)
    event$id <- review_hash(list(event,nonce=paste(Sys.getpid(),runif(1))))
    # A write failure may mean an uncertain network outcome. The UI locks until reload.
    append_event(event)
    verified <- replay_review(baseline,read_events())
    if(!identical(verified$head,event$id)) stop('Save could not be verified yet. Reconnect before making another decision.')
    list(state=verified$state,head=verified$head)
  }
  list(state=loaded$state,head=loaded$head,save=save)
}

fluree_review_backend <- function() {
  list(connect=function(settings) {
    for(p in c('novaRush','novaTagger','jsonlite','digest')) if(!requireNamespace(p,quietly=TRUE)) stop('Install the project dependency: ',p)
    if(!nzchar(settings$ledger)||!nzchar(settings$run_id)||!nzchar(settings$reviewer)) stop('Enter the ledger, tagging run and reviewer name.')
    config <- novaRush::setConfig(baseUrl=settings$url,ledger=settings$ledger,branch=settings$branch,
      apiKey=if(nzchar(Sys.getenv('FLUREE_API_KEY'))) Sys.getenv('FLUREE_API_KEY') else NULL,timeout=300)
    branches <- novaRush::listBranches(config)
    names <- vapply(branches,function(b) b$branch,character(1))
    if(!settings$branch %in% names) stop('That branch does not exist. Connect to main and create a review branch first.')
    scope <- paste0('https://data.nova.org/integration/',settings$ledger,'/')
    survey <- if(nzchar(settings$survey_graph)) settings$survey_graph else paste0(scope,'graph/survey')
    base <- if(nzchar(settings$tagging_base)) settings$tagging_base else paste0(scope,'graph/tagging/openai/',settings$run_id,'/')
    if(!endsWith(base,'/')) base <- paste0(base,'/')
    questions <- novaTagger::query_taggable_questions(config,graph=survey,branch=settings$branch,page_size=500L)
    repository <- novaTagger::novarush_semantic_repository(config,
      graphs=setNames(as.list(paste0(base,c('run','embedding','hierarchy','review'))),c('run','embedding','hierarchy','review')),
      branch=settings$branch,query_page_size=500L)
    store <- novaTagger::semantic_tag_store(repository,questions,settings$run_id)
    raw <- novaTagger::tag_store_load(store)
    baseline <- review_from_pipeline(raw)
    graph <- paste0(base,'reviewer/',utils::URLencode(settings$branch,reserved=TRUE))
    journal <- fluree_event_journal(config,graph,settings$branch)
    read_events <- journal$read;append_event <- journal$append
    session <- new_review_session(baseline,read_events,append_event,settings$reviewer,settings$branch=='main')
    c(session,list(branches=names,settings=settings,
      create_branch=function(name) novaRush::createBranch(config,branch=name,from='main')))
  })
}

fluree_event_journal <- function(config,graph,branch) {
    property <- 'https://data.nova.org/vocabulary/reviewer/eventPayload'
    read_events <- function() {
      rows <- list();offset <- 0L
      repeat {
        page <- tryCatch(novaRush::queryNamedGraph(list(select=list('?payload'),
          where=list(setNames(list('?event','?payload'),c('@id',property))),
          orderBy=list('?event'),limit=500L,offset=offset),graph,config,branch=branch),
          error=function(e) {if(grepl('Unknown named graph',conditionMessage(e),fixed=TRUE)) return(list());stop(e)})
        if(is.data.frame(page)) page <- lapply(seq_len(nrow(page)),function(i) as.list(page[i,,drop=FALSE]))
        if(!length(page)) break
        rows <- c(rows,lapply(page,function(row) {
          value <- row[[1]];if(is.list(value)&&!is.null(value[['@value']])) value <- value[['@value']]
          jsonlite::fromJSON(value,simplifyVector=TRUE)
        }))
        if(length(page)<500L) break
        offset <- offset+500L
      }
      rows
    }
    append_event <- function(e) {
      node <- list('@id'=paste0(graph,'/event/',e$id),'@type'='https://data.nova.org/vocabulary/reviewer/Decision')
      node[[property]] <- review_json(e)
      novaRush::upsertNamedGraph(node,graph,config,branch=branch)
    }

  list(read=read_events,append=append_event)
}
