# Reviewer decisions are an append-only journal in a dedicated Fluree graph.
# The pipeline run is the immutable input; replay produces the reviewer draft.
review_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox=TRUE, null='null', na='null', digits=NA))
review_hash <- function(x) digest::digest(review_json(x), algo='sha256', serialize=FALSE)
empty_review_state <- function() list(
  nodes=data.frame(id=1L,parent=NA_integer_,label='Survey questions',stable_id='root'),
  questions=data.frame(id=character(),text=character(),leaf=integer(),fit=character(),suggested=integer(),reason=character()),
  reviewed=integer(),history=data.frame(action=character(),detail=character()),
  deferred=integer(),affected=character(),resolved=character(),unresolved=character(),placements=data.frame(id=character(),decision=character()),new_count=0L)

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
  s$nodes <- rbind(s$nodes,data.frame(id=ids,parent=parents,label=as.character(c$tag),stable_id=key))
  s$nodes$source_key <- s$nodes$stable_id
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
  if(command$type=='edit') return(novaTagger::edit_review_tree(s,command))
  if(command$type=='edit_batch') {
    commands <- command$commands
    if(is.data.frame(commands)) commands <- lapply(seq_len(nrow(commands)),function(i) as.list(commands[i,,drop=FALSE]))
    if(!is.list(commands) || !length(commands)) stop('A saved edit batch must contain at least one taxonomy edit.')
    for(item in commands) {
      if(!is.list(item) || !identical(item$type,'edit')) stop('A saved edit batch may only contain taxonomy edits.')
      s <- novaTagger::edit_review_tree(s,item)
    }
    return(s)
  }
  # Legacy edits invalidate editor undo rather than undoing unrelated placement work.
  s$editor_undo <- NULL; s$editor_redo <- NULL
  if(command$type=='review') {
    if(!identical(as.integer(current_split(s)),as.integer(command$target))) stop('This split has changed. Reconnect to reload the saved draft.')
    return(review_split(s,command$action,command$child,command$label %||% ''))
  }
  if(command$type=='place') {
    if(!identical(as.character(queue_item(s)),as.character(command$target))) stop('This question has changed. Reconnect to reload the saved draft.')
    return(place_question(s,command$decision,command$destination,command$name %||% '',command$parent,command$rationale %||% '',command$node_key %||% NULL))
  }
  stop('Unrecognised saved review action.')
}

review_state_to_taxonomy <- function(s,run_id,revision) {
  if(!'stable_id' %in% names(s$nodes)) stop('The review state does not contain stable taxonomy identifiers.')
  stable <- setNames(as.character(s$nodes$stable_id),as.character(s$nodes$id))
  if(anyNA(stable)||any(!nzchar(stable))||anyDuplicated(stable)) stop('The review state contains invalid stable taxonomy identifiers.')
  parents <- rep(NA_character_,nrow(s$nodes)); has_parent <- !is.na(s$nodes$parent)
  parents[has_parent] <- unname(stable[as.character(s$nodes$parent[has_parent])])
  leaves <- leaf_ids(s)
  assigned <- s$questions$leaf %in% leaves
  taxonomy <- novaTagger::new_reviewed_taxonomy(
    nodes=data.frame(id=as.character(s$nodes$stable_id),parent_id=parents,label=as.character(s$nodes$label)),
    assignments=data.frame(question_id=as.character(s$questions$id[assigned]),leaf_id=unname(stable[as.character(s$questions$leaf[assigned])])),
    run_id=run_id,revision=revision,
    progress=list(
      reviewed_splits=unname(stable[as.character(intersect(s$reviewed,s$nodes$id))]),
      deferred_splits=unname(stable[as.character(intersect(s$deferred %||% integer(),s$nodes$id))]),
      outstanding_questions=as.character(queue_ids(s))
    )
  )
  taxonomy
}

review_state_from_taxonomy <- function(taxonomy,questions) {
  novaTagger::validate_reviewed_taxonomy(taxonomy)
  q <- as.data.frame(questions)
  if(!all(c('id','caption') %in% names(q))) stop('Published taxonomy questions require id and caption fields.')
  root <- taxonomy$nodes$id[is.na(taxonomy$nodes$parent_id)]
  walk <- function(id) {
    kids <- taxonomy$nodes$id[!is.na(taxonomy$nodes$parent_id) & taxonomy$nodes$parent_id==id]
    c(id,unlist(lapply(sort(kids),walk),use.names=FALSE))
  }
  stable_order <- walk(root)
  numeric_id <- setNames(seq_along(stable_order),stable_order)
  node_rows <- match(stable_order,taxonomy$nodes$id)
  parent_stable <- taxonomy$nodes$parent_id[node_rows]
  nodes <- data.frame(
    id=unname(numeric_id[stable_order]),
    parent=as.integer(unname(numeric_id[parent_stable])),
    label=taxonomy$nodes$label[node_rows],stable_id=stable_order,
    source_key=stable_order,stringsAsFactors=FALSE
  )
  q <- q[order(as.character(q$id)),,drop=FALSE]
  assignment <- setNames(taxonomy$assignments$leaf_id,taxonomy$assignments$question_id)
  leaf_stable <- unname(assignment[as.character(q$id)])
  leaf <- as.integer(unname(numeric_id[leaf_stable])); leaf[is.na(leaf)] <- numeric_id[[root]]
  fit <- rep('amber',nrow(q)); reason <- rep('Published assignment has not been reassessed.',nrow(q))
  outstanding <- as.character(taxonomy$progress$outstanding_questions %||% character())
  pending <- as.character(q$id) %in% outstanding | is.na(leaf_stable)
  fit[pending] <- 'red'; reason[pending] <- ifelse(is.na(leaf_stable[pending]),
    'This question has no existing leaf placement.','This published question still requires placement review.')
  s <- empty_review_state();s$nodes <- nodes
  s$questions <- data.frame(id=as.character(q$id),text=as.character(q$caption),leaf=leaf,
    fit=fit,suggested=NA_integer_,reason=reason,stringsAsFactors=FALSE)
  for(col in intersect(c('procedure_id','source_form_id','element_order','iri'),names(q))) s$questions[[col]] <- as.character(q[[col]])
  completed_splits <- setdiff(taxonomy$progress$reviewed_splits,
                              taxonomy$progress$deferred_splits)
  s$reviewed <- as.integer(unname(numeric_id[completed_splits]))
  s$reviewed <- s$reviewed[!is.na(s$reviewed)]
  s$deferred <- as.integer(unname(numeric_id[taxonomy$progress$deferred_splits]))
  s$deferred <- s$deferred[!is.na(s$deferred)]
  s$source <- list(run_id=taxonomy$run_id,revision=taxonomy$revision)
  s
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
new_review_session <- function(baseline,read_events,append_event,reviewer,read_only=FALSE,base_main_head=NULL) {
  events <- read_events(); loaded <- replay_review(baseline,events)
  save <- function(command,expected_head) {
    if(read_only) stop('The published baseline is read only. Create a review branch.')
    current <- replay_review(baseline,read_events())
    if(!identical(current$head,expected_head)) stop('Another session saved a decision. Reconnect to load it before continuing.')
    candidate <- apply_review_command(current$state,command)
    event <- list(schema=1L,parent=expected_head,source=current$signature,reviewer=reviewer,base_main_head=base_main_head,
      saved_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),command=command)
    event$id <- review_hash(list(event,nonce=paste(Sys.getpid(),runif(1))))
    # A write failure may mean an uncertain network outcome. The UI locks until reload.
    append_event(event)
    verified <- replay_review(baseline,read_events())
    if(!identical(verified$head,event$id)) stop('Save could not be verified yet. Reconnect before making another decision.')
    list(state=verified$state,head=verified$head)
  }
  list(state=loaded$state,head=loaded$head,save=save,base_main_head=base_main_head)
}

review_dataset_settings <- function(branch='main',reviewer='') list(
  url='http://localhost:8090',ledger='nova-survey-baseline-20260916',
  run_id='survey-baseline-v1',branch=as.character(branch),reviewer=trimws(as.character(reviewer)),
  survey_graph='',tagging_base=''
)

fluree_review_backend <- function() {
  require_dependencies <- function() {
    for(p in c('novaRush','novaTagger','jsonlite','digest')) if(!requireNamespace(p,quietly=TRUE)) stop('Install the project dependency: ',p)
  }
  config_for <- function(settings,branch=settings$branch) novaRush::setConfig(
    baseUrl=settings$url,ledger=settings$ledger,branch=branch,
    apiKey=if(nzchar(Sys.getenv('FLUREE_API_KEY'))) Sys.getenv('FLUREE_API_KEY') else NULL,timeout=300)
  locations_for <- function(settings) {
    scope <- paste0('https://data.nova.org/integration/',settings$ledger,'/')
    survey <- if(nzchar(settings$survey_graph)) settings$survey_graph else paste0(scope,'graph/survey')
    base <- if(nzchar(settings$tagging_base)) settings$tagging_base else paste0(scope,'graph/tagging/openai/',settings$run_id,'/')
    if(!endsWith(base,'/')) base <- paste0(base,'/')
    list(survey=survey,base=base)
  }
  list_branches <- function(settings=review_dataset_settings()) {
    require_dependencies()
    records <- novaRush::listBranches(config_for(settings,'main'))
    names <- unique(vapply(records,function(b) as.character(b$branch),character(1)))
    c(intersect('main',names),sort(setdiff(names,'main')))
  }
  create_branch <- function(settings,name) {
    require_dependencies()
    if(!nzchar(settings$reviewer)) stop('Enter your reviewer name before creating a branch.')
    config <- config_for(settings,'main')
    base_head <- novaRush::branchHead(config,'main')
    result <- novaRush::createBranch(config,branch=name,from='main')
    if(isTRUE(result$exists)) return(result)
    base <- locations_for(settings)$base
    branch_graph <- paste0(base,'reviewer/',utils::URLencode(name,reserved=TRUE))
    fluree_event_journal(config,branch_graph,name)$write_metadata(base_head,settings$reviewer)
    result
  }
  connect <- function(settings) {
    require_dependencies()
    if(!nzchar(settings$reviewer)) stop('Enter your reviewer name.')
    config <- novaRush::setConfig(baseUrl=settings$url,ledger=settings$ledger,branch=settings$branch,
      apiKey=if(nzchar(Sys.getenv('FLUREE_API_KEY'))) Sys.getenv('FLUREE_API_KEY') else NULL,timeout=300)
    names <- list_branches(settings)
    if(!settings$branch %in% names) stop('That branch no longer exists. Refresh the branch list and choose another workspace.')
    locations <- locations_for(settings);survey <- locations$survey;base <- locations$base
    questions <- novaTagger::query_taggable_questions(config,graph=survey,branch=settings$branch,page_size=500L)
    graphs <- setNames(as.list(paste0(base,c('run','embedding','hierarchy','review'))),c('run','embedding','hierarchy','review'))
    repository <- novaTagger::novarush_semantic_repository(config,
      graphs=graphs,
      branch=settings$branch,query_page_size=500L)
    scope_iri <- novaTagger::tagging_entity_iri('run',settings$run_id)
    semantic_records <- repository$load(scope_iri)
    is_published <- any(vapply(semantic_records$run,function(node) !is.null(node[['https://data.nova.org/vocabulary/tagging/taxonomyChecksum']]),logical(1)))
    published_taxonomy <- NULL
    if(is_published) {
      taxonomy <- novaTagger::reviewed_taxonomy_from_records(semantic_records)
      published_taxonomy <- taxonomy
      baseline <- review_state_from_taxonomy(taxonomy,questions)
    } else {
      store <- novaTagger::semantic_tag_store(repository,questions,settings$run_id)
      raw <- novaTagger::tag_store_load(store)
      baseline <- review_from_pipeline(raw)
    }
    graph <- paste0(base,'reviewer/',utils::URLencode(settings$branch,reserved=TRUE))
    journal <- fluree_event_journal(config,graph,settings$branch)
    read_events <- journal$read;append_event <- journal$append
    existing_events <- read_events()
    metadata <- journal$read_metadata()
    branch_publications <- Filter(function(node) identical(node[['@type']],'https://data.nova.org/vocabulary/tagging/TaxonomyPublication') && identical(as.character(node[['https://data.nova.org/vocabulary/tagging/sourceBranch']] %||% ''),settings$branch),semantic_records$review)
    prepared_existing <- NULL
    if(settings$branch!='main' && length(branch_publications) && !is.null(published_taxonomy)) {
      publication <- branch_publications[[length(branch_publications)]]
      changes_value <- publication[['https://data.nova.org/vocabulary/tagging/changes']]
      changes <- if(is.list(changes_value)&&'@value' %in% names(changes_value)) unlist(changes_value[['@value']]) else integer()
      prepared_existing <- list(
        id=as.character(publication[['https://schema.org/identifier']]),taxonomy=published_taxonomy,
        publication=publication,changes=changes,
        base_main_head=as.character(publication[['https://data.nova.org/vocabulary/tagging/baseMainHead']])
      )
    }
    recorded_heads <- unique(vapply(existing_events,function(e) as.character(e$base_main_head %||% NA_character_),character(1)))
    recorded_heads <- recorded_heads[!is.na(recorded_heads)&nzchar(recorded_heads)]
    main_head <- novaRush::branchHead(config,'main')
    base_main_head <- if(settings$branch=='main') main_head else if(!is.null(prepared_existing)) prepared_existing$base_main_head else if(!is.null(metadata$base_main_head)) metadata$base_main_head else if(length(recorded_heads)==1L) recorded_heads else NA_character_
    session <- new_review_session(baseline,read_events,append_event,settings$reviewer,settings$branch=='main'||!is.null(prepared_existing),base_main_head)
    taxonomy_repository <- novaTagger::novarush_reviewed_taxonomy_repository(
      config,graphs,branch=settings$branch,query_page_size=500L)
    prepare_publish <- function(review_state,event_head,note='') {
      if(settings$branch=='main') stop('Main is already the published branch.')
      if(is.na(base_main_head)||!nzchar(base_main_head)) stop('This legacy review branch does not record its base main commit. Create a fresh branch from main before publishing.')
      current <- replay_review(baseline,read_events())
      if(!identical(current$head,event_head)) stop('The review changed before publication. Reconnect and try again.')
      before <- review_state_to_taxonomy(baseline,settings$run_id,baseline$source$revision)
      after <- review_state_to_taxonomy(current$state,settings$run_id,baseline$source$revision+1L)
      changes <- novaTagger::reviewed_taxonomy_diff(before,after)
      publication_id <- paste0('publish-',review_hash(list(branch=settings$branch,head=event_head,taxonomy=after$taxonomy_checksum,assignments=after$assignment_checksum)))
      publication <- novaTagger::taxonomy_publication_record(after,publication_id,settings$reviewer,settings$branch,base_main_head,changes,note)
      taxonomy_repository$replace(after,publication)
      journal$clear()
      list(id=publication_id,taxonomy=after,publication=publication,changes=changes,base_main_head=base_main_head)
    }
    merge_publish <- function(prepared) {
      novaRush::mergeBranch(config,settings$branch,'main',prepared$base_main_head)
      main_repository <- novaTagger::novarush_reviewed_taxonomy_repository(config,graphs,branch='main',query_page_size=500L)
      verified <- main_repository$load(settings$run_id)
      ids <- vapply(verified$review,function(node) as.character(node[['https://schema.org/identifier']] %||% ''),character(1))
      published <- novaTagger::reviewed_taxonomy_from_records(verified)
      if(!prepared$id %in% ids || !identical(published$taxonomy_checksum,prepared$taxonomy$taxonomy_checksum) || !identical(published$assignment_checksum,prepared$taxonomy$assignment_checksum)) stop('The merge returned but main could not be verified. Reconnect before retrying.')
      list(main_head=novaRush::branchHead(config,'main'),publication_id=prepared$id)
    }
    c(session,list(branches=names,settings=settings,prepared=prepared_existing,
      prepare_publish=prepare_publish,merge_publish=merge_publish))
  }
  list(list_branches=list_branches,create_branch=create_branch,connect=connect)
}

fluree_event_journal <- function(config,graph,branch) {
    property <- 'https://data.nova.org/vocabulary/reviewer/eventPayload'
    base_property <- 'https://data.nova.org/vocabulary/reviewer/baseMainHead'
    reviewer_property <- 'https://data.nova.org/vocabulary/reviewer/createdBy'
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

    read_metadata <- function() {
      rows <- tryCatch(novaRush::queryNamedGraph(list(select=list('?head','?reviewer'),
        where=list(setNames(list(paste0(graph,'/workspace'),'?head','?reviewer'),c('@id',base_property,reviewer_property))),limit=1L),
        graph,config,branch=branch),error=function(e) {if(grepl('Unknown named graph',conditionMessage(e),fixed=TRUE)) return(list());stop(e)})
      if(is.data.frame(rows)) rows <- lapply(seq_len(nrow(rows)),function(i) as.list(rows[i,,drop=FALSE]))
      if(!length(rows)) return(list())
      value <- function(x) {if(is.list(x)&&!is.null(x[['@value']])) x[['@value']] else x}
      list(base_main_head=as.character(value(rows[[1]][[1]])),reviewer=as.character(value(rows[[1]][[2]])))
    }
    write_metadata <- function(base_main_head,reviewer) {
      node <- list('@id'=paste0(graph,'/workspace'),'@type'='https://data.nova.org/vocabulary/reviewer/Workspace')
      node[[base_property]] <- as.character(base_main_head);node[[reviewer_property]] <- as.character(reviewer)
      novaRush::upsertNamedGraph(node,graph,config,branch=branch)
    }

    clear <- function() {
      events <- read_events()
      nodes <- lapply(events,function(e) {
        node <- list('@id'=paste0(graph,'/event/',e$id),'@type'='https://data.nova.org/vocabulary/reviewer/Decision')
        node[[property]] <- review_json(e);node
      })
      metadata <- read_metadata()
      if(length(metadata)) {
        node <- list('@id'=paste0(graph,'/workspace'),'@type'='https://data.nova.org/vocabulary/reviewer/Workspace')
        node[[base_property]] <- metadata$base_main_head;node[[reviewer_property]] <- metadata$reviewer
        nodes[[length(nodes)+1L]] <- node
      }
      if(length(nodes)) novaRush::updateNamedGraph(delete=nodes,graph=graph,config=config,branch=branch)
      invisible(TRUE)
    }

  list(read=read_events,append=append_event,read_metadata=read_metadata,write_metadata=write_metadata,clear=clear)
}
