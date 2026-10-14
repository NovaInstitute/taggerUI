app_server <- function(input,output,session, demo=identical(Sys.getenv("NOVA_REVIEW_MODE"),"demo"), backend=NULL) {
  if(!demo && is.null(backend)) backend <- fluree_review_backend()
  state <- reactiveVal(if(demo) mock_state() else empty_review_state())
  branches <- reactiveVal(if(demo) c('main','review-alex') else character())
  active <- reactiveVal(if(demo) 'review-alex' else '')
  drafts <- reactiveVal(if(demo) list(main=mock_state(), 'review-alex'=mock_state()) else list())
  connected <- reactiveVal(FALSE)
  live <- reactiveVal(NULL)
  saved_head <- reactiveVal('baseline')
  saved_state <- reactiveVal(if(demo) mock_state() else empty_review_state())
  draft_commands <- reactiveVal(list())
  save_status <- reactiveVal('Choose a branch and load its workspace.')
  branch_status <- reactiveVal(if(demo) 'Branches are ready.' else 'Checking available branches…')
  locked <- reactiveVal(FALSE)
  prepared <- reactiveVal(NULL)
  initialise_draft <- function(persisted,branch) {
    saved_state(persisted);draft_commands(list());state(persisted)
    if(branch=='main') return(invisible(FALSE))
    command <- list(type='edit',action='deduplicate')
    cleaned <- apply_review_command(persisted,command)
    if(identical(cleaned,persisted)) return(invisible(FALSE))
    state(cleaned);draft_commands(list(command))
    invisible(TRUE)
  }
  if(demo) initialise_draft(mock_state(),'review-alex')
  connection_settings <- function(branch=input$branch %||% 'main')
    review_dataset_settings(branch=branch,reviewer=input$reviewer %||% '')
  refresh_branches <- function(selected=input$branch %||% '') {
    if(demo) return(invisible(branches()))
    branch_status('Checking available branches…')
    tryCatch({
      found <- backend$list_branches(connection_settings('main'))
      branches(found)
      choice <- if(selected %in% found) selected else if('main' %in% found) 'main' else found[1]
      updateSelectizeInput(session,'branch',choices=found,selected=choice,server=TRUE)
      branch_status(paste(length(found),'branches available. Choose one to load.'))
      invisible(found)
    },error=function(e) {
      branch_status('Could not read branches. Check that Fluree is running on this server.')
      showNotification(conditionMessage(e),type='error',duration=NULL)
      invisible(NULL)
    })
  }
  load_live <- function(settings) {
    connected(FALSE);locked(TRUE);prepared(NULL);save_status('Loading the saved dataset…')
    result <- withProgress(message='Loading saved questions and review',value=0.2,backend$connect(settings))
    live(result);saved_head(result$head);branches(result$branches);active(settings$branch);prepared(result$prepared %||% NULL)
    cleaned <- if(is.null(result$prepared)) initialise_draft(result$state,settings$branch) else {
      saved_state(result$state);draft_commands(list());state(result$state);FALSE
    }
    connected(TRUE);locked(!is.null(result$prepared))
    save_status(if(!is.null(result$prepared)) paste('Prepared publication restored:',result$prepared$id) else if(cleaned)
      paste(nrow(result$state$questions),'questions loaded. Repeated categories were cleaned up and are ready to save.') else
      paste(nrow(result$state$questions),'questions loaded. Saved decisions restored.'))
    updateSelectizeInput(session,'branch',choices=result$branches,selected=settings$branch)
  }
  commit <- function(command) {
    if(!editable()) return()
    if(demo) return(state(apply_review_command(state(),command)))
    # Validate first so an ordinary input error does not lock the connection.
    candidate <- apply_review_command(state(),command)
    locked(TRUE);save_status('Saving your decision…')
    tryCatch({
      result <- withProgress(message='Saving decision',value=0.5,live()$save(command,saved_head()))
      state(result$state);saved_head(result$head);locked(FALSE)
      save_status(paste('Saved',format(Sys.time(),'%H:%M:%S'), '· safe to close and resume later.'))
    },error=function(e) {
      save_status('Save not confirmed. Reconnect to restore the last saved state before continuing.')
      showNotification(conditionMessage(e),type='error',duration=NULL)
    })
  }
  apply_draft <- function(command) {
    if(!editable()) return(invisible(FALSE))
    candidate <- apply_review_command(state(),command)
    state(candidate);draft_commands(c(draft_commands(),list(command)))
    save_status(paste(length(draft_commands()),'unsaved taxonomy change(s).'))
    invisible(TRUE)
  }
  save_draft <- function() {
    if(!length(draft_commands())) {showNotification('There are no unsaved taxonomy changes.',type='message');return(invisible(FALSE))}
    if(!editable()) return(invisible(FALSE))
    if(demo) {
      saved_state(state());draft_commands(list());save_status('Changes saved in this local session.')
      return(invisible(TRUE))
    }
    command <- list(type='edit_batch',commands=draft_commands())
    locked(TRUE);save_status('Saving taxonomy changes…')
    tryCatch({
      result <- withProgress(message='Saving taxonomy changes',value=0.5,live()$save(command,saved_head()))
      state(result$state);saved_state(result$state);saved_head(result$head);draft_commands(list());locked(FALSE)
      save_status(paste('Saved',format(Sys.time(),'%H:%M:%S'),'· safe to close and resume later.'))
    },error=function(e) {
      save_status('Save not confirmed. Your edits remain visible, but reconnect before making more changes.')
      showNotification(conditionMessage(e),type='error',duration=NULL)
    })
    invisible(TRUE)
  }
  discard_draft <- function() {
    if(!length(draft_commands())) {showNotification('There are no unsaved taxonomy changes.',type='message');return(invisible(FALSE))}
    state(saved_state());draft_commands(list());save_status('Unsaved taxonomy changes discarded.')
    invisible(TRUE)
  }
  editable <- function() {
    if(!demo && (!connected()||locked())) {showNotification('Connect or reload the saved workspace before making a decision.',type='warning');return(FALSE)}
    if(!demo && !identical(connection_settings(),live()$settings)) {showNotification('Connection settings changed. Click Connect before reviewing.',type='warning');return(FALSE)}
    if(active()=='main') {showNotification('Create or connect to a review branch to make changes.',type='warning'); return(FALSE)}
    if(!is.null(prepared())) {showNotification('This branch is prepared for publication. Merge it or reconnect before editing.',type='warning');return(FALSE)}
    TRUE
  }
  switch_branch <- function(name) {
    d <- drafts(); d[[active()]] <- state(); drafts(d); active(name); state(d[[name]])
  }
  attempt <- function(expr) tryCatch(expr,error=function(e) showNotification(conditionMessage(e),type='error'))
  if(!demo) observeEvent(TRUE,refresh_branches(),once=TRUE)
  observeEvent(input$refresh_branches,refresh_branches())
  observeEvent(input$connect, {
    if(!demo) {attempt(load_live(connection_settings()));return()}
    req(input$branch %in% branches()); switch_branch(input$branch); connected(TRUE)
  })
  observeEvent(input$create_branch, {
    name <- trimws(input$branch_name)
    if(!grepl('^[A-Za-z0-9][A-Za-z0-9_-]{1,39}$',name)) {showNotification('Use 2–40 letters, numbers, hyphens or underscores.',type='warning');return()}
    if(name %in% branches()) {showNotification('That branch already exists. Choose another name.',type='warning');return()}
    if(!demo) {
      attempt({
        settings <- connection_settings('main')
        result <- backend$create_branch(settings,name)
        if(isTRUE(result$exists)) stop('That branch already exists. Refresh the list and select it.')
        refresh_branches(name)
        settings$branch <- name
        load_live(settings)
      });return()
    }
    d <- drafts(); d[[name]] <- mock_state(); drafts(d); branches(c(branches(),name)); switch_branch(name);initialise_draft(state(),name)
    updateSelectInput(session,'branch',choices=branches(),selected=name)
  })
  editor_server(input,output,session,state,apply_draft,save_draft,discard_draft,
    function() length(draft_commands()),editable,connected,demo)
  output$connection <- renderText(if(!demo) save_status() else if(connected()) 'Workspace loaded.' else 'Choose a branch and load it.')
  output$branch_status <- renderText(branch_status())
  output$workspace_banner <- renderUI(div(class='workspace-banner',strong(paste('Active workspace:',if(nzchar(active())) active() else 'Not connected')),span(if(active()=='main') 'Published baseline · read only' else if(demo) 'Your local review draft' else save_status())))
  output$branches <- renderUI(tagList(lapply(branches(),function(b) div(class='branch-row',strong(b),span(if(b=='main') 'Published baseline' else 'Review draft'),if(b==active()) span(class='demo-pill','Active')))))
  observeEvent(input$start,updateTabsetPanel(session,'page',selected='summary'))
  observeEvent(input$go_place,updateTabsetPanel(session,'page',selected='placement'))
  observeEvent(input$go_summary,updateTabsetPanel(session,'page',selected='summary'))
  output$review_content <- renderUI({
    if(!demo && !connected()) return(div(class='card',h2('Connect to your dataset'),p('Open Workspace to load the saved taxonomy and questions.')))
    s <- state(); id <- current_split(s)
    if(is.na(id)) return(div(class='card complete',span(class='eyebrow','TAXONOMY PASS COMPLETE'),h2('The first pass is done.'),
      p(paste(length(s$reviewed),'splits reviewed.',sum(s$history$action=='defer'),'flagged for later.')),
      p('Now review the exception questions against your draft taxonomy.'),actionButton('go_place','Continue to question placement →',class='btn-primary')))
    kids <- children(s,id); choices <- setNames(kids,paste(node_label(s,kids),seq_along(kids),sep=' · '))
    tagList(div(class='page-heading',span(class='eyebrow',paste('Split',length(s$reviewed)+1,'of',length(s$reviewed)+length(setdiff(split_order(s),s$reviewed)))),
      p(class='path',node_path(s,id)),h2(paste('Review:',node_label(s,id))),p('Do these child groups make useful, distinct homes for the questions?')),
      lapply(split_warnings(s,id),function(w) div(class='notice',w)),
      div(class='child-grid',lapply(kids,function(k) div(class='card',h3(node_label(s,k)),question_examples(representatives(s,k))))),
      div(class='card actions',actionButton('approve','Split looks right',class='btn-primary'),actionButton('defer','Flag for later'),
        tags$details(tags$summary('Rename child'),selectInput('rename_child','Child to rename',choices),textInput('replacement','Replacement tag'),actionButton('rename','Save rename and continue',class='btn-primary')),
        tags$details(tags$summary('Collapse child into parent'),p('Remove this grouping and bring its contents up one level. Affected questions will need a placement check.'),
          selectInput('collapse_child','Child to collapse',choices),actionButton('collapse','Collapse and continue',class='btn-primary'))))
  })
  observeEvent(input$approve,{if(editable()) attempt(commit(list(type='review',target=current_split(state()),action='approve')))})
  observeEvent(input$defer,{if(editable()) attempt(commit(list(type='review',target=current_split(state()),action='defer')))})
  observeEvent(input$rename,{if(editable()) attempt(commit(list(type='review',target=current_split(state()),action='rename',child=as.integer(input$rename_child),label=input$replacement)))})
  observeEvent(input$collapse,{if(editable()) attempt(commit(list(type='review',target=current_split(state()),action='collapse',child=as.integer(input$collapse_child))))})
  output$history <- renderUI({h <- tail(state()$history,3); if(!nrow(h)) p(class='muted','Your decisions will appear here.') else tags$ul(lapply(seq_len(nrow(h)),function(i) tags$li(paste(switch(h$action[i],approve='Approved',defer='Flagged for later',rename='Renamed',collapse='Collapsed',h$action[i]),'·',h$detail[i]))))})
  output$placement_content <- renderUI({
    req(demo || connected()); s <- state(); id <- queue_item(s)
    before <- if(!is.na(current_split(s))) div(class='notice','Taxonomy review normally comes first. You can explore placement now, but later changes may need another check.')
    if(is.na(id)) return(tagList(before,div(class='card complete',h2('Every queued question has a home.'),p('Review the summary to see your decisions and any deferred taxonomy splits.'),actionButton('go_summary','View summary →',class='btn-primary'))))
    q <- s$questions[s$questions$id==id,]; leaves <- leaf_ids(s); suggestion <- q$suggested
    valid_suggestion <- !is.na(suggestion) && suggestion %in% leaves
    options <- c('Keep in current leaf'='keep','Move to suggested leaf'='suggested','Choose another existing leaf'='other','Create a new leaf'='new','Leave unresolved'='unresolved')
    tagList(before,div(class='page-heading',span(class='eyebrow',paste(length(queue_ids(s)),'questions remaining ·',length(s$unresolved),'left unresolved')),h2('Find the right home')),
      div(class='card question-card',h3(q$text),if(!demo) tags$details(class='question-source',tags$summary('Question source'),p(class='muted',if('source_form_id' %in% names(q)) paste('Form:',utils::URLdecode(sub('^https?://[^/]+/survey/source-form/','',q$source_form_id))) else 'Source form not supplied'),p(class='muted',paste('Reference:',q$id))),p(class='path',paste('Current path:',node_path(s,q$leaf))),
        div(class='notice',paste(if(id %in% s$affected) 'A grouping on this question’s path was collapsed. Please check its placement.' else q$reason)),
        if(id %in% s$unresolved) p('You left this question unresolved. It remains here until you choose a home.'),
        p(strong('Suggested destination: '),if(valid_suggestion) node_path(s,suggestion) else 'No suggestion available; choose an existing leaf or create one.'),
        radioButtons('decision','Choose one decision',options,selected=if(valid_suggestion) 'suggested' else if(q$leaf %in% leaves) 'keep' else 'other'),
        conditionalPanel("input.decision == 'other'",selectInput('destination','Existing leaf',setNames(leaves,vapply(leaves,function(x) node_path(s,x),character(1))))),
        conditionalPanel("input.decision == 'new'",textInput('new_name','New leaf name'),selectInput('new_parent','Parent branch',setNames(setdiff(s$nodes$id,leaves),vapply(setdiff(s$nodes$id,leaves),function(x) node_path(s,x),character(1)))),textAreaInput('rationale','Short rationale',rows=2)),
        uiOutput('destination_examples'),actionButton('place','Save decision and continue',class='btn-primary')))
  })
  output$destination_examples <- renderUI({
    req(demo || connected()); s <- state(); id <- queue_item(s); req(!is.na(id)); q <- s$questions[s$questions$id==id,]; req(input$decision)
    dest <- switch(input$decision,keep=q$leaf,suggested=q$suggested,other=as.integer(input$destination),NA_integer_)
    if(!length(dest)||is.na(dest)||!dest %in% leaf_ids(s)) return(p(class='muted',if(input$decision=='new') 'Your new leaf will start with this question.' else if(input$decision=='unresolved') 'This item will stay in the queue. Other pending questions come first.' else 'This destination is unavailable. Choose another leaf.'))
    div(class='destination',h4(paste('Clear-fit examples ·',node_label(s,dest))),question_examples(representatives(s,dest,TRUE)))
  })
  observeEvent(input$place,{if(editable()) {
    command <- list(type='place',target=queue_item(state()),decision=input$decision,destination=as.integer(input$destination),name=input$new_name %||% '',parent=as.integer(input$new_parent),rationale=input$rationale %||% '')
    if(identical(input$decision,'new')) command$node_key <- paste0('created-',review_hash(list(head=saved_head(),question=command$target,name=trimws(command$name),parent=command$parent)))
    attempt(commit(command))
  }})
  output$summary <- renderUI({
    req(demo || connected()); s <- state(); h <- s$history; p <- s$placements
    counts <- c('Splits reviewed'=length(s$reviewed),'Splits approved'=sum(h$action=='approve'),'Splits deferred'=sum(h$action=='defer'),
      'Renamed tags'=sum(h$action=='rename'),'Collapsed branches'=sum(h$action=='collapse'),'Questions kept'=sum(p$decision=='kept'),
      'Questions moved'=sum(p$decision=='moved'),'Left unresolved'=length(s$unresolved),'New leaves created'=s$new_count,'Remaining queue'=length(queue_ids(s)))
    tagList(div(class='page-heading',h2('Your review, at a glance'),p(if(is.na(current_split(s)) && !length(queue_ids(s)) && !any(h$action=='defer')) 'Review complete. Your draft is ready to discuss.' else 'Draft in progress. Deferred splits and unresolved questions still need attention.')),
      div(class='stats',lapply(seq_along(counts),function(i) div(class='stat',strong(counts[i]),span(names(counts)[i])))))
  })
  output$publish_status <- renderUI({
    if(demo) return(NULL)
    if(!connected()) return(p(class='muted','Connect to a review branch to prepare a publication.'))
    if(is.null(live()$prepare_publish)) return(p(class='muted','Local draft: changes are saved locally. Fluree publication is unavailable in this session.'))
    if(active()=='main') return(p(class='muted','Main is the current published taxonomy. Create a review branch to make the next revision.'))
    if(length(draft_commands())) return(p(class='notice','Save your taxonomy changes before preparing this branch for publication.'))
    value <- prepared()
    if(is.null(value)) return(tagList(
      p('Preparation replaces the canonical taxonomy on this isolated branch, reads it back, and leaves main unchanged.'),
      textAreaInput('publication_note','Publication note',rows=2,placeholder='Optional summary for the next reviewer'),
      actionButton('prepare_publish','Prepare and validate publication',class='btn-primary')
    ))
    counts <- value$changes
    tagList(
      div(class='notice',strong('Ready to merge'),p(paste('Publication:',value$id)),
          p(paste(names(counts),counts,collapse=' · '))),
      actionButton('merge_publish','Merge this taxonomy into main',class='btn-primary')
    )
  })
  observeEvent(input$prepare_publish,{
    req(!demo,connected(),live(),active()!='main')
    if(locked()||!is.null(prepared())) return()
    if(length(draft_commands())) {showNotification('Save taxonomy changes before preparing publication.',type='warning');return()}
    locked(TRUE);save_status('Preparing the canonical taxonomy on this review branch…')
    tryCatch({
      result <- withProgress(message='Preparing publication',value=0.5,
        live()$prepare_publish(state(),saved_head(),input$publication_note %||% ''))
      prepared(result);save_status('Publication prepared and verified. Main is still unchanged.')
    },error=function(e) {
      locked(FALSE);save_status('Publication preparation failed. The review branch remains isolated from main.')
      showNotification(conditionMessage(e),type='error',duration=NULL)
    })
  })
  observeEvent(input$merge_publish,{
    req(!demo,connected(),live(),prepared(),active()!='main')
    save_status('Merging the prepared taxonomy into main…')
    tryCatch({
      result <- withProgress(message='Publishing to main',value=0.5,
        live()$merge_publish(prepared()))
      settings <- live()$settings;settings$branch <- 'main'
      load_live(settings)
      save_status(paste('Published and verified on main:',result$publication_id))
      showNotification('The reviewed taxonomy is now published on main.',type='message')
    },error=function(e) {
      save_status('Merge was not confirmed. Main was not treated as published; reconnect before retrying.')
      showNotification(conditionMessage(e),type='error',duration=NULL)
    })
  })
  output$download_draft <- downloadHandler(
    filename=function() paste0('taxonomy-review-',active(),'.json'),
    content=function(file) {req(demo || connected());writeLines(review_json(state()),file,useBytes=TRUE)},
    contentType='application/json'
  )
  observeEvent(input$reset,{
    if(!demo) return()
    state(mock_state());saved_state(mock_state());draft_commands(list());branches(c('main','review-alex'));active('review-alex');drafts(list(main=mock_state(),'review-alex'=mock_state()));connected(FALSE);prepared(NULL)
    updateSelectInput(session,'branch',choices=branches(),selected='review-alex');updateTextInput(session,'branch_name',value='');updateTabsetPanel(session,'page',selected='workspace')
  })
}
`%||%` <- function(x,y) if(is.null(x)) y else x
