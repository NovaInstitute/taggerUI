# Presentation only: commands are validated and applied by novaTagger.
editor_ui <- function() tagList(
  div(class='page-heading',h2('Shape your taxonomy'),p('Rename, move or combine categories. Changes stay in this browser session until you choose Save changes.')),
  div(class='editor-toolbar',actionButton('editor_save','Save changes',class='btn-primary'),actionButton('editor_discard','Discard unsaved'),
      actionButton('editor_undo','Undo'),actionButton('editor_redo','Redo'),actionButton('editor_expand','Expand all'),actionButton('editor_close','Close branches'),textOutput('editor_status',inline=TRUE)),
  div(class='editor-grid',div(class='card editor-browser',textInput('editor_search','Find a category',placeholder='Search names or paths…'),uiOutput('editor_tree')),
      div(class='card editor-inspector',uiOutput('editor_details'))),
  div(class='card editor-question-browser',h3('Inspect questions by category'),
    p(class='muted','Choose a category to browse every question beneath it. Results are paged so large categories remain responsive.'),
    fluidRow(column(7,selectizeInput('editor_inspect_category','Category',choices=NULL)),
      column(5,textInput('editor_inspect_search','Filter question wording',placeholder='Optional search…'))),
    div(class='question-browser-controls',actionButton('editor_inspect_previous','Previous'),
      actionButton('editor_inspect_next','Next'),textOutput('editor_inspect_status',inline=TRUE)),
    div(class='question-scroll',uiOutput('editor_inspect_questions'))))

editor_server <- function(input,output,session,state,apply_draft,save_draft,discard_draft,draft_count,editable,connected,demo) {
  selected <- reactiveVal(1L)
  pending <- reactiveVal(NULL)
  inspect_page <- reactiveVal(1L)
  inspect_category <- reactiveVal(1L)
  inspect_page_size <- 100L
  observeEvent(input$editor_select,{
    id <- as.integer(input$editor_select)
    selected(id);inspect_category(id)
    updateSelectizeInput(session,'editor_inspect_category',selected=id,server=TRUE)
    updateTextInput(session,'editor_inspect_search',value='')
    inspect_page(1L)
  })
  observeEvent(state(),{
    if(!selected() %in% state()$nodes$id) selected(1L)
    if(!inspect_category() %in% state()$nodes$id) inspect_category(1L)
  },ignoreInit=TRUE)
  output$editor_status <- renderText({
    pending_count <- draft_count()
    paste(if(pending_count) paste(pending_count,'unsaved change(s)') else 'All changes saved','·',
      length(state()$editor_undo),'undoable ·',length(state()$editor_redo),'redoable')
  })
  output$editor_tree <- renderUI({
    if(!demo && !connected()) return(p('Connect on Workspace to load your taxonomy.'))
    s <- state(); needle <- tolower(trimws(input$editor_search %||% ''))
    paths <- vapply(s$nodes$id,function(id) node_path(s,id),character(1))
    visible <- s$nodes$id
    if(nzchar(needle)) {
      hits <- s$nodes$id[grepl(needle,tolower(paths),fixed=TRUE)]
      visible <- unique(unlist(lapply(hits,function(id) {
        out <- id
        while(!is.na(s$nodes$parent[match(id,s$nodes$id)])) {id <- s$nodes$parent[match(id,s$nodes$id)];out <- c(out,id)}
        out
      })))
    }
    draw <- function(id) {
      if(!id %in% visible) return(NULL)
      kids <- children(s,id)
      count <- sum(s$questions$leaf %in% descendants(s,id))
      row <- div(class='editor-row',`data-node`=id,draggable=if(id==1L) 'false' else 'true',
        tags$button(type='button',class='editor-select',`data-node`=id,node_label(s,id)),
        span(class='editor-count',format(count,big.mark=',')),
        tags$button(type='button',class='editor-rename',`data-node`=id,`aria-label`=paste('Rename',node_label(s,id)),'✎'))
      if(!length(kids)) return(div(class='editor-leaf',row))
      tags$details(class='editor-branch',`data-branch`=id,open=if(id==1L || nzchar(needle)) NA else NULL,
        tags$summary(row),div(class='tree-children',lapply(kids,draw)))
    }
    tagList(if(!length(visible)) p('No matching categories.'),draw(1L))
  })
  output$editor_details <- renderUI({
    req(demo || connected());s <- state();id <- selected();req(id %in% s$nodes$id)
    ids <- descendants(s,id);q <- s$questions[s$questions$leaf %in% ids,,drop=FALSE]
    counts <- table(q$text); uq <- q[!duplicated(q$text),,drop=FALSE]
    # Spread examples across the available unique wordings instead of showing repeated forms.
    if(nrow(uq)>12) uq <- uq[unique(round(seq(1,nrow(uq),length.out=12))),,drop=FALSE]
    destinations <- setdiff(s$nodes$id,ids)
    opts <- setNames(destinations,vapply(destinations,function(x) node_path(s,x),character(1)))
    tagList(p(class='path',node_path(s,id)),h3(node_label(s,id)),
      p(paste(nrow(q),'question occurrences ·',length(counts),'distinct wordings ·',length(children(s,id)),'child categories')),
      if(id %in% (s$deferred %||% integer())) div(class='notice','Flagged for later'),
      textInput('editor_label','Category name',node_label(s,id)),actionButton('editor_rename_save','Save name'),
      if(id!=1L) tags$details(tags$summary('Move or combine'),selectizeInput('editor_destination','Destination',opts),
        actionButton('editor_move','Move here'),actionButton('editor_merge','Combine into destination'),
        p(class='muted','Move keeps the whole subtree. Combine removes this category and transfers its contents to the destination.')),
      tags$details(tags$summary('Add a child category'),textInput('editor_new_label','New category name'),actionButton('editor_add','Add category')),
      if(id!=1L) actionButton('editor_remove','Remove this level'),actionButton('editor_flag','Flag for later'),
      p(class='muted','Use the question browser below to inspect every question assigned beneath this category.'))
  })
  observe({
    s <- state();ids <- s$nodes$id
    choices <- setNames(ids,vapply(ids,function(id) node_path(s,id),character(1)))
    chosen <- if(inspect_category() %in% ids) inspect_category() else 1L
    updateSelectizeInput(session,'editor_inspect_category',choices=choices,selected=chosen,server=TRUE)
  })
  observeEvent(input$editor_inspect_category,{
    id <- suppressWarnings(as.integer(input$editor_inspect_category))
    if(length(id)==1L && !is.na(id) && id %in% state()$nodes$id) inspect_category(id)
  })
  inspect_questions <- reactive({
    if(!demo && !connected()) return(state()$questions[FALSE,,drop=FALSE])
    s <- state();category <- inspect_category()
    if(!category %in% s$nodes$id) category <- 1L
    q <- s$questions[s$questions$leaf %in% descendants(s,category),,drop=FALSE]
    needle <- trimws(input$editor_inspect_search %||% '')
    if(nzchar(needle)) q <- q[grepl(tolower(needle),tolower(q$text),fixed=TRUE),,drop=FALSE]
    q
  })
  observeEvent(list(inspect_category(),input$editor_inspect_search),inspect_page(1L),ignoreInit=TRUE)
  inspect_pages <- reactive(max(1L,ceiling(nrow(inspect_questions())/inspect_page_size)))
  observe({if(inspect_page()>inspect_pages()) inspect_page(inspect_pages())})
  output$editor_inspect_status <- renderText({
    total <- nrow(inspect_questions());page <- inspect_page();pages <- inspect_pages()
    if(!total) return('No matching questions')
    first <- (page-1L)*inspect_page_size+1L;last <- min(total,page*inspect_page_size)
    paste0('Showing ',format(first,big.mark=','),'–',format(last,big.mark=','),' of ',format(total,big.mark=','),
      if(pages>1L) paste0(' · page ',page,' of ',pages) else '')
  })
  output$editor_inspect_questions <- renderUI({
    if(!demo && !connected()) return(p('Load a workspace to inspect its questions.'))
    s <- state();q <- inspect_questions()
    if(!nrow(q)) return(p(class='muted','No questions match this category and filter.'))
    first <- (inspect_page()-1L)*inspect_page_size+1L;last <- min(nrow(q),inspect_page()*inspect_page_size)
    rows <- q[first:last,,drop=FALSE]
    tags$ol(start=first,class='all-question-list',lapply(seq_len(nrow(rows)),function(i) tags$li(
      p(rows$text[i]),span(class='path',node_path(s,rows$leaf[i])),
      if('source_form_id' %in% names(rows) && !is.na(rows$source_form_id[i]) && nzchar(rows$source_form_id[i]))
        span(class='muted',paste(' · Source:',rows$source_form_id[i])))))
  })
  observeEvent(input$editor_inspect_previous,inspect_page(max(1L,inspect_page()-1L)))
  observeEvent(input$editor_inspect_next,inspect_page(min(inspect_pages(),inspect_page()+1L)))
  run <- function(command) tryCatch({if(editable()) apply_draft(command)},error=function(e) showNotification(conditionMessage(e),type='error'))
  preview <- function(command) {
    if(!editable()) return()
    tryCatch({
      after <- novaTagger::edit_review_tree(state(),command)
      pending(command)
      added <- setdiff(after$affected,state()$affected)
      showModal(modalDialog(title='Review this change',p(tail(after$history$detail,1)),
        p(paste(length(added),'additional questions will be marked for placement review.')),
        p('You can undo this change after applying it.'),footer=tagList(modalButton('Cancel'),actionButton('editor_confirm','Apply change',class='btn-primary'))))
    },error=function(e) showNotification(conditionMessage(e),type='error'))
  }
  command <- function(action,...) c(list(type='edit',action=action,target=selected()),list(...))
  observeEvent(input$editor_confirm,{req(pending());x<-pending();pending(NULL);removeModal();run(x)})
  observeEvent(input$editor_rename_save,run(command('rename',label=input$editor_label)))
  observeEvent(input$editor_inline_rename,run(list(type='edit',action='rename',target=as.integer(input$editor_inline_rename$id),label=input$editor_inline_rename$label)))
  observeEvent(input$editor_drop,preview(list(type='edit',action='move',target=as.integer(input$editor_drop$source),destination=as.integer(input$editor_drop$destination))))
  observeEvent(input$editor_move,preview(command('move',destination=as.integer(input$editor_destination))))
  observeEvent(input$editor_merge,preview(command('merge',destination=as.integer(input$editor_destination))))
  observeEvent(input$editor_remove,preview(command('remove_level')))
  observeEvent(input$editor_add,run(command('add',label=input$editor_new_label)))
  observeEvent(input$editor_flag,run(command('flag')))
  observeEvent(input$editor_undo,run(list(type='edit',action='undo')))
  observeEvent(input$editor_redo,run(list(type='edit',action='redo')))
  observeEvent(input$editor_save,save_draft())
  observeEvent(input$editor_discard,{
    if(!draft_count()) return(showNotification('There are no unsaved taxonomy changes.',type='message'))
    showModal(modalDialog(title='Discard unsaved changes?',p('This restores the last version saved to the workspace.'),
      footer=tagList(modalButton('Keep editing'),actionButton('editor_discard_confirm','Discard unsaved',class='btn-danger'))))
  })
  observeEvent(input$editor_discard_confirm,{removeModal();discard_draft()})
  observeEvent(input$editor_expand,session$sendCustomMessage('editorExpand',TRUE))
  observeEvent(input$editor_close,session$sendCustomMessage('editorExpand',FALSE))
}
