fit_badge <- function(fit) span(class=paste('fit',fit),switch(fit,green='✓ Clear fit',amber='! Check briefly',red='↗ Needs placement review'))
question_examples <- function(q) {
  if(!nrow(q)) return(p(class='muted','No clear-fit examples yet. Use the taxonomy path to guide your decision.'))
  tags$ul(class='examples',lapply(seq_len(nrow(q)),function(i) tags$li(p(q$text[i]),fit_badge(q$fit[i]))))
}
app_ui <- function(demo=identical(Sys.getenv('NOVA_REVIEW_MODE'),'demo'),local=FALSE) fluidPage(
  tags$head(tags$link(rel='stylesheet',href='editor-assets/style.css'),tags$script(src='editor-assets/editor.js')),
  div(class='shell',
    div(class='masthead',div(span(class='eyebrow','SURVEY WORKSPACE'),h1('A clearer home for every question.')),
      if(local) span(class='demo-pill','Taxonomy editor · saved locally') else if(!demo) span(class='demo-pill','Taxonomy review · saved to Fluree')),
    uiOutput('workspace_banner'),
    tabsetPanel(id='page',type='pills',
      tabPanel('1. Workspace',value='workspace',
        div(class='page-heading',h2('Choose your review workspace'),p('main is the published baseline. Reviewers make changes on their own branch.')),
        fluidRow(column(5,div(class='card',h3(if(local) 'Open local draft' else 'Open a workspace'),
          if(local) p(class='muted','This editor uses the supplied local snapshot.') else if(demo)
            p(class='muted','Choose an existing branch or create a new branch from main.') else
            div(class='notice',strong('Configured dataset'),p('nova-survey-baseline-20260916 · survey-baseline-v1'),p(class='muted','Fluree: localhost:8090')),
          if(!demo&&!local) textInput('reviewer','Reviewer name',Sys.getenv('NOVA_REVIEWER_ID','')),
          selectizeInput('branch','Review branch',if(demo) c('main','review-alex') else unique(c('main',Sys.getenv('FLUREE_TEST_BRANCH','main'))),selected=if(demo) 'review-alex' else Sys.getenv('FLUREE_TEST_BRANCH','main'),options=list(create=FALSE)),
          if(!demo&&!local) actionButton('refresh_branches','Refresh branch list'),
          actionButton('connect',if(local) 'Open local draft' else 'Load selected workspace',class='btn-primary'),
          textOutput('branch_status'),textOutput('connection'))),
          column(7,div(class='card',h3('Available branches'),uiOutput('branches'),
            if(!local) tagList(hr(),textInput('branch_name','New review branch name',placeholder='e.g. review-sam'),
              actionButton('create_branch',if(demo) 'Create from main' else 'Create from main and load',class='btn-primary'),
              p(class='muted',if(demo) 'A new branch starts from main and opens as the active workspace.' else 'Branch creation is lightweight. The new branch is loaded once after it is created. Every decision is then saved to that branch.'))))),
        actionButton('start','Open taxonomy editor →',class='btn-primary')),
      tabPanel('2. Taxonomy editor',value='summary',editor_ui(),
        tags$details(tags$summary('Review progress'),uiOutput('summary')),
        if(!demo) div(class='card publish-card',h3('Publish this checkpoint'),uiOutput('publish_status')),
        if(!demo) downloadButton('download_draft','Download reviewed dataset'),
        if(demo) tagList(actionButton('reset','Reset workspace',class='btn-default'),p(class='muted','Reset restores branches, questions and decisions to their starting state.')) else p(class='muted','Your review is saved after each decision. Create a new review branch to start a separate review.')))))
