fit_badge <- function(fit) span(class=paste('fit',fit),switch(fit,green='✓ Clear fit',amber='! Check briefly',red='↗ Needs placement review'))
question_examples <- function(q) {
  if(!nrow(q)) return(p(class='muted','No clear-fit examples yet. Use the taxonomy path to guide your decision.'))
  tags$ul(class='examples',lapply(seq_len(nrow(q)),function(i) tags$li(p(q$text[i]),fit_badge(q$fit[i]))))
}
app_ui <- function(demo=identical(Sys.getenv('NOVA_REVIEW_MODE'),'demo')) fluidPage(
  tags$head(tags$link(rel='stylesheet',href='style.css')),
  div(class='shell',
    div(class='masthead',div(span(class='eyebrow','SURVEY WORKSPACE'),h1('A clearer home for every question.')),
      span(class='demo-pill',if(demo) 'Guided demo · local simulation' else 'Taxonomy review · saved to Fluree')),
    p(class='intro','First refine the taxonomy. Then give the exception questions a place to belong.'),
    uiOutput('workspace_banner'),
    tabsetPanel(id='page',type='pills',
      tabPanel('1. Workspace',value='workspace',
        div(class='page-heading',h2('Choose your review workspace'),p('main is the published baseline. Reviewers make changes on their own branch.')),
        fluidRow(column(5,div(class='card',h3(if(demo) 'Simulated connection' else 'Connect to your dataset'),
          textInput('url','Fluree URL',if(demo) 'https://fluree.example.org' else Sys.getenv('FLUREE_BASE_URL','http://localhost:8090')),textInput('ledger','Ledger',if(demo) 'survey-baseline' else Sys.getenv('FLUREE_TEST_LEDGER','')),
          if(!demo) tagList(textInput('run_id','Completed tagging run',Sys.getenv('NOVA_TAGGING_RUN_ID','')),textInput('reviewer','Reviewer name',Sys.getenv('NOVA_REVIEWER_ID','')),
            tags$details(tags$summary('Data location settings'),p(class='muted','Leave blank for the standard pipeline locations.'),textInput('survey_graph','Survey location',Sys.getenv('NOVA_SURVEY_GRAPH','')),textInput('tagging_base','Tagging location',Sys.getenv('NOVA_TAGGING_GRAPH_BASE','')))),
          selectizeInput('branch','Branch',if(demo) c('main','review-alex') else unique(c('main',Sys.getenv('FLUREE_TEST_BRANCH','main'))),selected=if(demo) 'review-alex' else Sys.getenv('FLUREE_TEST_BRANCH','main'),options=list(create=TRUE)),
          actionButton('connect','Connect',class='btn-primary'),textOutput('connection'))),
          column(7,div(class='card',h3('Available branches'),uiOutput('branches'),hr(),
            textInput('branch_name','New review branch name',placeholder='e.g. review-sam'),
            actionButton('create_branch','Create from main',class='btn-primary'),
            p(class='muted',if(demo) 'Each branch keeps its own draft during this session. A new branch starts with the original baseline.' else 'Every decision is saved to this review branch. Reconnect to resume after closing the app. New branches start from main.')))),
        actionButton('start','Begin taxonomy review →',class='btn-primary')),
      tabPanel('2. Taxonomy review',value='review',uiOutput('review_content'),
        div(class='history',h4('Recent decisions'),uiOutput('history'))),
      tabPanel('3. Question placement',value='placement',uiOutput('placement_content')),
      tabPanel('4. Summary',value='summary',uiOutput('summary'),
        div(class='card',h3('Your draft taxonomy'),uiOutput('tree')),
        if(!demo) downloadButton('download_draft','Download reviewed dataset'),
        if(demo) tagList(actionButton('reset','Reset demo',class='btn-default'),p(class='muted','Reset restores all branches, questions and decisions to the original demo.')) else p(class='muted','Your review is saved after each decision. Create a new review branch to start a separate review.')))))
