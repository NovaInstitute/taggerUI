library(shiny)

# Standalone in-memory reviewer walkthrough; launch with shiny::runApp().
leaves <- data.frame(
  leaf = c("Age and birth", "Education and work", "Income and earnings",
           "Health habits", "Physical health", "Emotional wellbeing",
           "Healthcare access", "Housing and transport"),
  parent = rep(c("Personal context", "Health and wellbeing", "Services and living"),
               c(3, 3, 2)), stringsAsFactors = FALSE
)
texts <- list(
  c("What is your age?", "What year were you born?", "Which age range applies?"),
  c("What is your education level?", "Are you currently employed?", "How many hours do you work?"),
  c("What is your monthly income?", "What is your main income source?", "Has your income changed recently?"),
  c("Do you smoke tobacco?", "How often do you drink alcohol?", "Have you used tobacco recently?"),
  c("Do you have a chronic condition?", "Do you take long-term medication?", "Have you been diagnosed with diabetes?"),
  c("How often have you felt anxious?", "How often have you felt depressed?", "How well have you been sleeping?"),
  c("Do you have a regular healthcare provider?", "Did cost stop you seeking care?", "How long does it take to reach a clinic?"),
  c("Is your housing stable?", "Do you have reliable transport?", "Did transport stop you attending care?")
)
questions <- do.call(rbind, lapply(seq_len(nrow(leaves)), function(i) data.frame(
  id = paste0("Q", i, "-", 1:3), question = texts[[i]], leaf = leaves$leaf[i],
  parent = leaves$parent[i], fit = c("Green", "Green", if (i %in% c(3, 5, 6, 8)) "Red" else "Amber"),
  stringsAsFactors = FALSE
)))
splits <- list(
  list(path = "Survey questions", children = c("Personal context", "Health and wellbeing", "Services and living"), warning = ""),
  list(path = "Survey questions → Personal context", children = c("Age and birth", "Education and work", "Income and earnings"), warning = "Income and earnings may overlap with the parent and needs a specific label."),
  list(path = "Survey questions → Health and wellbeing", children = c("Health habits", "Physical health", "Emotional wellbeing"), warning = "Several red questions will need a later placement decision."),
  list(path = "Survey questions → Services and living", children = c("Healthcare access", "Housing and transport"), warning = "")
)

ui <- navbarPage("Taxonomy reviewer — guided demo",
  tabPanel("1. Review splits", fluidRow(
    column(4, wellPanel(textOutput("progress"), tags$strong("Current path"), textOutput("path"),
      selectInput("child", "Child group", character()), textInput("tag", "Clearer tag"),
      textAreaInput("note", "Reviewer note", rows = 2),
      actionButton("approve", "Split looks right", class = "btn-success"), br(), br(),
      actionButton("rename", "Save clearer tag", class = "btn-primary"), br(), br(),
      actionButton("collapse", "Mark child for collapse", class = "btn-warning"),
      hr(), "Green = clear fit. Amber = check briefly. Red = placement review.")),
    column(8, h3("Does this split make sense?"), uiOutput("warning"),
      h4("Representative questions"), tableOutput("split_questions"),
      h4("Decisions made"), tableOutput("history"))
  )),
  tabPanel("2. Place queued questions", fluidRow(
    column(4, wellPanel(textOutput("queue_count"), selectInput("queued", "Question", character()),
      tags$strong("Current location"), textOutput("location"),
      radioButtons("decision", "Decision", c("Keep in current leaf"="keep", "Move to existing leaf"="move", "Create a new leaf"="new")),
      selectInput("destination", "Destination leaf", character()),
      textInput("new_leaf", "New leaf name"),
      actionButton("place", "Save placement", class = "btn-primary"))),
    column(8, h3("Resolve only the exceptions"), p("Compare with the green representative questions before choosing a home."),
      h4("Questions in selected destination"), tableOutput("examples"),
      h4("Questions still queued"), tableOutput("queue"))
  )),
  tabPanel("3. Summary", h3("Reviewed taxonomy draft"), tableOutput("taxonomy"), uiOutput("summary"))
)

server <- function(input, output, session) {
  tax <- reactiveVal(leaves); qs <- reactiveVal(questions); i <- reactiveVal(1L)
  log <- reactiveVal(data.frame(decision=character(), detail=character()))
  done <- reactiveVal(character())
  current <- reactive(splits[[i()]])
  observe({ x <- current(); updateSelectInput(session, "child", choices=x$children, selected=x$children[1]) })
  output$progress <- renderText(paste("Split", i(), "of", length(splits)))
  output$path <- renderText(current()$path)
  output$warning <- renderUI({ w <- current()$warning; if(nzchar(w)) div(class="alert alert-warning", w) else div(class="alert alert-success", "No automatic warning for this split.") })
  output$split_questions <- renderTable({
    x <- current()$children; z <- qs()[qs()$leaf %in% x, c("leaf","question","fit")]; z
  }, striped=TRUE)
  add <- function(a,d) log(rbind(log(), data.frame(decision=a,detail=d)))
  next_split <- function() i(min(i()+1L,length(splits)))
  observeEvent(input$approve, { add("Approved split",current()$path); next_split() })
  observeEvent(input$rename, { req(nzchar(trimws(input$tag))); old<-input$child; new<-trimws(input$tag); q<-qs(); t<-tax(); q$leaf[q$leaf==old]<-new; t$leaf[t$leaf==old]<-new; qs(q);tax(t);add("Renamed child",paste(old,"→",new));next_split() })
  observeEvent(input$collapse, { add("Marked for collapse",paste(input$child,"into",current()$path));next_split() })
  output$history <- renderTable(log(), striped=TRUE)
  queue <- reactive({ q<-qs(); q[q$fit=="Red" & !q$id %in% done(),,drop=FALSE] })
  observe({ q<-queue(); updateSelectInput(session,"queued",choices=setNames(q$id,paste(q$id,"—",q$question))); updateSelectInput(session,"destination",choices=tax()$leaf) })
  item <- reactive({ q<-queue(); q[match(input$queued,q$id),,drop=FALSE] })
  output$queue_count <- renderText(paste(nrow(queue()),"questions need a placement decision"))
  output$location <- renderText({ z<-item(); if(!nrow(z)) "Queue complete" else paste(z$parent,z$leaf,sep=" → ") })
  output$examples <- renderTable({ q<-qs(); q[q$leaf==input$destination & q$fit=="Green",c("question","fit"),drop=FALSE] }, striped=TRUE)
  output$queue <- renderTable(queue()[,c("question","leaf","parent"),drop=FALSE], striped=TRUE)
  observeEvent(input$place,{ z<-item();req(nrow(z)); dest<-if(input$decision=="keep") z$leaf else if(input$decision=="move") input$destination else trimws(input$new_leaf);req(nzchar(dest));q<-qs();t<-tax();q$leaf[q$id==z$id]<-dest;if(input$decision=="new"&&!dest%in%t$leaf)t<-rbind(t,data.frame(leaf=dest,parent=z$parent));qs(q);tax(t);done(c(done(),z$id));add("Placed question",paste(z$question,"→",dest)) })
  output$taxonomy <- renderTable(tax(), striped=TRUE)
  output$summary <- renderUI(tags$ul(tags$li(paste(nrow(log()),"decisions recorded")),tags$li(paste(length(done()),"questions placed")),tags$li(paste(nrow(queue()),"questions unresolved"))))
}
shinyApp(ui, server)

