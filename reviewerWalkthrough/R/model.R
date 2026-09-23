# Pure state helpers keep the demo independent of external services.
mock_state <- function() {
  labels <- c('Survey questions','Health and wellbeing','Everyday life',
    'Physical wellbeing','Emotional wellbeing','Personal circumstances','Community and services',
    'Health habits','Physical health','Emotional wellbeing','Support and recovery',
    'Learning and work','Household resources','Local services','Neighbourhood life',
    'Tobacco use','Alcohol use','Long-term conditions','Daily mobility',
    'Mood and worry','Mood and worry','Social support','Rest and recovery',
    'Education','Employment','Income','Housing stability','Healthcare access','Transport access',
    'Neighbourhood safety','Community participation')
  nodes <- data.frame(id=1:31, parent=c(NA_integer_, rep(1:15, each=2)), label=labels)
  texts <- list(
    c('Do you currently smoke tobacco?','How often do you use tobacco?','Have you tried to stop smoking?'),
    c('How often do you drink alcohol?','How many drinks do you have in a typical week?','Has drinking affected your daily routine?'),
    c('Do you have a long-term health condition?','Do you take medication for an ongoing condition?','Does a health condition limit your activities?'),
    c('Can you walk for ten minutes without assistance?','Do you need help climbing stairs?','Can you reach a clinic using public transport?'),
    c('How often have you felt low in the past two weeks?','How often have you felt anxious?','Do worries make daily tasks difficult?'),
    c('How often do you feel overwhelmed?','How manageable are your daily responsibilities?','How often do you drink alcohol to relax?'),
    c('Is there someone you can talk to when upset?','Can you ask friends or family for help?','Do you feel supported by people close to you?'),
    c('How many hours do you usually sleep?','Do you wake feeling rested?','Do you have enough time to recover after work?'),
    c('What is your highest completed qualification?','Are you currently studying?','Can you access the learning opportunities you need?'),
    c('Are you currently employed?','How many paid hours do you work each week?','Do you feel secure in your current job?'),
    c('What is your main source of income?','Can your income cover essential expenses?','Have you missed a rent payment recently?'),
    c('Do you have a stable place to live?','Are you concerned about losing your home?','Is your home suitable for your household?'),
    c('Do you have a regular healthcare provider?','Can you get a medical appointment when needed?','Has cost prevented you from seeking care?'),
    c('Is public transport available near your home?','Can you afford your usual journeys?','Do you have reliable transport to work?'),
    c('Do you feel safe walking locally during the day?','Do you feel safe in your home?','Are you concerned about crime in your area?'),
    c('Do you take part in local community activities?','Do you feel a sense of belonging in your area?','Is there someone you can turn to for emotional support?'))
  q <- data.frame(id=1:48, text=unlist(texts), leaf=rep(16:31,each=3),
                  fit=rep(c('green','green','amber'),16), suggested=NA_integer_, reason='')
  red <- c(12,18,33,48)
  q$fit[red] <- 'red'; q$suggested[red] <- c(29,17,27,22)
  q$reason[red] <- c('This asks about transport rather than physical mobility.',
    'This asks about alcohol use rather than everyday stress.',
    'This asks about housing stability rather than income alone.',
    'This asks about personal support rather than community activities.')
  list(nodes=nodes, questions=q, reviewed=integer(), history=data.frame(action=character(),detail=character()),
       affected=integer(), resolved=integer(), unresolved=integer(),
       placements=data.frame(id=integer(),decision=character()), new_count=0L)
}
children <- function(s,id) s$nodes$id[!is.na(s$nodes$parent) & s$nodes$parent==id]
leaf_ids <- function(s) s$nodes$id[!s$nodes$id %in% s$nodes$parent]
descendants <- function(s,id) c(id,unlist(lapply(children(s,id),function(x) descendants(s,x)),use.names=FALSE))
node_label <- function(s,id) s$nodes$label[match(id,s$nodes$id)]
node_path <- function(s,id) {
  if (!length(id) || is.na(id) || !id %in% s$nodes$id) return('No longer available')
  p <- s$nodes$parent[match(id,s$nodes$id)]
  if(is.na(p)) node_label(s,id) else paste(node_path(s,p),node_label(s,id),sep=' → ')
}
split_order <- function(s) {
  walk <- function(id) if(length(children(s,id))) c(id,unlist(lapply(children(s,id),walk))) else integer()
  walk(1L)
}
current_split <- function(s) { x <- setdiff(split_order(s),s$reviewed); if(length(x)) x[1] else NA_integer_ }
add_history <- function(s,action,detail) {s$history <- rbind(s$history,data.frame(action,detail)); s}
review_split <- function(s,action,child=NULL,label='') {
  if(!action %in% c('approve','defer','rename','collapse')) stop('Choose a valid taxonomy decision.')
  id <- current_split(s); if(is.na(id)) return(s)
  if(action %in% c('rename','collapse') && !child %in% children(s,id)) stop('Choose a child of this split.')
  detail <- node_path(s,id)
  if(action=='rename') {
    label <- trimws(label); if(!nzchar(label)) stop('Enter a replacement tag.')
    detail <- paste(node_label(s,child),'→',label)
    s$nodes$label[s$nodes$id==child] <- label
  }
  if(action=='collapse') {
    detail <- paste(node_label(s,child),'into',node_label(s,id))
    affected <- s$questions$id[s$questions$leaf %in% descendants(s,child)]
    s$affected <- union(s$affected,affected)
    s$resolved <- setdiff(s$resolved,affected)
    s$unresolved <- setdiff(s$unresolved,affected)
    s$placements <- s$placements[!s$placements$id %in% affected,,drop=FALSE]
    s$nodes$parent[which(s$nodes$parent==child)] <- id
    s$questions$leaf[s$questions$leaf==child] <- id
    s$next_node_id <- max(s$nodes$id,if(is.null(s$next_node_id)) 0L else s$next_node_id)
    s$nodes <- s$nodes[s$nodes$id!=child,,drop=FALSE]
  }
  s$reviewed <- c(s$reviewed,id)
  add_history(s,action,detail)
}
queue_ids <- function(s) setdiff(union(s$questions$id[s$questions$fit=='red'],s$affected),s$resolved)
queue_item <- function(s) {
  ids <- queue_ids(s); fresh <- setdiff(ids,s$unresolved)
  if(length(fresh)) fresh[1] else if(length(ids)) ids[1] else NA_integer_
}
place_question <- function(s,decision,destination=NULL,name='',parent=NULL,rationale='') {
  id <- queue_item(s); if(is.na(id)) return(s)
  row <- match(id,s$questions$id); old <- s$questions$leaf[row]
  if(!decision %in% c('keep','suggested','other','new','unresolved')) stop('Choose a placement decision.')
  if(decision=='keep') destination <- old
  if(decision=='suggested') destination <- s$questions$suggested[row]
  if(decision=='new') {
    name <- trimws(name); rationale <- trimws(rationale)
    if(!nzchar(name) || !nzchar(rationale)) stop('Enter a leaf name and a short rationale.')
    if(!length(parent) || is.na(parent) || !parent %in% setdiff(s$nodes$id,leaf_ids(s))) stop('Choose a parent branch.')
    if(tolower(name) %in% tolower(node_label(s,children(s,parent)))) stop('A child with that name already exists here.')
    destination <- max(s$nodes$id,if(is.null(s$next_node_id)) 0L else s$next_node_id)+1L
    s$next_node_id <- destination
    new_node <- s$nodes[1,,drop=FALSE]
    new_node[1,] <- NA
    new_node$id <- destination;new_node$parent <- parent;new_node$label <- name
    s$nodes <- rbind(s$nodes,new_node)
    s$new_count <- s$new_count+1L
    s <- add_history(s,'new leaf',paste(node_path(s,destination),'—',rationale))
  }
  if(decision!='unresolved') {
    if(!length(destination) || is.na(destination) || !destination %in% leaf_ids(s)) stop('Choose an existing leaf. This question’s current location may now be a parent.')
    s$questions$leaf[row] <- destination
    s$resolved <- union(s$resolved,id); s$unresolved <- setdiff(s$unresolved,id)
  } else s$unresolved <- union(s$unresolved,id)
  s$placements <- s$placements[s$placements$id!=id,,drop=FALSE]
  s$placements <- rbind(s$placements,data.frame(id=id,decision=if(decision=='unresolved') 'unresolved' else if(old==destination) 'kept' else 'moved'))
  add_history(s,'placement',paste('Question',id,'—',decision))
}
representatives <- function(s,id,green_only=FALSE) {
  q <- s$questions[s$questions$leaf %in% descendants(s,id),,drop=FALSE]
  if(green_only) q <- q[q$fit=='green',,drop=FALSE]
  # Surface exceptions while preserving at least three meaningful examples.
  q <- q[order(match(q$fit,c('red','green','amber'))),,drop=FALSE]
  head(q,4)
}
split_warnings <- function(s,id) {
  kids <- children(s,id); labels <- tolower(node_label(s,kids)); parent <- tolower(node_label(s,id)); w <- character()
  if(anyDuplicated(labels)) w <- c(w,'Two child groups use the same tag. Give them distinct names.')
  if(parent %in% labels) w <- c(w,'A child repeats its parent tag and is not more specific. Consider a clearer name or a collapse.')
  if(any(s$questions$fit[s$questions$leaf %in% descendants(s,id)]=='red')) w <- c(w,'Some questions may fit elsewhere. They will appear in the placement queue.')
  w
}
