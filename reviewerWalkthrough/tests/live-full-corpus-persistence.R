# Full-corpus acceptance test against the reviewer's fixed local Fluree dataset.
# Run once without arguments, then again with `resume` in a fresh R process.
args <- commandArgs(FALSE);path <- sub('^--file=','',args[grepl('^--file=',args)])
setwd(normalizePath(file.path(dirname(path),'..')))
rush_paths <- c('../../novaRush_v2','../../novaRush')
rush_path <- rush_paths[dir.exists(rush_paths)][1L]
tagger_paths <- c('../../novaTagger','../../semanticTagger')
tagger_path <- tagger_paths[dir.exists(tagger_paths)][1L]
if(is.na(rush_path)||is.na(tagger_path)) stop('Could not find sibling novaRush and novaTagger/semanticTagger checkouts.')
pkgload::load_all(rush_path,quiet=TRUE)
pkgload::load_all(tagger_path,quiet=TRUE)
source('R/model.R');source('R/persistence.R')

mode <- commandArgs(TRUE)
info_path <- Sys.getenv('NOVA_FULL_REVIEW_TEST_INFO','/tmp/nova-review-full-corpus-test.rds')
backend <- fluree_review_backend()
base <- review_dataset_settings(branch='main',reviewer='full-corpus-acceptance')
config <- novaRush::setConfig(baseUrl=base$url,ledger=base$ledger,branch='main',timeout=300)

if(identical(mode,'resume')) {
  info <- readRDS(info_path)
  started <- Sys.time()
  settings <- base;settings$branch <- info$branch
  session <- backend$connect(settings)
  ix <- match(info$target_stable_id,session$state$nodes$stable_id)
  stopifnot(!is.na(ix),identical(session$state$nodes$label[ix],info$saved_label),
    identical(session$head,info$saved_head),nrow(session$state$questions)==info$question_count,
    info$question_count>=10000L,identical(novaRush::branchHead(config,'main'),info$main_head),
    info$branch %in% backend$list_branches(base))
  cat('PASS: fresh process resumed the saved full-corpus taxonomy edit.\n')
  cat('Questions:',format(info$question_count,big.mark=','),' Categories:',format(nrow(session$state$nodes),big.mark=','),'\n')
  cat('Branch:',info$branch,' Head:',session$head,'\n')
  cat('Main remains:',info$main_head,'\n')
  cat('Resume load seconds:',round(as.numeric(difftime(Sys.time(),started,units='secs')),1),'\n')
} else {
  suffix <- format(Sys.time(),'%Y%m%d-%H%M%S')
  branch <- paste0('review-ui-accept-',suffix)
  main_head <- novaRush::branchHead(config,'main')
  stopifnot('main' %in% backend$list_branches(base))
  created <- backend$create_branch(base,branch)
  stopifnot(!isTRUE(created$exists))
  settings <- base;settings$branch <- branch
  started <- Sys.time();session <- backend$connect(settings)
  stopifnot(nrow(session$state$questions)>=10000L,nrow(session$state$nodes)>1L)

  cleanup <- list(type='edit',action='deduplicate')
  cleaned <- apply_review_command(session$state,cleanup)
  target <- sort(leaf_ids(cleaned))[1L]
  target_stable_id <- as.character(cleaned$nodes$stable_id[match(target,cleaned$nodes$id)])
  original_label <- as.character(cleaned$nodes$label[match(target,cleaned$nodes$id)])
  saved_label <- paste0(original_label,' [acceptance ',suffix,']')
  rename <- list(type='edit',action='rename',target=target,label=saved_label)
  command <- list(type='edit_batch',commands=list(cleanup,rename))
  result <- session$save(command,session$head)
  ix <- match(target_stable_id,result$state$nodes$stable_id)
  stopifnot(!is.na(ix),identical(result$state$nodes$label[ix],saved_label),
    !identical(result$head,'baseline'),identical(novaRush::branchHead(config,'main'),main_head))
  info <- list(branch=branch,main_head=main_head,saved_head=result$head,
    target_stable_id=target_stable_id,original_label=original_label,saved_label=saved_label,
    question_count=nrow(result$state$questions),category_count=nrow(result$state$nodes))
  saveRDS(info,info_path)
  cat('PASS: loaded the full corpus and saved one batched editor event to Fluree.\n')
  cat('Questions:',format(info$question_count,big.mark=','),' Categories after cleanup:',format(info$category_count,big.mark=','),'\n')
  cat('Branch:',branch,' Head:',result$head,'\n')
  cat('Main remains:',main_head,'\n')
  cat('Initial load and save seconds:',round(as.numeric(difftime(Sys.time(),started,units='secs')),1),'\n')
  cat('Run this script with `resume` to verify restoration in a fresh process.\n')
}
