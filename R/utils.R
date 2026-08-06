# Internal null-coalescing helper used by the staged walkthrough.
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
