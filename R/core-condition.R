
#' @export
exit_main <- function(){
  cond <- simpleCondition('Main exited', call = NULL)
  class(cond) <- c('raveExitMain', class(cond))
  signalCondition(cond)
  invisible(cond)
}
