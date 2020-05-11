#' @importFrom dipsaus %?<-%
#' @import raveutils
#' @useDynLib ravecore, .registration = TRUE
NULL


# --------------------------- Utility functions ------------------------


stopifnot2 <- function(..., msg = 'Condition not satisfied'){
  if(!all(c(...))){
    rave_fatal(msg)
  }
}

`%within%` <- function(a, b){
  (a >= min(b)) & (a <= max(b))
}


# --------------------------- Dev-use ----------------------------------
soft_deprecated <- function(){
  env = parent.frame()
  call = do.call(match.call, envir = env, args = list())
  catgl('Function {call[[1]]} is soft-Deprecated. Details: \n{deparse(call)}', level = 'WARNING')
}

hard_deprecated <- function(){
  env = parent.frame()
  call = do.call(match.call, envir = env, args = list())
  catgl('Function {call[[1]]} is soft-Deprecated. Details: \n{deparse(call)}', level = 'FATAL')
}


# --------------------------------- Misc -------------------------------

do_nothing <- function(...){}

