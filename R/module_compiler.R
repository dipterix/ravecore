

#' @export
parse_module <- function(package, module_id, debug = FALSE){
  with_rave_context('default', {
    module <- loaded_rave_module(module_id, package)
    if(is.null(module)){
      module = RAVEModule$new(package = package, module_id = module_id, force = FALSE)
    }
    module$debug <- debug

    container <- module$add_container()
    container$import_widgets()
    container$parse_module()
  })

  container
}



#' Load external script in 'RAVE' modules
#' @description Load scripts such as output render functions or shiny observers
#' in \code{'comp.R'}
#' @param ... file path or \code{rlang::quo}; see details.
#' @param asis for backward compatibility, ignored in current version.
#' @details \code{'comp.R'} defines inputs and outputs, \code{'main.R'} defines
#' main function. For output render functions and customized reactive events,
#' store them in another file and load them via \code{load_scripts}. For
#' convenient purpose, short debug code don't have to sit in files: wrap
#' them in a \code{quosure}, and the expressions will be loaded during
#' compiling time.
#'
#' @export
load_scripts <- rave_context_generics(
  fun_name = 'load_scripts', alist(...=, asis = TRUE))

#' @export
load_scripts.rave_compile <- function(..., asis = TRUE){

  ctx <- rave_context()
  instance <- ctx$instance

  fs = c(...)
  fs = sapply(fs, function(x){
    if(rlang::is_quosure(x)){
      x
    }else{
      instance$module$get_path( x )
    }
  })

  instance$dynamic_script = c(
    instance$dynamic_script,
    fs
  )
}

#' @export
load_scripts.rave_module_debug <- function(..., asis = TRUE){
  parent_env <- parent.frame()
  fs = c(...)
  fs = sapply(fs, function(x){
    if(rlang::is_quosure(x)){
      dipsaus::eval_dirty(x, parent_env)
    }else{
      source(find_path(x, '.'), local = parent_env)
    }
  })
}


#' Define module initialization expression
#' @description Used to define global variables and
#' change default behaviors of 'RAVE' modules. This function is a
#' \code{\link{rave_context}} generics and it's behavior will change when
#' running with/without shiny
#' @param expr R expression to run when initialize a module
#' @export
define_initialization <- rave_context_generics(
  fun_name = 'define_initialization', alist(expr=))


#' @export
define_initialization.rave_compile <- function(expr){

  expr = substitute(expr)

  ctx <- rave_context()
  instance <- ctx$instance
  instance$init_script[[length(instance$init_script) + 1]] <- expr
  invisible()
}

#' @export
define_initialization.rave_module_debug <- function(expr){
  expr = substitute(expr)
  eval(expr, envir = .GlobalEnv)
}