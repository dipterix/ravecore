

#' @export
parse_module <- function(package, module_id, debug = FALSE){
  raveutils::rave_context('default')
  module <- loaded_rave_module(module_id)
  if(is.null(module)){
    module = RAVEModule$new(package = package, module_id = module_id, force = FALSE)
  }
  module$debug = debug

  container = module$add_container()
  container$import_widgets()
  container$parse_module()

  container
}




#' @export
load_scripts <- raveutils::rave_context_generics(
  fun_name = 'load_scripts', alist(...=, asis = TRUE))

#' @export
load_scripts.rave_compile <- function(..., asis = TRUE){

  ctx <- raveutils::rave_context()
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
define_initialization <- raveutils::rave_context_generics(
  fun_name = 'define_initialization', alist(expr=))


#' @export
define_initialization.rave_compile <- function(expr){

  expr = substitute(expr)

  ctx <- raveutils::rave_context()
  instance <- ctx$instance
  instance$init_script[[length(instance$init_script) + 1]] <- expr
  invisible()
}