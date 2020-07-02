#' @export
rave_needs <- function(expr, label, env = parent.frame()){
  expr <- substitute(expr)
  if(missing(label)){
    label <- deparse(expr)[[1]]
  }
  tryCatch({
    stopifnot(eval(expr, envir = env))
  }, error = function(e){
    raveutils::rave_condition(label, call = expr, class = 'rave_check_error')
  })
  invisible()
}


#' @export
rave_validate <- raveutils::rave_context_generics('rave_validate', alist(
  checks=, onfailure=, onload=, ...=
))

#' @export
rave_validate.rave_compile <- function(checks, onfailure, onload, ...){

  ctx <- raveutils::rave_context()

  ctx$instance$register_data_check(checks)
  ctx$instance$register_loader_interface(onfailure)
  ctx$instance$register_onload_action(onload, names(list(...)))

  ctx$instance$.data_validation <- match.call()

}

#' @export
rave_validate.rave_module_debug <- function(checks, onfailure, onload, ...){
  raveutils::rave_info('Enter validation')
  conf_file <- raveutils::package_file('inst/rave2.yaml')
  conf <- raveutils::load_yaml(conf_file)
  conf$dev_subject
  session_data <- getDefaultSessionData()
  package_data <- getDefaultPackageData()
  global_data <- getDefaultDataRepository()
  has_error <- FALSE
  withCallingHandlers({
    checks(session_data, package_data, global_data)
  }, rave_check_error = function(e){
    raveutils::rave_warn('[Module Validation]: {e$message}')
    has_error <<- TRUE
  })
  if( has_error ){
    raveutils::rave_info('Failed validation, automatically load data under debug mode.')
    dipsaus::list_to_fastmap2(list(...), session_data)
    onload(session_data, package_data, global_data)
  }
}



