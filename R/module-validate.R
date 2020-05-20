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
  checks=, onfailure=, onload=, variables=, ...=
))

#' @export
rave_validate.rave_compile <- function(checks, onfailure, onload, variables = NULL, ...){

  ctx <- raveutils::rave_context()

  ctx$instance$register_data_check(checks)
  ctx$instance$register_loader_interface(onfailure)
  ctx$instance$register_onload_action(onload, variables)

  ctx$instance$.data_validation <- match.call()


  # ctx$instance$data_loader_reactive = function(check_list){
  #
  #
  #   modal_info <- onfailure(check_list, ctx$instance$module$module_data, getDefaultDataRepository())
  #
  #   modal_info
  #
  #   session <- shiny::getDefaultReactiveDomain()
  #   input <- session$input
  #   ns <- session$ns
  #
  #
  #
  #   # trigger input change
  #   dipsaus::set_shiny_input(inputId = '..rave_import_data_ui_show..', value = Sys.time())
  #
  # }



  # check_results <- ctx$instance$data_check()
  # if(length(check_results$error_list)){
  #
  #   # print out error message
  #   rave_info('RAVE might require more data...')
  #   for(e in check_results$error_list){
  #     rave_info('  ', e$message, ' - [DEBUG INFO]: ', deparse(e$call)[[1]])
  #   }
  #
  #   # calling on_failure, but need to store observers that will be
  #   # destroyed later
  #   modal_info <- ctx$instance$data_loader_ui(check_results$check_list)
  #
  # }

}

#' @export
rave_validate.rave_module_debug <- function(checks, onfailure, onload, variables = NULL, ...){
  raveutils::rave_info('Enter validation')
  conf_file <- raveutils::package_file('inst/rave.yaml')
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



