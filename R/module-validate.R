#' @name rave-validate
#' @title Validate rave data and other conditions
#' @description For 'RAVE' modules, you always need to check if data is
#' available or missing before initializing the inputs.
#'
#' @param checks function checking whether data is missing; see details and
#' examples
#' @param onfailure function to call once \code{checks} fails, defines user
#' interface for user inputs
#' @param onload load data for modules: function to call once \code{checks}
#' fails, users finish input forms and decide to load data
#' @param expr expression returning \code{TRUE/FALSE} or throwing error (
#' meaning fail the test) indicating whether data needed is missing
#' @param label  message to convey to user if data failed validation test
#' @param env environment to evaluate \code{expr}
#' @param ... variables to set under debug mode
#'
#' @details \code{rave_validate} should be called in \code{'comp.R'},
#' \code{rave_needs} functions within \code{checks} function.
#'
#' \code{checks}, \code{onfailure}, and \code{onload} are functions taking
#' three arguments: \code{session_data}, \code{package_data}, and
#' \code{global_data}. \code{session_data} stores temporary data,
#' \code{package_data} stores module and package data, \code{global_data}
#' shares some global settings. \code{checks} function needs to validate
#' data and find if data is missing. \code{rave_needs} will capture the
#' failed validations. If one or more tests fail, function \code{onfailure}
#' will be called to generate loading user interface. Users are required
#' to enter information required for the missing data. Once all information
#' is collected, \code{onload} will be called to load data.
#'
#' \code{...} are key-value pairs for debug mode. There are two purposes
#' for these variables. The first usage is in debug mode, there will be
#' no user interface popping up to load any data. Instead, those key-value
#' pairs will be "fake" inputs stored in \code{package_data}. The second
#' usage is in the run-time, the keys work as indicator to store
#' the actual input values (\code{input[[key]]}) to \code{package_data}
#' for further use, for example, by \code{onload}
#'
#' @examples
#' \dontrun{
#'
#' # Running this block of code directly will result in an error
#' # you need to run in RAVE application or in debug mode
#' # to enter debug mode, open RAVE module packages in rstudio as a project
#' # run `rave_context('rave_module_debug', package = <your package>)`
#'
#' # A validation of whether project is chosen
#' rave_validate(
#'   checks = function(session_data, package_data, global_data){
#'     loaded_project <- package_data$project_name
#'     rave_needs(isTRUE(loaded_project %in% get_projects()),
#'                label = 'Project name is missing')
#'   },
#'   onfailure = function(session_data, package_data, ...){
#'     all_projects <- get_projects()
#'
#'     ui <- tagList(
#'       selectInput(ns('project_name'), 'Select a project',
#'                   choices = all_projects,
#'                   selected = package_data$project_name)
#'     )
#'
#'     observeEvent(input$project_name, {
#'       print(sprintf('switch to project %s ?', input$project_name))
#'     }, ignoreInit = FALSE, ignoreNULL = TRUE)
#'
#'     list(ui = ui)
#'   },
#'   onload = function(session_data, package_data, ...){
#'     # use information from `onfailure` to make sure `checks` passes
#'     # nothing to do because `project_name` has been stored to `package_data`
#'   },
#'
#'   # Indicating `project_name` is an input and need to be stored in
#'   # package_data During debug mode, package_data$project_name will be
#'   # assigned with 'test'
#'   project_name = 'test'
#' )
#'
#' }
#'
NULL


#' @rdname rave-validate
#' @export
rave_validate <- rave_context_generics('rave_validate', alist(
  checks=, onfailure=, onload=, ...=
))


#' @rdname rave-validate
#' @export
rave_needs <- function(expr, label, env = parent.frame()){
  expr <- substitute(expr)
  if(missing(label)){
    label <- deparse(expr)[[1]]
  }
  tryCatch({
    stopifnot(eval(expr, envir = env))
  }, error = function(e){
    rave_condition(label, call = expr, class = 'rave_check_error')
  })
  invisible()
}


#' @export
rave_validate.rave_compile <- function(checks, onfailure, onload, ...){

  ctx <- rave_context()

  ctx$instance$register_data_check(checks)
  ctx$instance$register_loader_interface(onfailure)
  ctx$instance$register_onload_action(onload, names(list(...)))

  ctx$instance$.data_validation <- match.call()

}

#' @export
rave_validate.rave_module_debug <- function(checks, onfailure, onload, ...){
  rave_info('Enter validation')
  conf_file <- package_file('inst/rave2.yaml')
  conf <- load_yaml(conf_file)
  conf$dev_subject
  session_data <- getDefaultSessionData()
  package_data <- getDefaultPackageData()
  global_data <- getDefaultDataRepository()
  has_error <- FALSE
  withCallingHandlers({
    checks(session_data, package_data, global_data)
  }, rave_check_error = function(e){
    rave_warn('[Module Validation]: {e$message}')
    has_error <<- TRUE
  })
  if( has_error ){
    rave_info('Failed validation, automatically load data under debug mode.')
    dipsaus::list_to_fastmap2(list(...), session_data)
    onload(session_data, package_data, global_data)
  }
}



