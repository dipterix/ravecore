# Functions to call during the run time

.RAVEGlobalEnv <- new.env(parent = emptyenv())

#' @export
getDefaultDataRepository <- function(){
  .RAVEGlobalEnv
}


#' @export
getDefaultReactiveInput <- raveutils::rave_context_generics('getDefaultReactiveInput', alist(isolated = FALSE, ...=))

#' @export
getDefaultReactiveInput.rave_running <- function(isolated = FALSE, ..., session = shiny::getDefaultReactiveDomain()){
  module_id = raveutils::from_rave_context('module_id')
  if(module_id != session$ns(NULL)){
    session = session$rootScope()$makeScope(module_id)
  }
  if(isolated){
    shiny::isolate(shiny::reactiveValuesToList(session$input, ...))
  } else {
    session$input
  }
}

#' @export
getDefaultReactiveInput.rave_running_local <- function(isolated = FALSE, ...){
  new.env(parent = emptyenv())
}

#' @export
getDefaultReactiveInput.rave_module_debug <- function(isolated = FALSE, ...){
  module_id = raveutils::from_rave_context('module_id')
  module = loaded_rave_module(module_id)
  new = TRUE
  if(!is.null(module)){
    re = module$module_data$...debug_reactive_input...
    if(!is.null(re)){
      new = FALSE
    }
  }
  if(new){
    re = dipsaus::fastmap2()
    class(re) = 'list'
    set_f = re$set
    re$set = local({
      warned = FALSE
      function(key, value){
        if(!warned){
          warned <<- TRUE
          rave_warn("Setting {key} to a read-only input is only allowed when debugging. Don't use it in production.\n  This message only appears once when you try to assign values to this variable.")
        }
        set_f(key, value)
      }
    })
    class(re) = c('fastmap2', 'list')
  }
  if(!is.null(module)){
    module$module_data$...debug_reactive_input... = re
  }
  re
}


#' @export
getDefaultReactiveOutput <- raveutils::rave_context_generics('getDefaultReactiveOutput', alist(...=))

#' @export
getDefaultReactiveOutput.rave_running <- function(..., session = shiny::getDefaultReactiveDomain()){
  module_id = raveutils::from_rave_context('module_id')
  if(module_id != session$ns(NULL)){
    session = session$rootScope()$makeScope(module_id)
  }
  session$output
}

#' @export
getDefaultReactiveOutput.rave_running_local <- function(...){
  # get context instance
  instance <- raveutils::from_rave_context('instance')
  instance$local_output %?<-% dipsaus::fastmap2()
  instance$local_output
}

#' @export
getDefaultReactiveOutput.rave_module_debug <- function(isolated = FALSE, ...){
  re = shiny::reactiveValues()
  re
}



#' @export
getDefaultModuleData <- raveutils::rave_context_generics('getDefaultModuleData')


..getDefaultModuleData <- function(){
  raveutils::from_rave_context('instance')$module$module_data
}

#' @export
getDefaultModuleData.rave_running <- ..getDefaultModuleData
#' @export
getDefaultModuleData.rave_running_local <- ..getDefaultModuleData
#' @export
getDefaultModuleData.rave_module_debug <- ..getDefaultModuleData


#' @export
getDefaultSessionData <- raveutils::rave_context_generics('getDefaultSessionData')


..getDefaultSessionData <- function(){
  raveutils::from_rave_context('instance')$container_data
}

#' @export
getDefaultSessionData.rave_running <- ..getDefaultSessionData
#' @export
getDefaultSessionData.rave_running_local <- ..getDefaultSessionData
#' @export
getDefaultSessionData.rave_module_debug <- ..getDefaultSessionData





