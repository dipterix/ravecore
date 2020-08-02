# Functions to call during the run time

.RAVEGlobalEnv <- new.env(parent = emptyenv())

#' Get default data repository
#' @description Default data repository is an environment shared by modules
#' and packages to store/fetch small global items. Not used often, but useful
#' when 'RAVE' modules need communication across packages.
#' @return environment
#' @export
getDefaultDataRepository <- function(){
  .RAVEGlobalEnv
}

getDefaultContainer <- rave_context_generics('getDefaultContainer')

getDefaultContainer.rave_running <- function(){
  from_rave_context('instance')$container_reactives
}

getDefaultContainer.rave_running_local <- function(){
  list()
}
getDefaultContainer.default <- function(){
  list()
}
getDefaultContainer.rave_module_debug <- function(){
  list()
}

#' Get shiny \code{input} object
#' @description Get \code{session$input} object with correct scope
#' @param isolated whether to \code{\link[shiny]{isolate}} input as a list
#' @param ... pass to other methods
#' @export
getDefaultReactiveInput <- rave_context_generics('getDefaultReactiveInput', alist(isolated = FALSE, ...=))

#' @export
getDefaultReactiveInput.rave_running <- function(isolated = FALSE, ..., session = shiny::getDefaultReactiveDomain()){
  module_id = from_rave_context('module_id')
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
  module_id = from_rave_context('module_id')
  module = loaded_rave_module(module_id)
  new = TRUE
  if(!is.null(module)){
    re = module$package_data$...debug_reactive_input...
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
    module$package_data$...debug_reactive_input... = re
  }
  re
}

#' Get shiny \code{output} object
#' @description Get \code{session$output} object with correct scope
#' @param ... pass to other methods
#' @export
getDefaultReactiveOutput <- rave_context_generics('getDefaultReactiveOutput', alist(...=))

#' @export
getDefaultReactiveOutput.rave_running <- function(..., session = shiny::getDefaultReactiveDomain()){
  module_id = from_rave_context('module_id')
  if(module_id != session$ns(NULL)){
    session = session$rootScope()$makeScope(module_id)
  }
  session$output
}

#' @export
getDefaultReactiveOutput.rave_running_local <- function(...){
  # get context instance
  instance <- from_rave_context('instance')
  instance$local_output %?<-% dipsaus::fastmap2()
  instance$local_output
}

#' @export
getDefaultReactiveOutput.rave_module_debug <- function(isolated = FALSE, ...){
  re = shiny::reactiveValues()
  re
}


#' Get a list shared across modules within a same package
#' @description Package data is a \code{\link[dipsaus]{fastmap2}} instance that
#' stores key-value pairs. A package data is shared across modules but
#' independent across 'RAVE' packages. It's useful to store shared data
#' for modules
#' @return A \code{\link[dipsaus]{fastmap2}} instance
#' @export
getDefaultPackageData <- rave_context_generics('getDefaultPackageData')


..getDefaultPackageData <- function(){
  from_rave_context('instance')$module$package_data
}

#' @export
getDefaultPackageData.rave_running <- ..getDefaultPackageData
#' @export
getDefaultPackageData.rave_running_local <- ..getDefaultPackageData
#' @export
getDefaultPackageData.rave_module_debug <- function(){
  module_id <- from_rave_context('module_id')
  package <- from_rave_context('package')
  module <- loaded_rave_module(module_id, package)
  if(!inherits(module, 'RAVEModule')){
    module = RAVEModule$new(package = package, module_id = module_id, force = FALSE)
  }
  module$package_data
}

#' Get a list specific for currect shiny session and 'RAVE' module
#' @description Session data is a \code{\link[dipsaus]{fastmap2}} instance that
#' stores key-value pairs. Session data are independent across modules and
#' shiny sessions. Two modules have different session data, and for same
#' module, two shiny sessions also have different session data.
#' @return A \code{\link[dipsaus]{fastmap2}} instance
#' @export
getDefaultSessionData <- rave_context_generics('getDefaultSessionData')


..getDefaultSessionData <- function(){
  from_rave_context('instance')$container_data
}

#' @export
getDefaultSessionData.rave_running <- ..getDefaultSessionData
#' @export
getDefaultSessionData.rave_running_local <- ..getDefaultSessionData

..debug_session_data <- dipsaus::fastmap2()
#' @export
getDefaultSessionData.rave_module_debug <- function(){
  ..debug_session_data
}

#' @export
getDefaultSessionData.rave_compile <- function(){
  ..debug_session_data
}


#' Create a fake 'RAVE-shiny' session
#' @param rave_id internally used
#' @param id 'RAVE' module ID
#' @export
fake_session <- function(rave_id = '__fake_session__', id = NULL){
  self_id = id
  fakesession = new.env()

  shiny = asNamespace('shiny')
  list2env(as.list(shiny$createMockDomain()), fakesession)

  fakesession$sendInputMessage = function(inputId, message){
    return(message)
  }
  fakesession$userData = new.env(parent = emptyenv())
  fakesession$userData$rave_id = rave_id
  fakesession$ns = shiny::NS(id)

  fakesession$makeScope = function(id = NULL){
    if( identical(self_id, id) ){
      return(fakesession)
    }else{
      re = fake_session(rave_id = rave_id, id = id)
      re$userData = fakesession$userData
      return(re)
    }
  }

  fakesession$rootScope = function(){
    if(is.null(self_id)){
      return(fakesession)
    }else{
      re = fake_session(rave_id = rave_id, id = NULL)
      re$userData = fakesession$userData
      return(re)
    }
  }

  fakesession
}
