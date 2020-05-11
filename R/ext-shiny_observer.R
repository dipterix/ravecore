# creating observers, but keep track of the handlers
make_observe <- function(map, error_handler = NULL){
  stopifnot(inherits(map, 'fastmap2'))

  function(x, env = parent.frame(), quoted = FALSE, ..., label = rand_string(10)){
    if(!quoted){ x = substitute(x) }
    x = rlang::quo_squash(rlang::quo({
      tryCatch({ !!x }, error = function(e){
        raveutils::rave_error("[RAVE ERROR] {e$message}")
        # TODO: Signal STOP command to session
        if(is.function(error_handler)){
          error_handler(e)
        } else {
          traceback(e)
          raveutils::rave_debug("Event expression with error raised")
          cat(!!deparse1(x), '\n')
        }
      })
    }))
    if(!length(label) || is.na(label)){
      label = rand_string(11)
    }
    map[[label]] <- shiny::observe(x = x, env = env, quoted = TRUE, ..., label = label)
    invisible(map[[label]])
  }

}


make_observeEvent <- function(map, error_handler = NULL){
  stopifnot(inherits(map, 'fastmap2'))


  function(eventExpr, handlerExpr,
           event.env = parent.frame(), handler.env = parent.frame(),
           event.quoted = FALSE, handler.quoted = FALSE, ..., ignoreInit = TRUE, label = rand_string(12)){
    if( !event.quoted ){ eventExpr = substitute(eventExpr) }
    if( !handler.quoted ){ handlerExpr = substitute(handlerExpr) }
    eventExpr = rlang::quo_squash(rlang::quo({
      tryCatch({ !!eventExpr }, error = function(e){
        raveutils::rave_error("[RAVE ERROR] {e$message}")
        if(is.function(error_handler)){
          error_handler(e)
        } else {
          traceback(e)
          raveutils::rave_debug("Event expression with error raised")
          cat(!!deparse1(eventExpr), '\n')
        }
      })
    }))
    handlerExpr = rlang::quo_squash(rlang::quo({
      tryCatch({ !!handlerExpr }, error = function(e){
        raveutils::rave_error("[RAVE ERROR] {e$message}")
        if(is.function(error_handler)){
          error_handler(e)
        } else {
          traceback(e)
          raveutils::rave_debug("Event expression with error raised")
          cat(!!deparse1(handlerExpr), '\n')
        }
      })
    }))
    if(!length(label) || is.na(label)){
      label = rand_string(13)
    }
    map[[label]] <- shiny::observeEvent(
      eventExpr = eventExpr, handlerExpr = handlerExpr,
      event.quoted = TRUE, handler.quoted = TRUE,
      event.env = event.env, handler.env = handler.env,
      ignoreInit = ignoreInit, ..., label = label)
    invisible(map[[label]])
  }

}


remove_observers <- function(map){
  stopifnot(inherits(map, 'fastmap2'))

  for(nm in names(map)){
    try({
      map[[nm]]$suspend()
      map[[nm]]$destroy()
      .subset2(map, 'remove')(nm)
    }, silent = TRUE)
  }

}


#' @export
module_notification <- function(
  ..., type = c("message", "warning", "error", "default"),
  duration = 10, closeBotton = TRUE, action = NULL, id,
  session = shiny::getDefaultReactiveDomain()){

  type = match.arg(type)
  context = raveutils::from_rave_context('context')
  if(context == 'rave_running'){
    if(missing(id)){
      id = paste0('..rave-notification-', raveutils::from_rave_context('module_id'))
    }
    shiny::showNotification(ui = shiny::p(...), action = action, duration = duration,
                            closeButton = closeBotton, id = id, type = type, session = session)
  } else {
    level = list(
      "message" = "INFO",
      "warning" = 'WARNING',
      "error" = 'ERROR',
      "default" = 'DEFAULT'
    )[[type]]
    catgl(..., level = level)
  }
}

#' @export
module_remove_notification <- function(id, ...){
  context = raveutils::from_rave_context('context')
  if(context == 'rave_running'){
    if(missing(id)){
      id = paste0('..rave-notification-', raveutils::from_rave_context('module_id'))
    }
    shiny::removeNotification(id, ...)
  }
}