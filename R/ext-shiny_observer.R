# creating observers, but keep track of the handlers
make_observe <- function(map, error_handler = NULL, on_invalidate = NULL){
  stopifnot(inherits(map, 'fastmap2'))

  function(x, env = parent.frame(), quoted = FALSE, ..., label = rand_string(10)){
    if(!quoted){ x = substitute(x) }
    x = rlang::quo_squash(rlang::quo({
      tryCatch({ !!x }, error = function(e){
        # TODO: Signal STOP command to session
        local({
          error_handler <- !!error_handler
          if(is.function(error_handler)){
            error_handler(e)
          } else {
            print(e$call)
            raveutils::rave_debug("Event expression with error raised")
            cat(!!deparse(x), sep = '\n')
            raveutils::rave_error("[Module ERROR] {e$message}")
          }
        })
      })
    }))
    if(!length(label) || is.na(label)){
      label = rand_string(11)
    }
    call <- as.call(list(
      quote(shiny::observe),
      x = x, env = env, quoted = FALSE, ...,
      label = label
    ))
    map[[label]] <- local({eval(call)})
    if(is.function(on_invalidate)){
      map[[label]]$onInvalidate(on_invalidate)
    }
    invisible(map[[label]])
  }

}


make_observeEvent <- function(map, error_handler = NULL, on_invalidate = NULL){
  stopifnot(inherits(map, 'fastmap2'))


  function(eventExpr, handlerExpr,
           event.env = parent.frame(), handler.env = parent.frame(),
           event.quoted = FALSE, handler.quoted = FALSE, ..., ignoreInit = TRUE, label = rand_string(12)){
    if( !event.quoted ){ eventExpr = substitute(eventExpr) }
    if( !handler.quoted ){ handlerExpr = substitute(handlerExpr) }
    eventExpr = rlang::quo_squash(rlang::quo({
      tryCatch({ !!eventExpr }, error = function(e){
        local({
          error_handler <- !!error_handler
          if(is.function(error_handler)){
            error_handler(e)
          } else {
            raveutils::rave_debug("Event expression with error raised")
            print(e$call)
            cat(!!deparse(eventExpr), sep = '\n')
            raveutils::rave_error("[Module ERROR] {e$message}")
          }
        })
      })
    }))
    handlerExpr = rlang::quo_squash(rlang::quo({
      tryCatch({ !!handlerExpr }, error = function(e){

        local({
          error_handler <- !!error_handler
          if(is.function(error_handler)){
            error_handler(e)
          } else {
            raveutils::rave_debug("Event expression with error raised")
            print(e$call)
            cat(!!deparse(handlerExpr), sep = '\n')
            raveutils::rave_error("[Module ERROR] {e$message}")
          }
        })
      })
    }))
    if(!length(label) || is.na(label)){
      label = rand_string(13)
    }
    call <- as.call(list(
      quote(shiny::observeEvent),
      eventExpr = eventExpr, handlerExpr = handlerExpr,
      event.quoted = FALSE, handler.quoted = FALSE,
      event.env = event.env, handler.env = handler.env,
      ignoreInit = ignoreInit, ..., label = label
    ))
    map[[label]] <- local({ eval(call) })
    if(is.function(on_invalidate)){
      map[[label]]$onInvalidate(on_invalidate)
    }

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