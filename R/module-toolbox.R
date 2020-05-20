# functions that will be used by modules

#' @export
auto_recalculate <- raveutils::rave_context_generics(
  'auto_recalculate', alist(on=)
)

#' @export
auto_recalculate.default <- function(on){
  return(TRUE)
}

#' @export
auto_recalculate.rave_running <- function(on){
  instance <- raveutils::from_rave_context('instance')
  if(!missing(on)){
    if(isTRUE(on) || is.infinite(on)){
      raveutils::rave_info('[{instance$module_label}] Auto-recalculation is turned [ON]')
      instance$auto_run = Inf
    } else {
      on = as.numeric(on)
      if(on <= 0){
        raveutils::rave_info('[{instance$module_label}] Auto-recalculation is turned [OFF]')
      } else {
        raveutils::rave_info('[{instance$module_label}] Auto-recalculation is temporary on for {on} times')
      }

      instance$auto_run = on
    }
  }
  return(is.infinite(instance$auto_run))
}


