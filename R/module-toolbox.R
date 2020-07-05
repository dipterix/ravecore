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
        instance$auto_run = on
      } else if(instance$has_data){
        raveutils::rave_info('[{instance$module_label}] Auto-recalculation is temporary on for {on} time(s)')
        instance$auto_run = on
      } else {
        # No data detected, no need to recalculate
        instance$auto_run = 0
      }
    }
  }
  return(is.infinite(instance$auto_run))
}


