# functions that will be used by modules

#' Turn 'RAVE' module auto-recalculate on or off
#' @param on logical or integer; see details
#' @details Used inside of 'RAVE' modules. By default modules are automatically
#' updated: that means if an input is updated, usually the module main function
#' will be executed and outputs will be rendered. However, if main body function
#' takes very long time to run, automatically recalculate module will result
#' in freeze shiny session. To solve the issue, module writers can choose
#' manually recalculate modules by turning off auto-recalculation.
#'
#' \code{on} can be logical or numerical. For logical values, either the
#' auto-recalculation is constantly on or off. For numerical, typically
#' integer values, on means temporary on for limited times. For example
#' \code{auto_recalculate(1)} means run main function for one time.
#'
#' To turn off auto-recalculation, call \code{auto_recalculate} within
#' \code{\link{define_initialization}} with \code{on=FALSE}. To temporary
#' turn on recalculation, use shiny \code{\link[shiny]{observeEvent}}
#' to capture an event and call \code{auto_recalculate(1)} once event is
#' triggered.
#'
#' @examples
#'
#' require(shiny)
#' # Turn on debug mode for example
#' rave_context('rave_module_debug')
#'
#' # In initialization
#' define_initialization({
#'   auto_recalculate(FALSE)
#'   # ..., other initialization code
#' })
#'
#' @export
auto_recalculate <- rave_context_generics(
  'auto_recalculate', alist(on=)
)

#' @export
auto_recalculate.default <- function(on){
  return(TRUE)
}

#' @export
auto_recalculate.rave_running <- function(on){
  instance <- from_rave_context('instance')
  if(!missing(on)){
    if(isTRUE(on) || is.infinite(on)){
      rave_info('[{instance$module_label}] Auto-recalculation is turned [ON]')
      instance$auto_run = Inf
    } else {
      on = as.numeric(on)
      if(on <= 0){
        rave_info('[{instance$module_label}] Auto-recalculation is turned [OFF]')
        instance$auto_run = on
      } else if(instance$has_data){
        rave_info('[{instance$module_label}] Auto-recalculation is temporary on for {on} time(s)')
        instance$auto_run = on
      } else {
        # No data detected, no need to recalculate
        instance$auto_run = 0
      }
    }
  }
  return(is.infinite(instance$auto_run))
}


