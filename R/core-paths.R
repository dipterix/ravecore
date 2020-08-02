#' Get package information under current 'RAVE' context
#' @seealso \code{\link{rave-context}}
#' @param path relative path to file in the project
#' @param package package name
#' @examples
#'
#' # Debug mode, but package is not specified
#' rave_context('rave_module_debug')
#' rave_module_package()
#'
#' # Debug under ravecore package
#' rave_context('rave_module_debug', package = 'ravecore')
#' rave_module_package()
#'
#' # Production mode, running modules under 'ravecore'
#' # always use with_rave_context to temporary set rave_running context
#' with_rave_context(context = 'rave_running', {
#'   rave_module_package()
#' }, package = 'ravecore')
#'
#'
#' # Get package directory
#' rave_context('rave_module_debug', package = 'raveutils')
#' rave_module_root_directory()
#'
#' # Get path to a file within the package
#' # When debugging ravecore, returns relative path of input_compound.js
#' # When running in production mode, returns system path
#' package_file('./inst/assets/input_compound.js', package = 'ravecore')
#'
#' @name module-package
NULL

#' @rdname module-package
#' @export
rave_module_package <- function(){
  if(from_rave_context('context') == 'default'){
    rave_fatal("Cannot call 'rave_module_package' from default context")
  }
  from_rave_context('package')
}


#' @rdname module-package
#' @export
rave_module_root_directory <- function(){
  d <- dipsaus::rs_active_project()
  pkgname <- rave_module_package()

  if(length(d) == 1 && grepl(paste0('/', pkgname, '$'), d)){
    return(d)
  }else{
    # package user
    return(system.file('', package = pkgname))
  }

}



#' @rdname module-package
#' @export
package_file <- rave_context_generics('package_file', alist(path=, package=NULL))

#' @export
package_file.default <- function(path, package=NULL){
  package %?<-% from_rave_context('package')
  stopifnot2(length(package), msg = 'package_file: package must be specified or in current context')
  find_path(path, system.file('', package = package, mustWork = TRUE))
}

#' @export
package_file.rave_module_debug <- function(path, package = NULL){
  if(!length(package)){
    package <- from_rave_context('package')
    proj <- dipsaus::rs_active_project()

    if(is.null(proj)){
      stop('Debug mode must run in RStudio projects')
    }

    proj <- stringr::str_split(proj, '/|\\\\', simplify = TRUE)
    proj <- proj[length(proj)]
    stopifnot2(isTRUE(package == proj), msg = 'package_file: package must be specified or in current context')
    return(normalizePath(file.path(path), mustWork = TRUE))
  }
  find_path(path, system.file('', package = package, mustWork = TRUE))
}

#' @export
package_file.rave_compile <- package_file.rave_module_debug

#' @export
package_file.rave_running <- package_file.rave_module_debug

#' @export
package_file.rave_running_local <- package_file.rave_module_debug

