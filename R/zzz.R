

.onLoad <- function(libname, pkgname){
  rave_options_initialize()
}


#' @export
rave_clear_cache <- function(){
  cache_path = rave_options('cache_path')
  if(dir.exists(cache_path)){
    unlink(cache_path, recursive = TRUE)
  }
}

.onUnload <- function(libpath){
}