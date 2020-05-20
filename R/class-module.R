# definition of modules and module repo

rave_package_data_repository <- dipsaus::fastmap2()
rave_loaded_modules <- dipsaus::fastmap2()

loaded_rave_module <- function(module_id){
  rave_loaded_modules[[module_id]]
}

#' @export
RAVEModule <- R6::R6Class(
  classname = 'RAVEModule',
  portable = FALSE,
  cloneable = FALSE,
  parent_env = asNamespace('ravecore'),
  lock_objects = FALSE, # FIXME
  public = list(
    debug = FALSE,

    package = character(0),
    package_env = NULL,
    package_config = NULL,

    # stores module ID shared data
    module_id = character(0),
    module_label = character(0),
    module_group = character(0),
    package_data = NULL,

    # stores execenv instances
    containers = NULL,

    initialize = function(package, module_id, force = FALSE, debug = FALSE){

      self$package = package
      self$module_id = module_id
      self$package_env <- asNamespace(package)
      self$debug <- debug

      if(!is.null(rave_loaded_modules[[module_id]])){
        if(!force){
          raveutils::rave_error("Trying to create a new module that has been loaded: {module_id}")
        }
        old_module <- rave_loaded_modules[[module_id]]
        self$containers <- old_module$containers
        rave_loaded_modules[[module_id]] <- self
      } else {
        rave_loaded_modules[[module_id]] <- self
      }
      rave_package_data_repository[[module_id]] %?<-% dipsaus::fastmap2()
      self$package_data = rave_package_data_repository[[module_id]]
      self$containers %?<-% dipsaus::fastmap2()


      # self$analyze_module()
      rave_conf = self$get_path('rave.yaml')
      self$package_config = raveutils::load_yaml(rave_conf)
      for(conf in self$package_config$modules){
        if(conf$module_id == self$module_id){
          self$module_label = conf$module_label
          self$module_group = conf$group_name
          break()
        }
      }
    },

    add_container = function(session = shiny::getDefaultReactiveDomain()){
      if(!inherits(session, c('ShinySession', 'session_proxy'))){
        # if session is NULL, then default global container
        rave_id <- 'global'
      } else {
        # if session is shiny session, check RAVE_ID
        rave_id <- raveutils::add_to_session(session = session)
      }

      # create new container
      self$containers[[rave_id]] <- RAVEContainer$new(self)

      return(self$containers[[rave_id]])
    },

    get_path = function(path){
      raveutils::find_path(file.path('inst', path), root_dir = self$package_root)
    }

  ),
  active = list(
    package_root = function(){
      if(debug){
        context = 'rave_module_debug'
      } else {
        context = 'rave_compile'
      }
      raveutils::with_rave_context(
        context,
        {
          raveutils::rave_module_root_directory()
        },
        package = self$package, module_id = self$module_id
      )
    }
  )
)




