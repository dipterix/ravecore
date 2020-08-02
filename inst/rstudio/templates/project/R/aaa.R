
# README!!!

## --- First time running the package
# 1. Go to menu bar: Tools > Project Options > Build Tools, select `Generate Documentation with Roxygen` and press configuration button and select all items.
# 2. Enter `command/ctrl + shift + B` to build the package

## --- To create dev environment:
# run `${{PACKAGE}}:::dev_${{PACKAGE}}(module_id)`, where `module_id` is your module ID.

## --- To add more dependencies
# Open DESCRIPTION, append package names xx under `Imports` field
# In this file, append `#' @import xx` or `#' @importFrom xx function_name`
# `command/ctrl + shift + B` to rebuild the package

## --- To add more than one modules
# Go to `inst/yaml`, append something like this. (indent sensitive)

# - module_id: ${{MODULEID}}_another
#   module_label: '${{MODULELABEL}}'
#   active: yes
#   icon: ~
#   group_name: ~

# Create folder `inst/modules/${{MODULEID}}_another` (matches with module ID),
# Add comp.R and main.R. You can copy from your first project


# -------------------- Imports dependencies --------------------
# There are two ways to import
# First method is to import the entire package by @import pkg
# Second is to import a specific function @importFrom pkg function
#
# If your package depends heavily on one package, use the first one
# otherwise, it's recommended to use the second method to avoid potential conflicts

# Make sure to declare ALL dependencies here to make sure R can find them.


#' @import dipsaus
#' @import raveio
#' @import ravecore
#' @import shiny


# -------------------- Create dev environment --------------------


dev_${{PACKAGE}} <- function(module_id, expose = TRUE, clear_env = FALSE){

  if(!requireNamespace('devtools')){
    stop('Please run `install.packages("devtools")` first.')
  }

  if(clear_env){
    dipsaus::clear_env(.GlobalEnv)
  }
  if(dipsaus::rs_avail()){
    pkg_dir <- dipsaus::rs_active_project()
  }
  pkg_dir %?<-% '.'
  dipsaus::rs_save_all()
  devtools::document(pkg_dir)
  devtools::load_all(pkg_dir, reset = FALSE, export_all = TRUE)

  # set context
  ravecore::rave_context(context = 'rave_module_debug', package = '${{PACKAGE}}', module_id = module_id)

  re_env <- if (expose) .GlobalEnv else new.env(parent = .GlobalEnv)
  # get toolboxes
  fs <- list.files(ravecore::package_file('inst/tools/'), full.names = TRUE)
  for(f in fs){
    source(f, local = re_env)
  }

  session_data <- ravecore::getDefaultSessionData()
  dipsaus::clear_env(session_data)

  invisible(re_env)

}


init_${{PACKAGE}} <- function(module_id){
  module <- ravecore::RAVEModule$new(package = '${{PACKAGE}}', module_id = module_id, force = TRUE, debug = TRUE)
  instance <- module$add_container()
  instance$parse_module(context = 'rave_compile')
  call <- instance$.data_validation
  if(length(call)){
    call[[1]] <- quote(rave_validate)
    eval(call, envir = .GlobalEnv)
  }

  for(expr in instance$init_script){
    eval(expr, envir = .GlobalEnv)
  }
  # load scripts
  for(expr in instance$dynamic_script){
    if(rlang::is_quosure(expr)){
      dipsaus::eval_dirty(expr, env = .GlobalEnv)
    } else {
      source(expr, local = FALSE)
    }
  }
  instance$parse_module(context = 'rave_module_debug')

}

