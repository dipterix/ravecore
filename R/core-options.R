# RAVE Options

# A RAVE option must be stored at ~/.rave.yaml

# this is a rewrite of rave_option to make the package portable

RAVE_YAML_FILE <- '~/.rave.yaml'
rave_options_object <- dipsaus::fastmap2()

rave_options_initialize <- function(){
  .subset2(rave_options_object, 'mset')(

    # Flags
    debug = FALSE,
    test_mode = FALSE,
    disable_startup_speed_check = TRUE,
    test_wavelet = FALSE,

    # Performace
    fast_cache = TRUE,
    max_worker = future::availableCores(),
    drive_speed = c(200, 90),
    max_mem = dipsaus::mem_limit2()$total,

    # shiny-related
    delay_input = 200,
    image_height = 768L, image_width = 1280L,
    default_theme = "dark",

    # paths
    raw_data_dir = "~/rave_data/raw_dir",
    data_dir = "~/rave_data/data_dir",
    module_root_dir = "~/rave_modules/",
    module_lookup_file = "~/rave_modules/modules.csv",
    cache_path = "~/rave_data/cache_dir",

    threeBrain_template_subject = "N27",
    threeBrain_template_dir = "~/rave_data/others/three_brain",

    # Misc
    content_regex = "e([0-9]+)[^0-9]*",
    module_export = "./export",
    export_path = "./export",
    server_time_zone = "America/Chicago",
    logger_level = "DEBUG",
    content_format = "mat",
    crayon_enabled = TRUE
  )


  if(file.exists(RAVE_YAML_FILE)){
    load_yaml(RAVE_YAML_FILE, map = rave_options_object)
  }
}

rave_options_save <- function(){
  if(!dir.exists('~')){
    rave_warn("Cannot find user's HOME directory ~/  Do you have home directory?")
    return(invisible())
  }
  # save to RAVE_YAML_FILE
  save_yaml(rave_options_object, RAVE_YAML_FILE)
}

rave_options_tolist <- function(){
  as.list(rave_options_object)
}



rave_options_shiny <- function(host = '127.0.0.1', port = NULL){

}

#' @export
rave_options <- function(..., .save = TRUE, launch_gui = TRUE,
                         host = '127.0.0.1', port = NULL)
{
  new_opt <- list(...)
  if(length(new_opt)){

    # check if has names
    if(!length(names(new_opt))){
      # get options
      new_opt <- unlist(new_opt)
      if(length(new_opt) == 1){
        return(rave_options_object[[new_opt]])
      } else{
        return(rave_options_object[new_opt])
      }
    }

    # set options
    dipsaus::list_to_fastmap2(new_opt, rave_options_object)
    if(isTRUE(.save)){
      # save options to '~/.rave.yaml'
      rave_options_save()
    }
    return(invisible(new_opt))
  }
  # else it's time to get options

  # In shiny mode or launch_gui is set to FALSE, just return the options
  if(!isTRUE(launch_gui) || shiny_is_running()){
    return(rave_options_tolist())
  }

  # launch a shiny app

  print(rave_options_shiny(host = host, port = port))
}



#' @export
set_rave_theme <- function(...){
  rave_warn('set_rave_theme not implemented yet')
}

#' @export
get_rave_theme <- function(...){
  rave_warn('get_rave_theme not implemented yet')
}





