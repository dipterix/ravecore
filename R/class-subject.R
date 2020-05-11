#' @export
as_rave_subject <- function(subject_id, strict = TRUE){
  if(inherits(subject_id, 'RAVESubject')){
    return(subject_id)
  } else {
    RAVESubject$new(subject_id, strict = strict)
  }
}

#' @export
as_rave_project <- function(project){
  if(inherits(project, 'RAVEProject')){
    return(project)
  } else {
    RAVEProject$new(project)
  }
}

#' @export
RAVESubject <- R6::R6Class(
  classname = 'RAVESubject',
  class = TRUE,
  portable = TRUE,
  private = list(
    .root = character(0),
    .name = character(0),
    .path = character(0),
    .project = NULL,
    .preprocess = NULL,
    .cached_config = NULL

  ),
  public = list(
    print = function(...){
      cat('RAVE subject <', self$subject_id, '>\n', sep = '')
      cat('')
    },
    initialize = function(project_name, subject_code = NULL, strict = TRUE){
      stopifnot2(is.character(project_name), msg = "RAVESubject: project name and subject code must be characters")
      if(length(subject_code) != 1){
        if(stringr::str_detect(project_name, '/|\\\\')){
          project_name = stringr::str_trim(
            unlist(stringr::str_split(project_name, '/|\\\\'))
          )
          subject_code = project_name[2]
          project_name = project_name[1]
        } else if(strict){
          raveutils::rave_fatal("Subject {project_name} invalid.")
        }
      }
      private$.project = RAVEProject$new(project_name)
      private$.name = subject_code
      private$.root = normalizePath(file.path(private$.project$path, subject_code), mustWork = FALSE)
      if(!dir.exists(private$.root) && strict){
        raveutils::rave_fatal("Subject {project_name}/{subject_code} doesn't exist.")
      }
      private$.path = file.path(private$.root, 'rave')
      private$.preprocess = RAVEPreprocessSettings$new(subject = self, read_only = TRUE)
      private$.cached_config = dipsaus::fastmap2()
    },

    meta_data = function(
      meta_type = c('electrodes', 'frequencies', 'time_points',
                    'epoch', 'references'),
      meta_name = 'deafult'){
      meta_type = match.arg(meta_type)
      load_meta(meta_type = meta_type, meta_name = meta_name,
                project_name = self$project_name, subject_code = self$subject_code)
    }

  ),
  active = list(
    project = function(){
      private$.project
    },
    project_name = function(){
      private$.project$name
    },
    subject_code = function(){
      private$.name
    },
    subject_id = function(){
      sprintf('%s/%s', private$.project$name, private$.name)
    },
    path = function(){
      private$.root
    },
    rave_path = function(){
      private$.path
    },
    meta_path = function(){
      file.path(self$rave_path, 'meta')
    },
    freesurfer_path = function(){
      # To find freesurfer directory, here are the paths to search
      # 0. if options('rave.freesurfer_dir') is provided, then XXX/subject/
      # 1. rave_data/project/subject/rave/fs
      # 2. rave_data/project/subject/fs
      # 3. rave_data/project/subject/subject

      re = file.path(getOption('rave.freesurfer_dir'), self$subject_code)
      if(isTRUE(dir.exists(re)) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
      re = file.path(self$rave_path, 'fs')
      if(dir.exists(re) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
      re = file.path(self$path, 'fs')
      if(dir.exists(re) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
      re = file.path(self$path, self$subject_code)
      if(dir.exists(re) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
      return(NA)
    },
    preprocess_path = function(){
      file.path(self$rave_path, 'preprocess')
    },
    data_path = function(){
      file.path(self$rave_path, 'data')
    },
    cache_path = function(){
      file.path(self$rave_path, 'data', 'cache')
    },
    epoch_names = function(){
      fs <- list.files(self$meta_path, pattern = '^epoch_[a-zA-Z0-9_]+\\.csv$', ignore.case = TRUE)
      stringr::str_match(fs, '^epoch_([a-zA-Z0-9_]+)\\.[cC][sS][vV]$')[,2]
    },
    reference_names = function(){
      fs <- list.files(self$meta_path, pattern = '^reference_[a-zA-Z0-9_]+\\.csv$', ignore.case = TRUE)
      stringr::str_match(fs, '^reference_([a-zA-Z0-9_]+)\\.[cC][sS][vV]$')[,2]
    },
    reference_path = function(){
      file.path(self$data_path, 'reference')
    },
    preprocess_settings = function(){
      private$.preprocess
    },
    blocks = function(){
      private$.preprocess$blocks
    },
    electrodes = function(){
      private$.preprocess$electrodes
    },
    raw_sample_rates = function(){
      private$.preprocess$sample_rates
    },
    power_sample_rate = function(){
      private$.preprocess$wavelet_params$downsample_to
    },
    has_wavelet = function(){
      private$.preprocess$has_wavelet
    },
    notch_filtered = function(){
      private$.preprocess$notch_filtered
    },
    electrode_types = function(){
      private$.preprocess$electrode_types
    }
  )
)


#' @export
RAVEProject <- R6::R6Class(
  classname = 'RAVEProject',
  class = TRUE,
  portable = TRUE,
  private = list(
    root = character(0),
    .name = character(0),
    .path = character(0)
  ),
  public = list(
    initialize = function(project_name){
      project_name = stringr::str_trim(project_name)
      stopifnot2(length(project_name) == 1 && project_name != '',
                 msg = 'RAVEProject: project_name must not be blank character.')
      stopifnot2(!stringr::str_detect(project_name, '/|\\\\'),
                 msg = 'RAVEProject: project_name must contains no {sQuote("/")} nor {sQuote("\\\\")}')
      private$root = normalizePath(rave_options('data_dir'), mustWork = FALSE)
      private$.name = project_name

      private$.path = normalizePath(file.path(private$root, private$.name), mustWork = FALSE)
      if(!dir.exists(private$.path)){
        raveutils::rave_warn("RAVE project does not exist:\n  {private$.path}")
      }
    },

    subjects = function(){
      re <- list.dirs(private$.path, full.names = FALSE, recursive = FALSE)
      # Must start with a-zA-Z
      re = re[stringr::str_detect(re, '^[a-zA-Z]+')]
      re
    },

    has_subject = function(subject_code){
      dir.exists(file.path(private$.path, subject_code))
    },

    group_path = function(module_id, must_work = FALSE){
      p = file.path(private$.path, '_project_data', module_id)
      if(must_work){
        raveutils::dir_create(p, check = FALSE)
      }
      normalizePath(p, mustWork = FALSE)
    }

  ),
  active = list(
    path = function(){
      private$.path
    },
    name = function(){
      private$.name
    }
  )
)
