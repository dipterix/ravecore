#' Definition for preprocess configurations
#'
#' @examples
#'
#' \dontrun{
#'
#' conf <- RAVEPreprocessSettings$new(subject = 'demo/DemoSubject')
#' conf$blocks  # "008" "010" "011" "012"
#'
#' conf$electrodes   # 5 electrodes
#'
#' # Electrode 14 information
#' conf$electrode_info(electrode = 14)
#'
#' conf$data_imported # All 5 electrodes are imported
#'
#' conf$data_locked   # Whether block, sample rates should be locked
#'
#' }
#'
#' @export
RAVEPreprocessSettings <- R6::R6Class(
  classname = 'RAVEPreprocessSettings',
  portable = FALSE,
  private = list(
    has_key = function(keys){
      .subset2(self$data, 'has')(keys)
    },
    get_from = function(keys, default = NULL){
      has_k = private$has_key(keys)
      if(any(has_k)){
        self$data[[keys[has_k][[1]]]]
      } else {
        default
      }
    }
  ),
  public = list(

    #' @field current_version current configuration setting version
    current_version = 0,

    #' @field path settings file path
    path = character(0),

    #' @field backup_path alternative back up path for redundancy checks
    backup_path = character(0),

    #' @field data list of raw configurations, internally used only
    data = NULL,

    #' @field subject \code{\link{RAVESubject}} instance
    subject = NULL,

    #' @field read_only whether the configuration should be read-only,
    #' not yet implemented
    read_only = TRUE,

    #' @description constructor
    #' @param subject character or \code{\link{RAVESubject}} instance
    #' @param read_only whether subject should be read-only (not yet implemented)
    initialize = function(subject, read_only = TRUE){
      self$subject = as_rave_subject(subject, strict = FALSE)
      self$path = file.path(self$subject$preprocess_path, 'rave.yaml')
      self$backup_path = file.path(self$subject$rave_path, 'log.yaml')
      self$read_only = isTRUE(read_only)
      self$data = dipsaus::fastmap2()
      if(file.exists(self$path)){
        load_yaml(self$path, map = self$data)
      } else if( file.exists(self$backup_path) ){
        load_yaml(self$backup_path, map = self$data)
        dipsaus::list_to_fastmap2(self$data$preprocess, self$data)
      }
      self$migrate()
    },

    #' @description whether configuration is valid or not
    valid = function(){
      length(self$data) > 0
    },

    #' @description set electrodes
    #' @param electrodes integer vectors
    set_electrodes = function(electrodes){
      stopifnot2(!self$data_frozen, msg = 'The data is locked. Cannot change electrodes')
      self$data$electrodes = electrodes
    },

    #' @description convert old format to new formats
    #' @param force whether to force migrate and save settings
    migrate = function(force = FALSE){
      if(!self$old_version && !force){ return() }
      if( self$version < 0 ){
        if(length(self$data)){
          # rave_debug("Migrating from an old format")
        }
      }
      # transfer from RAVE-Fir
      electrodes = self$electrodes
      self$data$electrodes = electrodes
      self$data$data_locked = self$data_locked
      self$data$notch_params = self$notch_params
      self$data$wavelet_params = self$wavelet_params
      # get electrode info
      sample_rates = self$sample_rates
      notch_filtered = self$notch_filtered
      has_wavelet = self$has_wavelet
      data_imported = self$data_imported
      electrode_locked = self$electrode_locked
      for(ii in seq_along(electrodes)){
        x = list(
          sample_rate = sample_rates[[ii]],
          notch_filtered = notch_filtered[[ii]],
          has_wavelet = has_wavelet[[ii]],
          data_imported = data_imported[[ii]],
          electrode_locked = electrode_locked[[ii]],
          electrode_type = 'LFP'
        )
        self$data[[as.character(electrodes[[ii]])]] = x
      }
      self$data$preprocess_version = self$current_version

    },

    #' @description get electrode information
    #' @param electrode integer
    #' @return list of electrode type, number, etc.
    electrode_info = function(electrode){
      if(!electrode %in% self$electrodes){
        return()
      }
      self$data[[as.character(electrode)]]
    },

    #' @description save settings to hard disk
    save = function(){
      dir_create(dirname(self$path))
      dir_create(dirname(self$backup_path))
      self$data$signature = rand_string()
      # Make sure the basic information gets saved
      self$migrate(force = TRUE)
      save_yaml(self$data, self$path)
      save_yaml(list(preprocess = as.list(self$data),
                                preprocess_signature = self$data$signature),
                           self$backup_path)
    }
  ),
  active = list(

    #' @field version configure version of currently stored files
    version = function(){
      v = self$data$preprocess_version
      if(is.null(v)){
        v = -1
      }
      v
    },

    #' @field old_version whether settings file is old format
    old_version = function(){
      self$version < 0
    },

    #' @field blocks experiment blocks
    blocks = function(){
      self$data$blocks
    },

    #' @field electrodes electrode numbers
    electrodes = function(){
      private$get_from(c('electrodes', 'channels'))
    },

    #' @field sample_rates voltage data sample rate
    sample_rates = function(){
      # old format
      if(self$old_version){
        srate = private$get_from(c('sample_rate', 'srate'), default = NA)
        return(rep(srate, length(self$electrodes)))
      }
      all_elec = self$electrodes
      sapply(all_elec, function(e){
        x = self$data[[as.character(e)]]
        if(length(x$sample_rate) == 1){
          return(x$sample_rate)
        } else {
          NA
        }
      })
    },

    #' @field notch_filtered whether electrodes are notch filtered
    notch_filtered = function(){
      # NO_SUBJECT = 0
      # HAS_CACHE = 1
      # NOTCH_FILTERED = 2
      # WAVELETED = 3
      # REFERENCED = 4
      all_elec = self$electrodes
      if(self$old_version){
        # old format
        if(isTRUE(self$data$checklevel >= 2)){
          return(rep(TRUE, length(all_elec)))
        } else{
          return(rep(FALSE, length(all_elec)))
        }
      }

      # New format, all electrodes are recorded individually
      vapply(all_elec, function(e){
        x = self$data[[as.character(e)]]
        isTRUE(x$notch_filtered)
      }, FUN.VALUE = FALSE)
    },

    #' @field has_wavelet whether each electrode has wavelet transforms
    has_wavelet = function(){
      # old format
      all_elec = self$electrodes
      if(self$old_version){
        if(isTRUE(self$data$checklevel >= 3)){
          log = self$data$wavelet_log
          if(!length(log)){ return(NULL) }
          return(all_elec %in% log[[length(log)]]$electrodes)
        } else{
          return(rep(FALSE, length(all_elec)))
        }
      }

      # New format, all electrodes are recorded individually
      vapply(all_elec, function(e){
        x = self$data[[as.character(e)]]
        isTRUE(x$has_wavelet)
      }, FUN.VALUE = FALSE)
    },

    #' @field data_imported whether electrodes are imported
    data_imported = function(){
      all_elec = self$electrodes
      if(self$old_version){
        if(isTRUE(private$get_from('checklevel') >= 1)){
          return(rep(TRUE, length(all_elec)))
        } else {
          return(rep(FALSE, length(all_elec)))
        }
      }
      vapply(all_elec, function(e){
        isTRUE(self$data[[as.character(e)]]$data_imported)
      }, FUN.VALUE = FALSE)
    },

    # whether data is imported and one cannot change electrode / blocks / srate anymore?

    #' @field data_locked whether electrode, blocks and sample rate are locked?
    #' usually when an electrode is imported into 'rave', that electrode is
    #' locked
    data_locked = function(){
      if(self$old_version){
        return(isTRUE(isTRUE(private$get_from('checklevel') >= 2)))
      }
      length(self$data_imported) && any(self$data_imported)
    },

    #' @field electrode_locked whether electrode is imported and locked
    electrode_locked = function(){
      all_elec = self$electrodes
      if(self$old_version){
        return(rep(self$data_locked, length(all_elec)))
      }
      vapply(all_elec, function(e){
        isTRUE(self$data[[as.character(e)]]$locked)
      }, FUN.VALUE = FALSE)
    },

    #' @field wavelet_params wavelet parameters
    wavelet_params = function(){
      if(self$old_version){
        log = self$data$wavelet_log
        if(!length(log)){
          return(NULL)
        } else {
          log = log[[length(log)]]
          return(list(
            downsample_to = log$target_srate,
            frequencies = log$frequencies,
            cycle = log$wave_num
          ))
        }
      }

      self$data$wavelet_params

    },

    #' @field notch_params Notch filter parameters
    notch_params = function(){
      if(self$old_version){
        return(list(
          frequencies = c(60,120,180),
          half_bandwidths = c(1,2,2)
        ))
      }

      self$data$notch_params
    },

    #' @field electrode_types electrode types, see \code{type} field in
    #' \code{\link{RAVEAbstarctElectrode}} or \code{\link{LFP_electrode}}
    electrode_types = function(){
      all_elec = self$electrodes
      if(self$old_version){
        return(rep('LFP', length(all_elec)))
      }
      vapply(all_elec, function(e){
        type = self$data[[as.character(e)]]$electrode_type
        if(length(type) != 1){
          return('LFP')
        }
        type
      }, FUN.VALUE = 'LFP')
    },

    #' @field @freeze_blocks whether to free block, internally used
    `@freeze_blocks` = function(){
      tmp <- self$data_imported
      if(length(tmp) && any(tmp)){
        return(TRUE)
      } else {
        return(FALSE)
      }
    },

    #' @field @freeze_lfp whether to free 'LFP' electrodes, internally used
    `@freeze_lfp` = function(){
      is_lfp <- (self$electrode_types == 'LFP')
      if(length(is_lfp) && any(self$data_imported[is_lfp])){
        return(TRUE)
      } else{
        return(FALSE)
      }
    },

    #' @field @lfp_sample_rate 'LFP' electrode sample rates, internally used
    `@lfp_sample_rate` = function(){
      is_lfp <- (self$electrode_types == 'LFP')
      if(length(is_lfp) && any(is_lfp)){
        self$sample_rates[is_lfp][[1]]
      } else {
        NA
      }

    },

    #' @field all_blocks characters, all possible blocks even not included in
    #' some projects
    all_blocks = function(){
      blk <- list.dirs(self$raw_path, full.names = FALSE, recursive = FALSE)
      sort(unique(c(self$blocks, blk)))
    },

    #' @field raw_path raw data path
    raw_path = function(){
      file.path(ravecore::rave_options('raw_data_dir'), self$subject$subject_code)
    }

  )
)
