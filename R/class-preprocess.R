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
    current_version = 0,
    path = character(0),
    backup_path = character(0),
    data = NULL,
    subject = NULL,
    read_only = TRUE,
    initialize = function(subject, read_only = TRUE){
      self$subject = as_rave_subject(subject, strict = FALSE)
      self$path = file.path(self$subject$preprocess_path, 'rave.yaml')
      self$backup_path = file.path(self$subject$rave_path, 'log.yaml')
      self$read_only = isTRUE(read_only)
      self$data = dipsaus::fastmap2()
      if(file.exists(self$path)){
        raveutils::load_yaml(self$path, map = self$data)
      } else if( file.exists(self$backup_path) ){
        raveutils::load_yaml(self$backup_path, map = self$data)
        dipsaus::list_to_fastmap2(self$data$preprocess, self$data)
      }
      self$migrate()
    },
    valid = function(){
      length(self$data) > 0
    },

    set_electrodes = function(electrodes){
      stopifnot2(!self$data_frozen, msg = 'The data is locked. Cannot change electrodes')
      self$data$electrodes = electrodes
    },

    migrate = function(force = FALSE){
      if(!self$old_version && !force){ return() }
      if( self$version < 0 ){
        if(length(self$data)){
          # raveutils::rave_debug("Migrating from an old format")
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

    electrode_info = function(electrode){
      if(!electrode %in% self$electrodes){
        return()
      }
      self$data[[as.character(electrode)]]
    },

    save = function(){
      raveutils::dir_create(dirname(self$path))
      raveutils::dir_create(dirname(self$backup_path))
      self$data$signiture = rand_string()
      # Make sure the basic information gets saved
      self$migrate(force = TRUE)
      raveutils::save_yaml(self$data, self$path)
      raveutils::save_yaml(list(preprocess = as.list(self$data),
                                preprocess_signiture = self$data$signiture),
                           self$backup_path)
    }
  ),
  active = list(
    version = function(){
      v = self$data$preprocess_version
      if(is.null(v)){
        v = -1
      }
      v
    },
    old_version = function(){
      self$version < 0
    },
    blocks = function(){
      self$data$blocks
    },
    electrodes = function(){
      private$get_from(c('electrodes', 'channels'))
    },
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
    data_locked = function(){
      if(self$old_version){
        return(isTRUE(isTRUE(private$get_from('checklevel') >= 2)))
      }
      length(self$data_imported) && any(self$data_imported)
    },
    electrode_locked = function(){
      all_elec = self$electrodes
      if(self$old_version){
        return(rep(self$data_locked, length(all_elec)))
      }
      vapply(all_elec, function(e){
        isTRUE(self$data[[as.character(e)]]$locked)
      }, FUN.VALUE = FALSE)
    },
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
    notch_params = function(){
      if(self$old_version){
        return(list(
          frequencies = c(60,120,180),
          half_bandwidths = c(1,2,2)
        ))
      }

      self$data$notch_params
    },
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
    }
  )
)
