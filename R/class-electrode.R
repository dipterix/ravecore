#' @export
RAVEAbstarctElectrode <- R6::R6Class(
  classname = 'RAVEAbstarctElectrode',
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    reference_cachename = character(0)
  ),
  public = list(
    type = 'Electrode',
    subject = NULL,
    number = NULL,
    reference = NULL,
    epoch = NULL,
    cached_reference = NULL,
    is_reference = FALSE,

    .set_cachename = function(name){
      private$reference_cachename = name
    },

    initialize = function(subject, number, is_reference = FALSE){
      self$subject = ravecore::as_rave_subject(subject)
      self$number = number
      self$reference = NULL
      self$epoch = NULL
      private$reference_cachename = rand_string(6)

      # load cached references
      cache_table = file.path(self$subject$cache_path, 'cached_reference.csv')
      if(file.exists(cache_table)){
        cache_table = raveutils::safe_read_csv(cache_table)
        self$cached_reference = cache_table$Reference[cache_table$Electrode == number]
      } else {
        raveutils::rave_error('Cannot find cached_reference.csv')
      }

    },
    .set_reference = function(reference, type){
      if(missing(type)){
        type = self$type
      }
      stopifnot2(
        is.null(reference) || (
          inherits(reference, 'RAVEAbstarctElectrode') &&
            reference$type == type
        ),
        msg = sprintf('set_reference must receive a %s electrode', sQuote(type))
      )

      self$reference = reference
    },
    set_epoch = function(epoch){
      if(!inherits(epoch, 'RAVEEpoch')){
        epoch = RAVEEpoch$new(subject = self$subject, name = epoch)
      }
      self$epoch = epoch
    },

    clear_cache = function(...){},
    clear_memory = function(...){}

  ),
  active = list(
    exists = function(){
      self$number %in% self$subject$electrodes
    },
    preprocess_file = function(){
      file.path(self$subject$preprocess_path, sprintf('electrode_%s.h5', self$number))
    },
    power_file = function(){
      file.path(self$subject$data_path, 'power', sprintf('%s.h5', self$number))
    },
    power_cached = function(){
      if(is.null(self$reference) || self$reference$number == 'noref'){
        # check if raw exists
        file.path(self$subject$cache_path, 'power', 'raw')
      }
      self$cached_reference
    },
    phase_file = function(){
      file.path(self$subject$data_path, 'phase', sprintf('%s.h5', self$number))
    },
    voltage_file = function(){
      file.path(self$subject$data_path, 'voltage', sprintf('%s.h5', self$number))
    },
    reference_name = function(){
      if(is.null(self$reference)){
        'noref'
      } else {
        ref = stringr::str_remove_all(self$reference$number, '(\\.h5$)|(^ref_)')
        sprintf('ref_%s', ref)
      }
    },
    cache_path = function(){
      if(!length(self$epoch)){
        return(NA)
      }
      cache_path = rave_options('cache_path')
      # save to cache_path/project/subject/epoch/cachename
      # cachename = reference + elec type
      file.path(cache_path, self$subject$project_name,
                self$subject$subject_code, self$epoch$name,
                private$reference_cachename)

    }
  )
)

#' @export
LFP_electrode <- R6::R6Class(
  classname = 'LFP_electrode',
  inherit = RAVEAbstarctElectrode,
  lock_class = TRUE,
  private = list(
    persisted_voltage_unref = NULL,
    persisted_power_unref = NULL,
    persisted_phase_unref = NULL,
    persisted_coef_ref = NULL,
    referenced_power_cache_file = character(0)
  ),
  public = list(
    type = 'LFP',
    set_reference = function(reference){
      self$.set_reference(reference)
    },

    initialize = function(subject, number, is_reference = FALSE){
      super$initialize(subject, number, is_reference)
      if(is_reference){
        # this is a reference electrode
        self$is_reference = TRUE
        ref_electrodes = stringr::str_match(number, 'ref_([0-9\\-,\\ ]+)')[,2]

        # no reference value, 'noref'
        if(is.na(ref_electrodes)){
          ref_electrodes = ''
        }

        e = dipsaus::parse_svec(ref_electrodes)
        if(length(e) == 0){
          self$number = 'noref'
        } else {
          if(length(e) == 1){
            self$number = e
          } else {
            # check subject reference directory
            self$number = sprintf('ref_%s.h5', ref_electrodes)
            if(!file.exists(file.path(self$subject$reference_path, self$number))){
              raveutils::rave_warn("Reference file {self$number} is missing")
            }
          }
        }
      }

    },

    # data method
    load_unreferenced_voltage = function(block, persist = FALSE){
      stopifnot2(block %in% self$subject$blocks, msg = sprintf(
        'Block %s doesn not exists', block
      ))
      stopifnot2(self$exists, msg = sprintf('Electrode %s is invalid', self$number))

      if(persist){
        if(!is.null(private$persisted_voltage_unref)){
          return(private$persisted_voltage_unref)
        }
      }


      if(is.numeric(self$number)){
        # load from data
        fst_path = file.path(self$subject$cache_path, 'voltage', 'raw', block, self$fst_fname)
        if(!file.exists(fst_path) && isTRUE(self$cached_reference == 'noref')){
          fst_path = file.path(self$subject$cache_path, 'voltage', 'ref', block, self$fst_fname)
        }
        h5_path = file.path(self$subject$data_path, 'voltage', self$h5_fname)
        h5_name = sprintf('/raw/voltage/%s', block)

        re = raveutils::load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        if(self$number == 'noref'){
          return(0)
        }
        # load from reference folder
        h5_path = file.path(self$subject$reference_path, self$h5_fname)
        h5_name = sprintf('/voltage/%s', block)
        re = raveutils::load_h5(h5_path, h5_name, read_only = TRUE, ram = FALSE)
      }

      if(persist){
        private$persisted_voltage_unref = re[]
        return(private$persisted_voltage_unref)
      }

      re

    },
    load_unreferenced_power = function(block, persist = FALSE){
      stopifnot2(block %in% self$subject$blocks, msg = sprintf(
        'Block %s doesn not exists', block
      ))
      stopifnot2(self$exists, msg = sprintf('Electrode %s is invalid', self$number))

      if(persist){
        if(!is.null(private$persisted_power_unref)){
          return(private$persisted_power_unref)
        }
      }

      if(is.numeric(self$number)){
        # load from data
        fst_path = file.path(self$subject$cache_path, 'power', 'raw', block, self$fst_fname)
        if(!file.exists(fst_path) && isTRUE(self$cached_reference == 'noref')){
          fst_path = file.path(self$subject$cache_path, 'power', 'ref', block, self$fst_fname)
        }
        h5_path = file.path(self$subject$data_path, 'power', self$h5_fname)
        h5_name = sprintf('/raw/power/%s', block)

        re = raveutils::load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        if(self$number == 'noref'){
          return(0)
        }
        if(!persist){
          raveutils::rave_fatal('load_unreferenced_power is not for reference_electrode unless persist is TRUE')
        }

        # load from reference folder
        h5_path = file.path(self$subject$reference_path, self$h5_fname)
        h5_name = sprintf('/wavelet/coef/%s', block)
        re = raveutils::load_h5(h5_path, h5_name, read_only = TRUE, ram = TRUE)

        re = re[,,1, drop = FALSE] ^ 2
        dim(re) = dim(re)[1:2]
        private$persisted_power_unref = re
        return(re)

      }

      if(persist){
        private$persisted_power_unref = re[]
        return(private$persisted_power_unref)
      }

      re

    },
    load_unreferenced_phase = function(block, persist = FALSE){
      stopifnot2(block %in% self$subject$blocks, msg = sprintf(
        'Block %s doesn not exists', block
      ))
      stopifnot2(self$exists, msg = sprintf('Electrode %s is invalid', self$number))

      if(persist){
        if(!is.null(private$persisted_phase_unref)){
          return(private$persisted_phase_unref)
        }
      }

      if(is.numeric(self$number)){
        # load from data
        fst_path = file.path(self$subject$cache_path, 'phase', 'raw', block, self$fst_fname)
        if(!file.exists(fst_path) && isTRUE(self$cached_reference == 'noref')){
          fst_path = file.path(self$subject$cache_path, 'phase', 'ref', block, self$fst_fname)
        }
        h5_path = file.path(self$subject$data_path, 'phase', self$h5_fname)
        h5_name = sprintf('/raw/phase/%s', block)

        re = raveutils::load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        if(self$number == 'noref'){
          return(0)
        }
        if(!persist){
          raveutils::rave_fatal('load_unreferenced_phase is not for reference_electrode unless persist is TRUE')
        }

        # load from reference folder
        h5_path = file.path(self$subject$reference_path, self$h5_fname)
        h5_name = sprintf('/wavelet/coef/%s', block)
        re = raveutils::load_h5(h5_path, h5_name, read_only = TRUE, ram = TRUE)

        re = re[,,2, drop = FALSE]
        dim(re) = dim(re)[1:2]
        private$persisted_phase_unref = re
        return(re)

      }

      if(persist){
        private$persisted_phase_unref = re[]
        return(private$persisted_phase_unref)
      }

      re

    },

    # reference
    reference_power = function(block){
      # check whether the reference has been cached
      stopifnot2(!self$is_reference, msg = 'Cannot reference a reference ;D')
      if(is.null(self$reference) || isTRUE(self$reference$number == 'noref') ){
        # noref!
        return(self$load_unreferenced_power(block, persist = FALSE)[])
      }
      # check whether cached
      has_cached = self$reference_equals_cached

      if( has_cached ){
        fst_path = file.path(self$subject$cache_path, 'power', 'ref', block, self$fst_fname)
        h5_path = file.path(self$subject$data_path, 'power', self$h5_fname)
        h5_name = sprintf('/ref/power/%s', block)
        re = raveutils::load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        # calculate reference by yourself
        power = self$load_unreferenced_power(block = block, persist = FALSE)
        phase = self$load_unreferenced_phase(block = block, persist = FALSE)
        ref_power = self$reference$load_unreferenced_power(block = block, persist = TRUE)
        ref_phase = self$reference$load_unreferenced_phase(block = block, persist = TRUE)

        coef = sqrt(power[]) * exp(phase[] * 1i) - sqrt(ref_power) * exp(ref_phase * 1i)
        re = Mod(coef) ^ 2
      }

      re
    },
    reference_phase = function(block){
      # check whether the reference has been cached
      stopifnot2(!self$is_reference, msg = 'Cannot reference a reference ;D')
      if(is.null(self$reference) || isTRUE(self$reference$number == 'noref') ){
        # noref!
        return(self$load_unreferenced_phase(block, persist = FALSE)[])
      }
      has_cached = self$reference_equals_cached

      if( has_cached ){
        fst_path = file.path(self$subject$cache_path, 'phase', 'ref', block, self$fst_fname)
        h5_path = file.path(self$subject$data_path, 'phase', self$h5_fname)
        h5_name = sprintf('/ref/phase/%s', block)
        re = raveutils::load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        # calculate reference by yourself
        power = self$load_unreferenced_power(block = block, persist = FALSE)
        phase = self$load_unreferenced_phase(block = block, persist = FALSE)
        ref_power = self$reference$load_unreferenced_power(block = block, persist = TRUE)
        ref_phase = self$reference$load_unreferenced_phase(block = block, persist = TRUE)

        coef = sqrt(power[]) * exp(phase[] * 1i) - sqrt(ref_power) * exp(ref_phase * 1i)
        re = Arg(coef)
      }

      re
    },
    reference_voltage = function(block){
      # check whether the reference has been cached
      stopifnot2(!self$is_reference, msg = 'Cannot reference a reference ;D')
      if(is.null(self$reference) || isTRUE(self$reference$number == 'noref') ){
        # noref!
        return(self$load_unreferenced_voltage(block, persist = FALSE)[])
      }
      has_cached = self$reference_equals_cached

      if( has_cached ){
        fst_path = file.path(self$subject$cache_path, 'voltage', 'ref', block, self$fst_fname)
        h5_path = file.path(self$subject$data_path, 'voltage', self$h5_fname)
        h5_name = sprintf('/ref/voltage/%s', block)
        re = raveutils::load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        # calculate reference by yourself
        voltage = self$load_unreferenced_voltage(block = block, persist = FALSE)
        ref_voltage = self$reference$load_unreferenced_voltage(block = block, persist = TRUE)
        re = voltage-ref_voltage
      }

      re
    },

    is_power_cached = function(before_onset, after_onset){
      stopifnot2(inherits(self$epoch, 'RAVEEpoch'), msg = 'You need to set electrode epoch first')
      preproc = self$preprocess_info
      stopifnot2(isTRUE(preproc$has_wavelet), msg = sprintf('Electrode %s does not have power data', self$number))
      power_srate = wave_info$downsample_to
      tidx = seq.int(- ceiling(before_onset * power_srate), ceiling(after_onset * power_srate))

      dim = rep(NA_integer_, 4L)
      dim[[1]] = self$epoch$n_trials
      dim[[2]] = length(wave_info$frequencies)
      dim[[3]] = length(tidx)
      dim[[4]] = length(self$subject$electrodes)

      tmp = sprintf('pre__%.4f_post_%.4f', before_onset, after_onset)
      fst_cache_dir = file.path(self$cache_path, 'power', tmp)
      fst_cache_file = file.path(fst_cache_dir, self$fst_fname)
      fst_cache_meta = file.path(fst_cache_dir, 'lazyarray.meta')
      if(file.exists(fst_cache_file) && file.exists(fst_cache_meta)){
        arr <- lazyarray::load_lazyarray(fst_cache_dir, read_only = TRUE)
        return(all(dim(arr) - dim == 0))
      } else {
        return(FALSE)
      }
    },

    # epoch
    epoch_power = function(before_onset, after_onset){
      stopifnot2(inherits(self$epoch, 'RAVEEpoch'), msg = 'You need to set electrode epoch first')
      preproc = self$preprocess_info
      stopifnot2(isTRUE(preproc$has_wavelet), msg = sprintf('Electrode %s does not have power data', self$number))
      wave_info = self$subject$preprocess_settings$wavelet_params
      power_srate = wave_info$downsample_to

      tbl = self$epoch$table
      trials = self$epoch$trials
      blk = unique(tbl$Block)

      tidx = seq.int(- ceiling(before_onset * power_srate), ceiling(after_onset * power_srate))

      tmp = sprintf('pre__%.4f_post_%.4f', before_onset, after_onset)
      dir = file.path(self$cache_path, 'power', tmp)
      if(!dir.exists(dir)){
        # clculate dimension
        dim = rep(NA_integer_, 4L)
        dim[[1]] = self$epoch$n_trials
        dim[[2]] = length(wave_info$frequencies)
        dim[[3]] = length(tidx)
        dim[[4]] = length(self$subject$electrodes)
        dimnames = list(
          Trial = trials,
          Frequency = wave_info$frequencies,
          Time = tidx / power_srate,
          Electrode = self$subject$electrodes
        )
        tryCatch({
          lazyarray::create_lazyarray(path = dir, storage_format = 'double', dim = dim,
                                      dimnames = dimnames, multipart = TRUE, prefix = '',
                                      compress_level = 0L, multipart_mode = 1L)
        }, error = function(e){
          raveutils::rave_debug('Cache path already exists - {dir}')
          lazyarray::load_lazyarray(path = dir, read_only = FALSE)
        })
      }
      array = lazyarray::load_lazyarray(path = dir, read_only = FALSE)
      dim = dim(array)
      electrode_idx = which(self$subject$electrodes == self$number)[[1]]

      private$referenced_power_cache_file = array$get_partition_fpath(electrode_idx, full_path = TRUE)
      if(file.exists(private$referenced_power_cache_file)){
        # temporarily cached
        catgl('Partition exists')
        return(lazyarray::load_lazyarray(path = dir, read_only = TRUE))
      }
      # else epoch
      results = array(NA_real_, dim[c(1,2,3)])

      for(b in blk){
        ref_power = self$reference_power(b)

        # find which trials in the block
        sel = tbl$Block %in% b
        sub_tbl = tbl[sel, ]
        slice_idx = vapply(seq_len(nrow(sub_tbl)), function(ii){
          row = sub_tbl[ii,]
          t_pos = round(row$Time * power_srate)
          as.integer(t_pos + tidx)
        }, FUN.VALUE = tidx)
        # freq x time x trial
        ref_power <- ref_power[, as.vector(slice_idx)]
        dim(ref_power) <- c(dim[c(2, 3)], sum(sel))
        results[sel,,] = aperm(ref_power, c(3,1,2))
      }

      # write to array
      array[,,,electrode_idx] = results
      return(lazyarray::load_lazyarray(path = dir, read_only = TRUE))

    },
    clear_cache = function(...){
      f = private$referenced_power_cache_file
      if(length(f) && file.exists(f)){
        unlink(f)
      }
    },
    clear_memory = function(...){
      private$persisted_voltage_unref = NULL
      private$persisted_power_unref = NULL
      private$persisted_phase_unref = NULL
      private$persisted_coef_ref = NULL
      if(inherits(self$reference, 'RAVEAbstarctElectrode')){
        self$reference$clear_memory()
      }
    }

  ),
  active = list(
    # file exists?
    exists = function(){
      if(!self$is_reference || is.numeric(self$number)){
        super$exists
      } else if( isTRUE(self$number == 'noref') ) {
        return(TRUE)
      } else {
        file.exists(file.path(self$subject$reference_path, self$number))
      }
    },

    h5_fname = function(){
      if(is.character(self$number)){
        self$number
      } else {
        sprintf('%s.h5', self$number)
      }
    },
    fst_fname = function(){
      if(is.character(self$number)){
        self$number
      } else {
        sprintf('%s.fst', self$number)
      }
    },

    reference_equals_cached = function(){
      if(self$is_reference){
        return(TRUE)
      }

      ref = self$reference$number
      if(is.numeric(ref)){
        ref = sprintf('ref_%.0f', ref)
      }
      has_cached = any(sprintf('%s%s', self$cached_reference, c('', '.h5')) %in% ref)
      has_cached
    },

    # valid?
    valid = function(){
      if(!self$exists) {return(FALSE)}
      if(self$is_reference) {return(TRUE)}
      elec = self$subject$electrodes
      if(!self$number %in% elec){ return(FALSE) }
      # type matches with subject
      if(!isTRUE(self$subject$electrode_types[elec %in% self$number] == self$type)){
        return(FALSE)
      }
      return(TRUE)
    },
    raw_sample_rate = function(){
      elec = self$subject$electrodes
      srate = self$subject$raw_sample_rates[elec %in% self$number]
      if(!length(srate)){
        srate = NA
      }
      srate
    },
    power_sample_rate = function(){
      elec = self$subject$electrodes
      srate = self$subject$power_sample_rate[elec %in% self$number]
      if(!length(srate)){
        srate = NA
      }
      srate
    },
    preprocess_info = function(){
      self$subject$preprocess_settings$electrode_info(electrode = self$number)
    }

  )
)
