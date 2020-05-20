
rave_repos = dipsaus::fastmap2()

#' @export
loaded_rave_repository <- function(subject, reference, epoch, before_onset, after_onset){
  repo = RAVERepository$new(subject, reference, epoch, before_onset, after_onset)
  sig = repo$signiture

  # calculate digest and save to rave_repos
  rave_repos[[sig]] %?<-% repo

  rave_repos[[sig]]
}

#' @export
RAVERepository <- R6::R6Class(
  classname = 'RAVERepository',
  cloneable = FALSE,
  private = list(
    power = NULL
  ),
  public = list(
    subject = NULL,
    electrodes = NULL,
    ignored_electrodes = NULL,
    reference_table = NULL,
    time_range = c(NA_real_, NA_real_),
    reference_name = character(0),
    epoch = NULL,
    initialize = function(subject, reference, epoch, before_onset, after_onset){
      stopifnot2(before_onset >= 0, after_onset >= 0, msg = 'Both before_onset and after_onset must be non-negative')
      self$time_range = c(-before_onset, after_onset)

      subject = as_rave_subject(subject)
      self$subject = subject

      epoch = RAVEEpoch$new(subject = subject, name = epoch)
      self$epoch = epoch

      stopifnot2(isTRUE(reference %in% subject$reference_names), msg = sprintf('Reference %s is missing', reference))
      self$reference_name = reference
      reference_table = subject$meta_data(meta_type = 'reference', meta_name = reference)
      self$reference_table = reference_table
      self$ignored_electrodes = reference_table$Electrode[reference_table$Reference == '']

      electrodes = subject$electrodes
      electrode_types = subject$electrode_types

      generators = sapply(unique(electrode_types), function(ty){
        get(sprintf('%s_electrode', ty))
      }, simplify = FALSE, USE.NAMES = TRUE)


      li = lapply(seq_along(electrodes), function(ii){
        e = electrodes[[ii]]
        re = reference_table[reference_table$Electrode == e, ]
        re$Electrode_Type = electrode_types[[ii]]
        re
      })

      reference_electrodes = dipsaus::fastmap2()
      electrodes = dipsaus::fastmap2()

      lapply(li, function(row){
        etype = row$Electrode_Type
        e = generators[[etype]]$new(subject = subject, number = row$Electrode, is_reference = FALSE)

        reference_electrodes$etype %?<-% dipsaus::fastmap2()
        reference_electrodes$etype[[row$Reference]] %?<-% generators[[etype]]$new(
          subject = subject, number = row$Reference, is_reference = TRUE)

        e$set_reference(reference_electrodes$etype[[row$Reference]])
        e$.set_cachename(sprintf('%s-%s', etype, reference))
        e$set_epoch(epoch)

        electrodes[[as.character(row$Electrode)]] = e
        NULL
      })

      self$electrodes = electrodes

    },

    load_brain = function(surfaces = 'pial', use_141 = TRUE,
                          recache = FALSE, clean_before_cache = FALSE,
                          compute_template = FALSE, usetemplateifmissing = FALSE){
      call = match.call()
      call[[1]] = quote(rave_brain2)
      call[['subject']] = quote(self$subject)
      eval(call)
    },

    epoch_power = function(electrodes = NULL){
      if(!length(electrodes)){
        electrodes = self$subject$electrodes
      }
      ignored_electrodes = self$ignored_electrodes
      electrodes = as.integer(electrodes[(!electrodes %in% ignored_electrodes) &
                                           (electrodes %in% self$subject$electrodes)])

      n_elecs <- length(electrodes)
      if(!n_elecs){
        # nothing to load
        raveutils::rave_error('No electrodes to be loaded')
        return(NULL)
      }

      before_onset = -self$time_range[[1]]
      after_onset = self$time_range[[2]]
      dipsaus::make_forked_clusters(rave_options('max_worker'))

      self$clear_memory()

      re <- dipsaus::lapply_async2(electrodes, function(e){
        self$electrodes[[as.character(e)]]$epoch_power(
          before_onset = before_onset,
          after_onset = after_onset
        )
      }, callback = function(e){
        sprintf('Loading electrode %s', e)
      }, future.chunk.size = 1)
      lazyarray::set_lazy_threads(rave_options('max_worker'))
      re[[1]]
    },

    clear_cache = function(){
      # remove temparary folder
      before_onset = -self$time_range[[1]]
      after_onset = self$time_range[[2]]
      tmp = sprintf('pre__%.4f_post_%.4f', before_onset, after_onset)

      nms = names(self$electrodes)
      for(el in nms){
        d = file.path(self$electrodes[[el]]$cache_path, 'power', tmp)
        if( dir.exists(d) ){ unlink(d, recursive = TRUE )}
      }

    },
    clear_memory = function(){
      nms = names(self$electrodes)
      for(el in nms){
        self$electrodes[[el]]$clear_memory()
      }
    }
  ),
  active = list(
    signiture = function(){
      digest::digest(list(
        subject = self$subject$subject_id,
        reference = self$reference_table,
        epoch = self$epoch$table,
        time_range = self$time_range
      ))
    }
  )
)
