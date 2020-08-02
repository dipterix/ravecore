
rave_repos = dipsaus::fastmap2()

#' Create or get \code{\link{RAVERepository}} instance
#' @description If repository has been loaded with the same signature, the
#' loaded one will be returned, otherwise generate a new repository
#' @param subject character or \code{\link{RAVESubject}} instance
#' @param reference reference name
#' @param epoch epoch name
#' @param before_onset seconds to be loaded before trial onset
#' @param after_onset seconds to be loaded after trial onset
#' @return \code{\link{RAVERepository}} instance
#' @seealso \code{\link{RAVERepository}}
#'
#' @examples
#' \dontrun{
#'
#' # Create repository for demo subject 'demo/DemoSubject',
#' # reference is reference_default.csv
#' # epoch is reference_auditory_onset.csv
#' # Time span is -1 ~ 2 (total 3 seconds, 0s is auditory onset)
#' repo <-
#'   loaded_rave_repository(
#'     subject = 'demo/DemoSubject',
#'     reference = 'default',
#'     epoch = 'auditory_onset',
#'     before_onset = 1,
#'     after_onset = 2
#'   )
#'
#' # Epoch and get power data
#' power <- repo$epoch_power()
#'
#' power
#'
#' # get repository in other place instead of creating a new one
#' repo2 <-
#'   loaded_rave_repository(
#'     subject = 'demo/DemoSubject',
#'     reference = 'default',
#'     epoch = 'auditory_onset',
#'     before_onset = 1,
#'     after_onset = 2
#'   )
#'
#' identical(repo2, repo)
#'
#' # remove disk data
#' repo$clear_cache()
#'
#' # release RAM
#' repo$clear_memory()
#'
#' }
#'
#' @export
loaded_rave_repository <- function(subject, reference, epoch, before_onset, after_onset){
  repo = RAVERepository$new(subject, reference, epoch, before_onset, after_onset)
  sig = repo$signature

  # calculate digest and save to rave_repos
  rave_repos[[sig]] %?<-% repo

  rave_repos[[sig]]
}

#' Definition for data repository class
#' @description Do not create instance directly, use
#' \code{\link{loaded_rave_repository}} instead.
#' @export
RAVERepository <- R6::R6Class(
  classname = 'RAVERepository',
  cloneable = FALSE,
  private = list(
    power = NULL
  ),
  public = list(

    #' @field subject \code{\link{RAVESubject}} instance
    subject = NULL,

    #' @field electrodes electrodes loaded
    electrodes = NULL,

    #' @field ignored_electrodes electrodes ignored
    ignored_electrodes = NULL,

    #' @field reference_table reference table
    reference_table = NULL,

    #' @field time_range seconds before and after trial onset
    time_range = c(NA_real_, NA_real_),

    #' @field reference_name name of reference table
    reference_name = character(0),

    #' @field epoch \code{\link{RAVEEpoch}} instance
    epoch = NULL,

    #' @description constructor
    #' @param subject character or \code{\link{RAVESubject}} instance
    #' @param reference reference name
    #' @param epoch epoch name
    #' @param before_onset seconds to be loaded before trial onset
    #' @param after_onset seconds to be loaded after trial onset
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

    #' @description load 3D brain instance
    #' @param surfaces surface types such as \code{'pial'}, \code{'white'},
    #' \code{'smoothwm'}, \code{'sphere'}, etc.
    #' @param use_141 whether to use 'SUMA' 141 standard brain if possible
    #' @param recache whether to force re-calculate cache data
    #' @param clean_before_cache whether to clear data before caching
    #' @param compute_template whether to compute template vertices
    #' @param usetemplateifmissing whether to use template brain if surfaces
    #' are missing, default is false
    #' @return \code{threeBrain} brain instance
    #' @seealso \code{\link{rave_brain2}}, \code{\link[threeBrain]{freesurfer_brain2}}
    load_brain = function(surfaces = 'pial', use_141 = TRUE,
                          recache = FALSE, clean_before_cache = FALSE,
                          compute_template = FALSE, usetemplateifmissing = FALSE){
      call = match.call()
      call[[1]] = quote(rave_brain2)
      call[['subject']] = quote(self$subject)
      eval(call)
    },

    #' @description epoch power data
    #' @param electrodes electrodes to epoch
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
        rave_error('No electrodes to be loaded')
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

    #' @description call \code{clear_cache} for each electrodes loaded
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

    #' @description call \code{clear_memory} for each electrodes loaded
    clear_memory = function(){
      nms = names(self$electrodes)
      for(el in nms){
        self$electrodes[[el]]$clear_memory()
      }
    }
  ),
  active = list(

    #' @field signature unique signature for repository; if subject ID,
    #' reference table, epoch table, and time range are the same, signatures
    #' will be the same
    signature = function(){
      digest::digest(list(
        subject = self$subject$subject_id,
        reference = self$reference_table,
        epoch = self$epoch$table,
        time_range = self$time_range
      ))
    }
  )
)
