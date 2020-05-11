# To load a subject with given reference

load_subject <- function(subject, reference){
  subject = as_rave_subject(subject)

  stopifnot2(isTRUE(reference %in% subject$reference_names), msg = sprintf('Reference %s is missing', reference))

  reference_table = subject$meta_data(meta_type = 'reference', meta_name = reference)

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
    electrodes[[as.character(row$Electrode)]] = e
    NULL
  })

  re = dipsaus::fastmap2()
  re$subject = subject
  re$electrodes = electrodes
  class(re) <- c('RAVERepo', class(re))
}
