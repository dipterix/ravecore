#' @export
rave_brain2 <- function(subject, surfaces = 'pial', use_141 = TRUE,
         recache = FALSE, clean_before_cache = FALSE,
         compute_template = FALSE, usetemplateifmissing = FALSE){

  # if subject is NULL, use current loaded subject
  if( is.character( subject ) ){
    subject = as_rave_subject(subject, strict = FALSE)
  }

  # To find freesurfer directory, here are the paths to search
  # 1. rave_data/project/subject/rave/fs
  # 2. rave_data/project/subject/fs
  # 3. rave_data/project/subject/
  # 3. if options('rave.freesurfer_dir') is provided, then XXX/subject/

  fs_path = subject$freesurfer_path

  # load electrodes
  electrode_table = subject$meta_data('electrodes')

  if( is.na(fs_path) ){
    if( !usetemplateifmissing ){
      return(invisible())
    }

    brain = threeBrain::merge_brain()

    brain$set_electrodes(electrodes = electrode_table)

  }else{
    # import from freesurfer folder
    if(recache){
      if( clean_before_cache ){
        fs = list.files(file.path(fs_path, 'RAVE'), pattern = '\\.json$',
                        all.files = FALSE, recursive = FALSE, full.names = TRUE,
                        ignore.case = TRUE, include.dirs = FALSE, no.. = TRUE)
        lapply(fs, unlink)
      }
      threeBrain::import_from_freesurfer(fs_path, subject_name = subject$subject_code)
    }
    brain = threeBrain::freesurfer_brain2(
      fs_subject_folder = fs_path, subject_name = subject$subject_code,
      surface_types = surfaces, use_141 = use_141)

    brain$set_electrodes(electrodes = electrode_table)

    if( compute_template ){
      tf = tempfile()
      new_table = brain$calculate_template_coordinates(save_to = tf)
      if( file.exists(tf) ){
        brain$electrodes$raw_table_path = NULL
        unlink(tf)
        # need to update meta
        save_meta(new_table, meta_type = 'electrodes',
                  project_name = subject$project_name,
                  subject_code = subject$subject_code)
      }
    }
  }


  brain
}