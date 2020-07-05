
#' Function to save meta data to subject
#' @param data data table
#' @param meta_type see load meta
#' @param project_name project name
#' @param subject_code subject code
#' @export
save_meta <- function(data, meta_type, project_name, subject_code){
  data_dir = rave_options('data_dir')
  meta_dir = file.path(data_dir, sprintf('%s/%s', project_name, subject_code), 'rave', 'meta')

  if(!dir.exists(meta_dir)){
    dir.create(meta_dir, recursive = TRUE)
  }

  if(meta_type == 'electrodes'){
    names(data)[1] = c('Electrode')
    if(!'Coord_x' %in% names(data)){
      # try not to overwrite original data
      data$Coord_x = 0
      data$Coord_y = 0
      data$Coord_z = 0
      data$Label = ''
    }

    raveutils::safe_write_csv(data, file = file.path(meta_dir, 'electrodes.csv'), row.names = FALSE)
  }else if(meta_type == 'time_points'){
    names(data) = c('Block', 'Time')
    raveutils::safe_write_csv(data, file = file.path(meta_dir, 'time_points.csv'), row.names = FALSE)
  }else if(meta_type == 'frequencies'){
    names(data) = c('Frequency')
    raveutils::safe_write_csv(data, file = file.path(meta_dir, 'frequencies.csv'), row.names = FALSE)
  }else if(meta_type == 'time_excluded'){
    if(!is.data.frame(data)){
      data = as.data.frame(data, stringsAsFactors = FALSE)
    }
    if(nrow(data)){
      names(data) = c('Block', 'Start', 'End')
      raveutils::safe_write_csv(data, file = file.path(meta_dir, 'time_excluded.csv'), row.names = FALSE)
    }
  }


}

#' Load subject meta data
#' @param meta_type electrodes, epochs, time_points, frequencies, references ...
#' @param project_name project name
#' @param subject_code subject code
#' @param subject_id "project_name/subject_code"
#' @param meta_name only used if meta_type is epochs or references
#' @export
load_meta <- function(meta_type, project_name, subject_code, subject_id, meta_name){
  data_dir = rave_options('data_dir')
  if(missing(subject_id)){
    subject_dir = file.path(data_dir, sprintf('%s/%s', project_name, subject_code), 'rave')
  }else{
    subject_dir = file.path(data_dir, subject_id, 'rave')
  }
  meta_dir = file.path(subject_dir, 'meta')
  if(dir.exists(meta_dir)){
    if(meta_type == 'electrodes'){
      file = file.path(meta_dir, 'electrodes.csv')
      if(file.exists(file)){
        tbl = utils::read.csv(file, stringsAsFactors = FALSE)
        if(!'Label' %in% names(tbl)){
          tbl$Label = NA
        }
        na_labels = is.na(tbl$Label)
        if(any(na_labels)){
          tbl$Label[na_labels] = paste0('Unlabeled', seq_len(sum(na_labels)))
        }

        return(tbl)
      }
    }
    else if(meta_type == 'time_points'){
      file = file.path(meta_dir, 'time_points.csv')
      if(file.exists(file)){
        return(utils::read.csv(
          file,
          stringsAsFactors = FALSE,
          colClasses = c(Block = 'character')
        ))

      }
    }
    else if(meta_type == 'time_excluded'){
      # Read time_excluded.csv if exists
      time_excluded_path = file.path(meta_dir, 'time_excluded.csv')
      if(file.exists(time_excluded_path)){
        return(utils::read.csv(
          time_excluded_path,
          stringsAsFactors = FALSE,
          colClasses = c(Block = 'character')
        ))
      }else{
        return(data.frame(
          Block = NULL,
          Electrode = NULL,
          Start = NULL,
          End = NULL
        ))
      }
    }
    else if(meta_type == 'frequencies'){
      file = file.path(meta_dir, 'frequencies.csv')
      if(file.exists(file)){
        return(utils::read.csv(file, stringsAsFactors = FALSE))
      }
    }
    else if(meta_type == 'epoch'){
      epoch_file = file.path(meta_dir, sprintf('epoch_%s.csv', meta_name))
      if(!length(epoch_file) || !file.exists(epoch_file)){
        return(NULL)
      }
      default_cols = c('Block', 'Time', 'Trial', 'Condition', 'Duration', 'ExcludedElectrodes')

      epochs = utils::read.csv(epoch_file, header = TRUE, stringsAsFactors = FALSE,
                               colClasses = 'character')

      # check blocks
      preprocess_yaml = file.path(meta_dir, '..', 'preprocess', 'rave.yaml')
      if(file.exists(preprocess_yaml)){
        preproc_info = raveutils::load_yaml(preprocess_yaml)
        if(length(preproc_info$blocks)){
          pass_test = TRUE
          # let's check block!
          invalid_blocks = !epochs$Block %in% preproc_info$blocks
          if(any(invalid_blocks)){
            t1 = data.frame(idx = seq_along(epochs$Block), block = epochs$Block, stringsAsFactors = FALSE)
            numeric_blocks = suppressWarnings({ as.numeric(preproc_info$blocks) })
            t2 = data.frame(block = preproc_info$blocks, numblock = numeric_blocks, value = preproc_info$blocks, stringsAsFactors = FALSE)
            t1 = merge(t1, t2, all.x = TRUE, by.x = 'block', by.y = 'block')
            t1 = merge(t1[, c('block', 'idx', 'value')], t2, all.x = TRUE, by.x = 'block', by.y = 'numblock', suffixes = c('1', '2'))
            sel = is.na(t1$value1)
            t1$value1[sel] = t1$value2[sel]

            if(any(is.na(t1$value1))){
              # block cannot find
              # TODO
            }

            epochs$Block = t1$value1[order(t1$idx)]
          }
        }
      }



      epochs$Time = as.numeric(epochs$Time)
      epochs$Trial = as.numeric(epochs$Trial)
      epochs$Duration %?<-% NA
      epochs$Duration = as.numeric(epochs$Duration)

      epochs$Condition %?<-% 'NoCondition'
      epochs$Condition[is.na(epochs$Condition)] = 'NoCondition'
      epochs$Condition = as.character(epochs$Condition)

      epochs$ExcludedElectrodes %?<-% ''
      # sort column orders
      nms = names(epochs)
      nms = c(default_cols, nms[!nms %in% default_cols])
      epochs = epochs[, nms]
      return(epochs)
    }
    else if(meta_type == 'info'){
      info_file = file.path(meta_dir, 'info.yaml')
      info = raveutils::load_yaml(info_file)
      return(as.list(info))
    }
    else if(meta_type == 'time_excluded'){
      file = file.path(meta_dir, 'time_excluded.csv')
      if(!file.exists(file)){
        return(NULL)
      }
      time_excluded = utils::read.csv(file, header = TRUE, stringsAsFactors = FALSE,
                                      colClasses = c('character', 'numeric', 'numeric'))
      return(time_excluded)
    }
    else if(meta_type == 'references'){
      file = file.path(meta_dir, sprintf('reference_%s.csv', meta_name))
      if(!length(file) || !file.exists(file)){
        return(NULL)
      }
      ref_tbl = utils::read.csv(file, header = TRUE, stringsAsFactors = FALSE)
      if(length(names(ref_tbl)) && names(ref_tbl)[1] != 'Electrode'){
        ref_tbl = ref_tbl[,-1]
      }
      return(ref_tbl)
    }
  }

  return(NULL)
}

