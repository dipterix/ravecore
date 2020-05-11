#' @export
RAVEEpoch <- R6::R6Class(
  classname = 'RAVEEpoch',
  lock_objects = FALSE,
  private = list(
    basic_columns = c("Block", "Time", "Trial", "Condition")
  ),
  public = list(
    name = character(0),
    subject = NULL,

    data = NULL,
    table = NULL,

    # extra columns
    .columns = character(0),

    initialize = function(subject, name){
      stopifnot2(stringr::str_detect(name, '^[a-zA-Z0-9_]'),
                 msg = 'epoch name can only contain letters[a-zA-Z] digits[0-9] and underscore[_]')

      self$subject = as_rave_subject(subject)
      self$name = name

      self$data = dipsaus::fastmap2()

      if(name %in% self$subject$epoch_names){
        # load epoch
        table = self$subject$meta_data('epoch', name)
        for(ii in seq_len(nrow(table))){
          row = table[ii,]
          self$data[[as.character(row$Trial)]] = row
        }

        self$.columns = names(table)
        self$.columns = self$.columns[!self$.columns %in% c(private$basic_columns, 'X')]
        self$.columns = self$.columns[!stringr::str_detect(self$.columns, '^X\\.[0-9]+$')]
      }
      self$update_table()

    },

    trial_at = function(i, df = TRUE){
      re = as.list(self$data[[as.character(i)]])
      re = sapply(self$columns, function(nm){
        re = re[[nm]]
        if(!length(re) || is.na(re)){
          return(NA)
        }
        if(stringr::str_detect(nm, '^Event_.+$')){
          re = as.numeric(re)
        }
        re
      }, simplify = FALSE, USE.NAMES = TRUE)
      if(df){
        re = as.data.frame(re, stringsAsFactors = FALSE)
      }
      re
    },

    update_table = function(){
      self$table = do.call('rbind', lapply(self$trials, self$trial_at, df = TRUE))
    },

    set_trial = function(Block, Time, Trial, Condition, ...){
      stopifnot2(Block %in% self$subject$blocks, msg = 'invalid block')
      self$data[[as.character(Trial)]] = list(Block = Block, Time = Time, Trial = Trial, Condition = Condition, ...)
      names(list(...))
      more_cols = setdiff(names(list(...)), self$.columns)
      if(length(more_cols)){
        self$.columns = c(self$.columns, more_cols)
      }
      self$trial_at(Trial)
    }

  ),
  active = list(
    columns = function(){
      unique(c(private$basic_columns, self$.columns))
    },
    n_trials = function(){
      length(self$data)
    },
    trials = function(){
      sort(as.integer(names(self$data)))
    }
  )
)