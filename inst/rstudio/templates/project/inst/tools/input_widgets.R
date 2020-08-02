define_input_multiple_electrodes <- function(inputId, label = 'Electrodes'){
  quo = rlang::quo({
    define_input(
      definition = textInput(!!inputId, !!label, value = "", placeholder = '1-5,8,11-20'),
      init_args = c('label', 'value'),
      init_expr = {

        electrodes = global_data$loaded_electrodes


        last_input = raveio::get_val2(session_data,
                                         key = !!inputId,
                                         default = as.character(electrodes[1]),
                                         min_len = 1, max_len = 1)
        e = dipsaus::parse_svec(last_input)
        e = e[e %in% electrodes]
        if(!length(e)){
          e = electrodes[1]
        }
        value = dipsaus::deparse_svec(e)
        label = paste0(!!label, ' (currently loaded: ' , value , ')')
      }
    )
  })

  parent_frame = parent.frame()

  dipsaus::eval_dirty(quo, env = parent_frame)
}


define_input_single_electrode <- function(inputId, label = 'Electrode'){
  quo = rlang::quo({
    define_input(
      definition = selectInput(!!inputId, !!label, choices = '', selected = NULL, multiple = FALSE),
      init_args = c('choices', 'selected'),
      init_expr = {

        electrodes = global_data$loaded_electrodes
        choices = as.character(electrodes)

        selected = get_val2(session_data, !!inputId, default = electrodes[1])
        selected = as.character(selected)

        if(length(selected) != 1 || !selected %in% choices){
          selected = choices[1]
        }
      }
    )
  })

  parent_frame = parent.frame()

  dipsaus::eval_dirty(quo, env = parent_frame)
}



define_input_frequency <- function(inputId, label = 'Frequency', is_range = TRUE, round = -1, initial_value = NULL){

  if(is_range){
    v = c(1,200)
  }else{
    v = 1
  }

  quo = rlang::quo({
    define_input(
      definition = sliderInput(!!inputId, !!label, min = 1, max = 200, value = !!v, round = !!round),
      init_args = c('min', 'max', 'value'),
      init_expr = {
        freq_range = range(global_data$repository$subject$preprocess_settings$wavelet_params$frequencies)
        min = floor(freq_range[1])
        max = ceiling(freq_range[2])
        initial_value = !!initial_value
        if(!!is_range){
          initial_value %?<-% c(min, max)
        }else{
          initial_value %?<-% min
        }
        value = get_val2(session_data, !!inputId, initial_value)
        value %?<-% !!initial_value
        value %?<-% if(!!is_range) c(min, max) else min
      }
    )
  })

  parent_frame = parent.frame()

  dipsaus::eval_dirty(quo, env = parent_frame)
}


define_input_time <- function(inputId, label = 'Time Range', is_range = TRUE, round = -2, initial_value = NULL){
  if(is_range){
    v = c(0,1)
  }else{
    v = 0
  }

  quo = rlang::quo({

    define_input(
      definition = sliderInput(!!inputId, !!label, min = 0, max = 1, value = !!v, step = 0.01, round = !!round),
      init_args = c('min', 'max', 'value'),
      init_expr = {
        time_range <- global_data$repository$time_range

        min = min(time_range[1])
        max = max(time_range[2])
        initial_value = !!initial_value

        if(!!is_range){
          initial_value %?<-% c(min, max)
        }else{
          initial_value %?<-% min
        }
        value = get_val2(session_data, !!inputId, initial_value)
        value %?<-% !!initial_value
        value %?<-% if (!!is_range) c(min, max) else 0
      }
    )
  })

  parent_frame = parent.frame()

  dipsaus::eval_dirty(quo, env = parent_frame)
}



define_input_auto_recalculate <- function(inputId, label,
                                          type = c('checkbox', 'button'),
                                          button_type = 'primary',
                                          default_on = FALSE){
  type = match.arg(type)

  quo = rlang::quo({

    if(!!(type == 'checkbox')){
      define_input(shiny::checkboxInput(!!inputId, label = !!label, value = !!default_on))

      load_scripts(rlang::quo({

        if(isFALSE(!!default_on)){
          ravecore::auto_recalculate(FALSE)
        }

        observeEvent(input[[!!inputId]], {
          is_on = input[[!!inputId]]
          ravecore::auto_recalculate(isTRUE(is_on))
          reactive_data$..rave_auto_recalculate.. = if(is_on) Sys.time() else FALSE
        }, priority = 999999L)
      }))

    } else {
      define_input(dipsaus::actionButtonStyled(
        !!inputId, label = !!label, value = !!default_on,
        type = !!button_type, width = '100%', icon = shiny::icon('arrow-right')))

      load_scripts(rlang::quo({
        observeEvent(reactive_data$..rave_auto_recalculate.., {
          assign('aaaa', environment(), envir = globalenv())

          if(isFALSE(reactive_data$..rave_auto_recalculate..)){
            dipsaus::updateActionButtonStyled(session, !!inputId, disabled = FALSE)
          } else {
            dipsaus::updateActionButtonStyled(session, !!inputId, disabled = TRUE)
          }
        }, priority = 999998L, ignoreNULL = TRUE)
        observeEvent(input[[!!inputId]], {
          ravecore::auto_recalculate(1)
        }, priority = 999997L)
      }))
    }
  })
  parent_env = parent.frame()
  dipsaus::eval_dirty(quo, env = parent_env)


}







define_input_condition_groups <- function(
  inputId, label = 'Group', initial_groups = 1, max_group = 10, min_group = 1,
  label_color = rep('black', max_group), init_args, init_expr, quoted = FALSE, ...){

  if(missing(init_args)){
    init_args = c('initialization', 'value')
  }
  # dipsaus::registerCompoundInput2()

  if(missing(init_expr)){
    init_expr = rlang::quo({

      cond = unique(global_data$repository$epoch$table$Condition)

      initialization = list(
        group_conditions = list(
          choices = cond
        )
      )

      default_val = list(
        list(
          group_name = 'All Conditions',
          group_conditions = cond
        )
      )

      value = raveio::get_val2(session_data, key = !!inputId, default = default_val, min_len = 1)
      if( !length(value) || !length(value[[1]]$group_conditions) ||
          !any(value[[1]]$group_conditions %in% cond)){
        value = default_val
      }
    })
    init_expr = rlang::quo_squash(init_expr)
  }else if (!quoted){
    init_expr = substitute(init_expr)
  }

  quo = rlang::quo({

    define_input(
      definition = dipsaus::compoundInput2(
        inputId = !!inputId, label = !!label, inital_ncomp = !!initial_groups,
        components = shiny::div(
          textInput('group_name', 'Name', value = '', placeholder = 'Condition Name'),
          selectInput('group_conditions', ' ', choices = '', multiple = TRUE)
        ),
        label_color = !!label_color, max_ncomp = !!max_group, min_group = !!min_group
      ),

      init_args = !!init_args,

      init_expr = eval(!!init_expr)
    )
  })

  parent_frame = parent.frame()

  dipsaus::eval_dirty(quo, env = parent_frame)
}






# options to save and load analysis parameters
define_input_analysis_yaml_chooser <- function(
  inputId, name_prefix = 'settings_',
  # Relative to project directory
  read_path, write_path = read_path,
  labels = c('Save settings', 'Load settings')
){
  save_btn = paste0(inputId, '_save')
  load_btn = paste0(inputId, '_load')
  save_text = paste0(inputId, '_savename')
  do_save = paste0(inputId, '_do_save')
  quo = rlang::quo({
    load_scripts(rlang::quo({
      assign(!!inputId, function(){
        project_name = global_data$repository$subject$project_name
        data_dir = rave_options('data_dir')

        defaultPath = do.call(file.path, as.list(c(project_name, '_project_data', !!read_path)))
        dir.create(file.path(data_dir, defaultPath), showWarnings = FALSE, recursive = TRUE)
        defaultPath = normalizePath(defaultPath)
        shinyFiles::shinyFileChoose(
          input = input,
          id = !!load_btn, roots= c('RAVE Home' = normalizePath(data_dir), 'root' = '/'),
          filetypes = c('yaml', 'yml'), defaultRoot = 'RAVE Home',
          defaultPath = defaultPath
        )

        div(
          class = 'rave-grid-inputs', style='border:none',
          div(
            style = 'flex-basis:50%',
            dipsaus::actionButtonStyled(inputId = ns(!!save_btn),
                               label=!!labels[[1]], icon = shiny::icon('save'), width = '100%')
          ),
          div(
            style = 'flex-basis:50%',
            shinyFiles::shinyFilesButton(id = ns(!!load_btn), label = !!labels[[2]], title = 'Select Analysis Settings',
                                         multiple = FALSE, icon = shiny::icon('puzzle-piece'), style = 'width:100%')
          )
        )
      })

      if(dipsaus::shiny_is_running()){
        local({
          input <- ravecore::getDefaultReactiveInput()
          save_inputs <- function(yaml_path, variables_to_export){
            cache_list = shiny::isolate(shiny::reactiveValuesToList(input))
            if(!missing(variables_to_export)) {
              cache_list =cache_list[variables_to_export]
            }
            raveio::save_yaml(x = cache_list, file = yaml_path)
            return(TRUE)
          }

          # save Modal pop up
          observeEvent(input[[!!save_btn]], {
            tstmp <- strftime(Sys.time(), format = '%Y-%h-%d')

            shiny::showModal(shiny::modalDialog(
              title = 'Save Analysis Settings',
              size = 's',
              easyClose = TRUE,
              shiny::textInput(ns(!!save_text), label = 'Settings Name', value = paste0(!!name_prefix, tstmp)),
              shiny::tags$small('Will overwrite settings with the same name currently in RAVE settings folder'),
              footer = tagList(
                dipsaus::actionButtonStyled(ns(!!do_save), 'Save'),
                shiny::modalButton("Cancel")
              )
            ))
          })

          # Modal do save
          observeEvent(input[[!!do_save]], {
            # save
            fname = input[[!!save_text]]
            fname = stringr::str_replace_all(fname, '[^a-zA-Z0-9]+', '_')
            fname = paste0(fname, '.yaml')
            save_dir = do.call(file.path, as.list(c(normalizePath(global_data$project$path, mustWork = TRUE), '_project_data', !!write_path)))
            dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
            save_inputs(file.path(save_dir, fname))
            shiny::removeModal()
          })
        })
      }

    }))

    define_input(customizedUI(inputId = !!inputId))

  })

  parent_env = parent.frame()
  eval_dirty(quo, env = parent_env)
}









