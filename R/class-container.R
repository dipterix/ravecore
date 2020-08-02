
RAVEContainer <- R6::R6Class(
  classname = 'RAVEContainer',
  cloneable = FALSE,
  portable = FALSE,
  parent_env = asNamespace('ravecore'),
  lock_objects = FALSE, # FIXME
  private = list(
    check_fun = NULL,
    loader_interface = NULL,
    onloader_action = NULL,
    onload_variables = NULL,
    loader_info = NULL,

    error_list = list(),
    observer_list = NULL,
    sneaky_observe = NULL,
    sneaky_observeEvent = NULL

  ),
  public = list(
    module = NULL,

    # environments
    wrapper_env = NULL,
    static_env = NULL,
    mask_env = NULL,
    runtime_env = NULL,

    # observers
    user_observers = NULL,

    # scripts/components
    dynamic_script = list(), # loaded via "load_script"
    init_script = list(),
    input_components = list(),
    input_layout = list(),
    output_components = list(),
    output_layout = list(),
    main_functions = list(),
    .data_validation = NULL,  # stores validation expression
    .has_data = FALSE,
    auto_run = Inf,

    container_data = NULL,
    container_reactives = NULL,

    .data_selector_opened = FALSE,

    open_data_selector = function(session = shiny::getDefaultReactiveDomain()){
      rave_debug('Opening data selector')
      session$sendCustomMessage(type = 'rave_add_class',message = list(
        selector = sprintf('.rave-module-container[rave-module="%s"]', self$module_id),
        class = 'open'
      ))
      session$sendCustomMessage(type = 'rave_remove_class',message = list(
        selector = '#__rave__mask__',
        class = 'hidden'
      ))
      # trigger input change
      self$container_reactives$..rave_import_data_ui_show.. <- Sys.time()
      # dipsaus::set_shiny_input(session = session, inputId = '..rave_import_data_ui_show..',
      #                          value = Sys.time(), priority = 'event')

      self$data_selector_opened <- TRUE
    },

    close_data_selector = function(session = shiny::getDefaultReactiveDomain()){
      # session$sendCustomMessage(type = 'rave_add_class',message = list(
      #   selector = 'body',
      #   class = 'sidebar-collapse'
      # ))
      session$sendCustomMessage(type = 'rave_remove_class',message = list(
        selector = sprintf('.rave-module-container[rave-module="%s"]', self$module_id),
        class = 'open'
      ))
      session$sendCustomMessage(type = 'rave_add_class',message = list(
        selector = '#__rave__mask__',
        class = 'hidden'
      ))
      self$data_selector_opened <- FALSE
    },

    register_data_check = function(check_fun){
      stopifnot2(is.function(check_fun), msg = 'register_data_check requires a function')
      argnames = formals(check_fun)
      stopifnot2(length(argnames) >= 3 || '...' %in% argnames, msg = 'data checks must have at least three parameters')
      private$check_fun = check_fun
    },
    run_data_check = function(){
      private$error_list <- list()
      if(is.function(private$check_fun)){
        withCallingHandlers({
          private$check_fun(self$container_data, self$module$package_data, getDefaultDataRepository())
        }, rave_check_error = function(e){
          # only captures rave_check_error
          private$error_list[[e$message]] <- e
          rave_info('\r[Module Validation]: {e$message}')
        }, error = function(e){
          rave_error("Captured unknown error: {e$message} in call:")
          traceback(e)
          private$error_list[[e$message]] <- e
        })
      }
      if(length(private$error_list)){
        rave_debug('{self$module_label} needs to load more data...')
        self$has_data <- FALSE
        # self$`@set_data_status`(FALSE)
      } else {
        self$has_data <- TRUE
        # self$`@set_data_status`(TRUE)
      }

      list(check_list = self$container_data, error_list = private$error_list)
    },
    register_loader_interface = function(loader_interface){
      stopifnot2(is.function(loader_interface), msg = 'register_loader_interface requires a function')
      argnames = formals(loader_interface)
      stopifnot2(length(argnames) >= 3 || '...' %in% argnames, msg = 'onfailure must have at least three parameters')

      # Since R will copy this function, original function won't be affected
      f_env <- environment(loader_interface)
      sneaky_env <- new.env(parent = f_env)
      environment(loader_interface) <- sneaky_env
      sneaky_env$observe <- private$sneaky_observe
      sneaky_env$observeEvent <- private$sneaky_observeEvent

      private$loader_interface = loader_interface

    },
    run_loader_interface = function(session = shiny::getDefaultReactiveDomain()){
      if(!is.function(private$loader_interface)){
        return()
      }
      remove_observers(private$observer_list)

      tryCatch({
        private$loader_interface(self$container_data,
                                 self$module$package_data,
                                 getDefaultDataRepository())
      }, error = function(e){
      })
    },
    register_onload_action = function(onload, variables = NULL){
      stopifnot2(is.function(onload), msg = 'register_onloader_action requires a function')
      argnames = formals(onload)
      stopifnot2(length(argnames) >= 3 || '...' %in% argnames, msg = 'onload must have at least three parameters')
      private$onload_action = onload
      private$onload_variables = variables
    },
    `@shiny_run_loader` = function(input, session = shiny::getDefaultReactiveDomain()){
      # get all the inputs
      for(param in private$onload_variables){
        if(param != ''){
          self$module$package_data[[param]] = session$input[[param]]
        }
      }

      # Don't click the button twice
      dipsaus::updateActionButtonStyled(session, '..rave_import_data_btn..', disabled = TRUE)
      on.exit({
        dipsaus::updateActionButtonStyled(session, '..rave_import_data_btn..', disabled = FALSE)
      })
      rave_info("Loading start...")

      tryCatch({
        private$onload_action(self$container_data, self$module$package_data, getDefaultDataRepository())
        # REMOVE all observers!!!
        remove_observers(private$observer_list)

        self$has_data <- TRUE
        # self$`@set_data_status`(TRUE)
        self$last_loaded = Sys.time()
        # dipsaus::set_shiny_input(session = session, inputId = '..rave_data_loaded..', value = Sys.time())
        self$container_reactives$..rave_data_loaded.. <- Sys.time()

        self$close_data_selector()
      }, error = function(e){
        rave_error(e$message)
        rave_debug('[Module ERROR] Failures while loading data')
        base::print(private$onload_action)
      })
    },

    initialize = function(module){
      stopifnot2(inherits(module, 'RAVEModule'), msg = 'RAVEContainer must be initialized with a module instance')
      self$module = module

      # Wrapper pointing to package namespace
      self$wrapper_env = new.env(parent = module$package_env)
      self$wrapper_env$ns = shiny::NS(self$module_id)

      # stores all module functions
      self$static_env = new.env(parent = self$wrapper_env)

      # stores masked variables such as input or local_data
      self$mask_env = new.env(parent = self$static_env)

      # where modules get parsed and function environment
      self$runtime_env = new.env(parent = self$mask_env)

      self$container_data = dipsaus::fastmap2()
      self$input_update_levels = dipsaus::fastmap2()
      private$observer_list = dipsaus::fastmap2()
      private$sneaky_observe = make_observe(private$observer_list, on_invalidate = self$`@invalidate`)
      private$sneaky_observeEvent = make_observeEvent(private$observer_list, on_invalidate = self$`@invalidate`)

      # to control the custom observers
      self$user_observers = dipsaus::fastmap2()
      self$wrapper_env$observe = make_observe(self$user_observers, on_invalidate = self$`@invalidate`)
      self$wrapper_env$observeEvent = make_observeEvent(self$user_observers, on_invalidate = self$`@invalidate`)

    },

    with_context = function(context, expr, envir = parent.frame(), quoted = FALSE, ...){
      if(!quoted){
        expr = substitute(expr)
      }
      with_rave_context(
        context, expr, package = self$package, module_id = self$module_id,
        instance = self, quoted = TRUE, env = envir)
      # ctx <- rave_context()
      # on.exit({
      #   rave_context(ctx$context, ctx$package, ctx$module_id, ctx$instance)
      # }, add = TRUE, after = TRUE)
      # rave_context(context, self$package, self$module_id, self)
    },

    register_context = function(context){
      if(missing(context)){
        context = from_rave_context('context')
      }
      invisible(rave_context(context, self$package, self$module_id, self))
    },

    import_widgets = function(context = 'rave_compile'){
      self$with_context(context, {
        widgets <- list.files(self$module$get_path(file.path('tools')), full.names = TRUE, pattern = "\\.[rR]$")
        # load widgets to runtime_env and move to wrapper_env
        for(wf in widgets){
          tryCatch({
            source(file = wf, local = self$runtime_env)
          }, error = function(e){
            cat(wf, '\n')
            traceback(e)
            rave_fatal(e$message)
          })
        }
        list2env(as.list(self$runtime_env), envir = self$wrapper_env)
        clear_env(self$runtime_env)
      })
    },

    parse_module = function(context = 'rave_compile'){
      self$with_context(context, {

        # initialize
        self$dynamic_script = list()
        self$init_script = list()
        self$input_components = list()
        self$input_layout = list()
        self$output_components = list()
        self$output_layout = list()
        self$main_functions = list()


        comp_script = self$module$get_path(file.path('modules', self$module_id, 'comp.R'))

        # read in comp.R
        script = readLines(comp_script) # 11, 674

        # Locate start and stop lines
        start_line = stringr::str_detect(script, '^#\\ [>]{6,}\\ Start\\ [\\-]+')
        end_line = stringr::str_detect(script, '^#\\ [<]{6,}\\ End\\ [\\-]+')
        stopifnot2(sum(start_line) == sum(end_line) && sum(start_line) == 1,
                   msg = sprintf('[Module ID: %s] comp.R contains multiple or no starting/ending markers.', self$module_id))
        script = script[seq.int(which(start_line), which(end_line))]
        expr <- parse(text = script)
        eval(expr, envir = self$runtime_env)

        # TODO: transfer component flags
        self$input_layout = self$runtime_env$input_layout
        self$output_layout = self$runtime_env$output_layout

        # To be compatible with old design
        render_inputs = unlist(self$runtime_env$render_inputs)
        manual_inputs = unlist(self$runtime_env$render_inputs)

        # Register monitoring inputs
        lapply(self$input_components, function(comp){
          # list(
          #   generator = dipsaus::new_function2(body = { !!call }, env = ctx$instance$wrapper_env),
          #   hook = update_hook,
          #   update_level = update_level,
          #   input_id = input_id
          # )
          if(comp$input_id %in% manual_inputs){
            comp$update_level = UPDATE_LEVEL$manual
            self$input_components[[comp$input_id]] <- comp
          } else if(comp$input_id %in% render_inputs){
            comp$update_level = UPDATE_LEVEL$render_only
            self$input_components[[comp$input_id]] <- comp
          }
          self$input_update_levels[[comp$input_id]] = comp$update_level
        })

        # main.R
        main_script = self$module$get_path(file.path('modules', self$module_id, 'main.R'))

        # read in main.R
        script = readLines(main_script) # 11, 674
        start_line = stringr::str_detect(script, '^#\\ [>]{6,}\\ Start\\ [\\-]+')
        end_line = stringr::str_detect(script, '^#\\ [<]{6,}\\ End\\ [\\-]+')
        stopifnot2(sum(start_line) == sum(end_line) && sum(start_line) == 1,
                   msg = sprintf('[Module ID: %s] main.R contains multiple or no starting/ending markers.', self$module_id))
        script = script[seq.int(which(start_line), which(end_line))]
        # Find lines with #' @section
        section_line = stringr::str_detect(script, "#'\\ @section(.*)$")
        if(!any(section_line)){
          # There is no section, just one
          script = c('{', script, '}')
          expr <- parse(text = script)
          self$main_functions[[1]] = list(
            section = 'Default',
            include = NULL, # Any input change will result in recalculation
            expr = expr[[1]]
          )
        }else{
          section_header = which(section_line)
          overhead = stringr::str_trim(script[seq_len(section_header[[1]] - 1)])
          if(!all(stringr::str_detect(overhead, '^#') || overhead == '')){
            rave_warn(
              "There is overhead in main.R which will be ignored. Reasons:\n",
              "  You want to partition main.R using #' @section flags. ",
              "But you must comment any code before the first flag and starting anchor. \n",
              "(One valid example)\n\n",
              "# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------\n",
              "#' @section This is section 1\n\n",
              "(One invalid example)\n\n",
              "# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------\n",
              "...Some R script not commented...\n",
              "#' @section This is section 1\n")
          }
          for(ii in seq_along(section_header)){
            start = section_header[[ii]]
            end = if ( ii == length(section_header)) length(script) else (section_header[[ii + 1]] - 1)
            section_code = script[seq.int(start, end)]
            section_name = stringr::str_match(section_code[[1]], "#'\\ @section(.*)$")[[2]]
            section_name = stringr::str_trim(section_name)
            if( section_name == '' ){ section_name = 'No name' }
            # look for include lines
            includes = stringr::str_match(section_code, "#'\\ @include(.*)$")[,2]
            includes = includes[!is.na(includes)]
            if(!length(includes)){
              includes = NULL
            } else {
              includes = unlist(stringr::str_split(includes, "[\\ ,]+"))
              includes = stringr::str_trim(includes)
              includes = includes[includes != '']
              includes = lapply(includes, str2lang)
            }
            section_code = c('{', section_code, '}')
            expr <- parse(text = section_code)
            self$main_functions[[ length(self$main_functions) + 1 ]] = list(
              section = section_name,
              include = includes, # Any input change will result in recalculation
              expr = expr[[1]],
              order = ii
            )
          }

        }

        self$main_digests = rep('', length(self$main_functions))

      })
    },

    `@register_shinysession` = function(session){
      # ctx = rave_context()
      # if(ctx$context == 'rave_running' ){
      if(!is.null(session)){
        # Must be in correct context
        # stopifnot(identical(ctx$instance, self))
        module_id <- self$module_id
        if(module_id != session$ns(NULL)){
          session = session$rootScope()$makeScope(module_id)
        }

        self$container_reactives = shiny::reactiveValues()

        self$wrapper_env$session = session
        self$wrapper_env$input = session$input
        self$wrapper_env$output = session$output
        self$wrapper_env$reactive_data = shiny::reactiveValues()
        self$wrapper_env$ns = shiny::NS(self$module_id)

        self$wrapper_env$package_data = self$module$package_data
        self$wrapper_env$session_data = self$container_data
        makeActiveBinding('global_data', function(){
          getDefaultDataRepository()
        }, self$wrapper_env)
      }


    },

    # Digest input change and run
    # if all, suppress input digest
    # Need isolate
    `@run` = function(all = FALSE){
      module_label = self$module$module_label
      tmp_env <- new.env(parent = self$runtime_env)
      ran = FALSE
      init_time <- Sys.time()

      n_funcs <- length(self$main_functions)

      progress <- dipsaus::progress2(self$module_label,
                                     max = n_funcs,
                                     shiny_auto_close = TRUE)
      for(ii in seq_len(n_funcs)){
        start_time <- Sys.time()
        main_f = self$main_functions[[ii]]

        progress$inc(sprintf('%s (%d of %d)', main_f$section, ii, n_funcs))
        if(length(main_f$include)){
          # digest and monitored changes
          # evaluate in param_env?

          digest = lapply(main_f$include, function(lang){
            digest::digest(tryCatch({
              re = eval(lang, envir = tmp_env)
              # base::print(re)
              re
            }, error = function(e){
              rave_debug('Error while digesting {lang}')
              cat(e$message, '\n')
            }))
          })
          digest = digest::digest(digest)


          if(!all && !ran && isTRUE(self$main_digests[[ii]] == digest)){
            # This part should be skipped
            timer = structure(dipsaus::time_delta(start_time, Sys.time()), unit = 's', class = 'rave-units')
            rave_debug('[{module_label}]: Part {ii}: {main_f$section} - no changes found, skipping... ({timer})')
            next()
          } else {
            self$main_digests[[ii]] = digest
          }
        }

        # Run the function
        # ran = TRUE
        tryCatch({
          dipsaus::eval_dirty(main_f$expr, env = self$runtime_env)
        }, error = function(e){
          rave_warn("Please check the following line:", style = 'fatal')
          base::print(main_f$expr)

          base::print(e$call)
          rave_fatal('[{module_label}]: Part {ii}: Error found in section {sQuote(main_f$section)}:\n',
                                '  {e$message}\n', 'Please check the messages above for bug information.',
                                call = 'See above messages')
        })
        timer = structure(dipsaus::time_delta(start_time, Sys.time()), unit = 's', class = 'rave-units')
        rave_debug('[{module_label}]: Part {ii}: {main_f$section} - finished ({timer})')
      }
      timer = structure(dipsaus::time_delta(init_time, Sys.time()), unit = 's', class = 'rave-units')
      rave_debug('[{module_label}]: finished main.R in {timer}, start rendering')
    },

    `@invalidate` = function(...){
      # if(is.function(self$`@_invalidate`)){
      #   self$`@_invalidate`(...)
      # }

      # check context
      # ctx <- rave_context()
      # stopifnot2(identical(ctx$instance, self), msg = 'Context not set right')

    },

    `@_invalidate` = NULL,

    # UI
    `@sidebar_menu` = function(standalone = TRUE){
      stopifnot2(standalone, msg = 'standalone = FALSE not implemented yet')
      shinydashboard::menuItem(self$module$module_label, tabName = toupper(self$module_id))
    },

    `@input_panel` = function(){
      unlisted = self$input_ids[!self$input_ids %in% unlist(self$input_layout)]
      if(length(unlisted)){
        self$input_layout[['Misc Inputs']] = as.list(unlisted)
      }
      layout_panel_names = names(self$input_layout)
      dipsaus::iapply(self$input_layout, function(el, ii){
        panel_name = layout_panel_names[[ii]]
        collapsed = FALSE
        if(stringr::str_detect(panel_name, '^(\\[-\\])')){
          panel_name = stringr::str_replace(panel_name, '^(\\[-\\])', '')
          collapsed = TRUE
        }
        headerColor = '#f4f4f4'
        if(stringr::str_detect(panel_name, '^(\\[#[0-9a-f]{6}\\])(.*)$')){
          m = stringr::str_match(panel_name, '^\\[(#[0-9a-f]{6})\\](.*)$')
          panel_name = m[[3]]
          headerColor = m[[2]]
        }

        box(title = panel_name, collapsible = TRUE, collapsed = collapsed,
            headerColor = headerColor, lapply(el, function(inputids){
          inputids = inputids[inputids %in% self$input_ids]
          n_inputs = length(inputids)
          if(n_inputs == 0){ return() }
          if(n_inputs > 1){
            if(n_inputs %% 3 == 0){
              flex_basis = rep('flex-basis: 33%;', n_inputs)
            } else if (n_inputs %% 2 == 0) {
              flex_basis = rep('flex-basis: 50%;', n_inputs)
            } else if (n_inputs %% 3 == 1) {
              flex_basis = c(rep('flex-basis: 33%;', n_inputs - 1), 'flex-basis: 100%;')
            }
            # get ids
            shiny::div(
              class = 'rave-grid-inputs',
              dipsaus::iapply(inputids, function(inputid, ii){
                shiny::div(
                  style = flex_basis[[ii]],
                  self$input_components[[inputid]]$generator()
                )
              }, .method = 'lapply')
            )
          } else {
            self$input_components[[inputids]]$generator()
          }
        }))

      }, .method = 'lapply')

    },


    `@output_panel` = function(){
      output_layout <- self$output_layout
      width <- output_layout$width
      width %?<-% 12L
      output_layout$width <- NULL
      tagsets = names(output_layout)
      unlisted = self$output_ids[!self$output_ids %in% unlist(output_layout)]

      if(!length(self$output_ids)){
        return()
      }

      get_ = function(p, default_tabset, default_unlisted){
        tagset_re = sapply(output_layout, function(comp){
          re <- comp[[p]]
          re %?<-% default_tabset
          re
        })
        unlisted_re = sapply(unlisted, function(id){
          re = self$output_components[[id]][[p]]
          re %?<-% default_unlisted; re
        })
        unlist(c(tagset_re, unlisted_re))
      }

      tab_or_id = c(tagsets, unlisted)
      is_tabset = get_('is_tabset', TRUE, FALSE)
      layout_order = get_('order', -Inf, Inf)
      widths = get_('width', 12L, 12L)

      render_order = order(layout_order)

      exprs = lapply(render_order, function(ii){
        if(is_tabset[[ii]]){
          # tabset
          tabname = tab_or_id[[ii]]
          tab_content = output_layout[[tabname]]
          panel_names = names(tab_content)
          inner_idx = unlist(tab_content)
          inner_idx = inner_idx[inner_idx %in% self$output_ids]
          if(!length(inner_idx)){ return() }
          # generate calls
          quo_panels = dipsaus::iapply(panel_names, function(pname, jj){
            sub_boxes = tab_content[[pname]]

            rlang::quo(shiny::tabPanel(
              title = !!pname,
              shiny::fluidRow(
                shiny::tagList(
                  lapply(sub_boxes, function(ids){
                    ids = ids[ids %in% self$output_ids]
                    nids = length(ids)
                    if(!nids){ return() }
                    if(nids %% 3 == 0){
                      widths = rep(4L, nids)
                    } else if (nids %% 2 == 0){
                      widths = rep(6L, nids)
                    } else {
                      widths = c(12L, rep(4L, nids-1))
                    }
                    dipsaus::iapply(ids, function(id, kk){
                      shiny::column(
                        width = widths[[kk]],
                        self$output_components[[id]]$generator()
                      )
                    }, .method = 'lapply')
                  })
                )
              )
            ))
          }, .method = 'lapply')
          rlang::quo(shinydashboard::tabBox(
            title = !!tabname,
            width = !!widths[[ii]],
            !!!quo_panels))
        } else{
          # this is a output ID
          id = tab_or_id[[ii]]
          comp = self$output_components[[id]]
          rlang::quo(box(
            title = !!comp$title,
            width = !!widths[[ii]],
            collapsible = TRUE,
            !!comp$generator()
          ))
        }
      })

      rlang::eval_tidy(rlang::quo(shiny::tagList(!!!exprs)))

    },


    `@display_loader` = function(){
      # assign('self', self, envir = globalenv())
      remove_observers(private$observer_list)
      shiny::isolate({
        modal_info <- tryCatch({
          private$loader_interface(self$container_data,
                                   self$module$package_data,
                                   getDefaultDataRepository())
        }, error = function(e){
          rave_error("Captured error: {e$message} in call:")
          traceback(e)
        })
      })

      modal_info
    },

    `@safe_close_selector` = function(){
      if(self$has_data){
        module_remove_notification()
        self$close_data_selector()
      } else {
        module_notification('This module needs to load more data to proceed. If you want to switch modules, browse other options by clicking on sidebar')
        # go to previous module
        # adapter$switch_container(app_data$last_module_id)
      }
    },

    `@set_data_status` = function(has_data){
      if(has_data){
        lapply(names(self$user_observers), function(nm){
          self$user_observers[[nm]]$resume()
        })
      } else {
        lapply(names(self$user_observers), function(nm){
          self$user_observers[[nm]]$suspend()
        })
      }
      self$has_data = has_data
    },

    ns = function(id = NULL){
      shiny::NS(self$module_id, id)
    }

  ),
  active = list(
    package = function(){
      self$module$package
    },
    module_id = function(){
      self$module$module_id
    },
    module_label = function(){
      self$module$module_label
    },
    package_root = function(){
      self$module$package_root
    },
    input_ids = function(){
      names(self$input_components)
    },
    output_ids = function(){
      names(self$output_components)
    },

    has_data = function(v){
      if(!missing(v)){
        self$.has_data = !isFALSE(v)
        if(self$.has_data){
          rave_debug('Resuming module observers')
          lapply(names(self$user_observers), function(nm){
            self$user_observers[[nm]]$resume()
          })
        } else {
          rave_debug('Stopping module observers')
          lapply(names(self$user_observers), function(nm){
            self$user_observers[[nm]]$suspend()
          })
        }
      }
      self$.has_data
    },

    data_selector_opened = function(v){
      if(!missing(v)){
        self$.data_selector_opened = isTRUE(v)
        if(self$.data_selector_opened){
          lapply(names(private$observer_list), function(nm){
            private$observer_list[[nm]]$resume()
          })
        } else {
          lapply(names(private$observer_list), function(nm){
            private$observer_list[[nm]]$suspend()
          })
        }
      }
      self$.data_selector_opened
    }

  )
)









