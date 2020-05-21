shinirize <- function(input, output, session, container, app_data){
  if(!inherits(container, 'RAVEContainer')){ return() }

  # Local data, override observers
  local_reactives = shiny::reactiveValues()
  local_map <- dipsaus::fastmap2()
  local_map$delay_input <- app_data$delay_input
  local_reactives$initialized <- FALSE

  # Register shiny session
  container$`@register_shinysession`(session)

  # Load dynamic scripts
  raveutils::rave_info('[{container$module_label}] Loading dynamic scripts...')

  lapply(container$dynamic_script, function(s){
    tryCatch({
      if(rlang::is_quosure(s)){
        dipsaus::eval_dirty(s, env = container$runtime_env)
      } else {
        source(s, local = container$runtime_env)
      }
    }, error = function(e){
      raveutils::rave_error('Error found in script: \n {s} \nReason: {e$message}')
      traceback(e)
    })
    invisible()
  })
  list2env(as.list(container$runtime_env), envir = container$static_env)


  # ------------ Register RAVE module output renderers ---------------
  lapply(names(container$output_components), function(output_id){
    output[[output_id]] <- container$output_components[[output_id]]$renderer()
    invisible()
  })

  # ------------ Data loader UI components ---------------

  # Whenever data is missing or user wants to change data
  output$..rave_data_loader.. <- shiny::renderUI({
    input$..rave_import_data_ui_show..
    raveutils::rave_debug('Showing data panel')
    container$`@display_loader`()
  })

  # Cancel loading data
  container$wrapper_env$observeEvent(input$..rave_import_data_btn_cancel.., {
    if(container$has_data){
      module_remove_notification()
      container$close_data_selector()
    } else {
      module_notification('This module needs to load data to proceed...')
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # When data load succeeds
  container$wrapper_env$observeEvent(input$..rave_data_loaded.., {
    raveutils::rave_debug('Data loaded signal received')

    if(!isFALSE(container$has_data)){


      container$close_data_selector(session = session)

      if(!local_reactives$initialized){
        raveutils::rave_debug('Initialize inputs')
        # Check and update inputs
        for(expr in container$init_script){
          # initialize inputs
          dipsaus::eval_dirty(expr, env = container$mask_env)
        }

        list2env(as.list(container$mask_env), container$runtime_env)

        # update inputs
        for(comp in container$input_components){
          # raveutils::rave_debug('[{container$module_label}] Registering UI component - {comp$input_id}')
          comp$hook()
        }
        # local_map$input_flag = shiny::isolate(local_reactives$input_changed)
        # later::later(function(){
        #   if(isTRUE(local_map$input_flag == shiny::isolate(local_reactives$input_changed))){
        #     local_reactives$input_changed = Sys.time()
        #   }
        # }, delay = 0.5)


        local_reactives$initialized = TRUE
      }

    }
  }, ignoreNULL = TRUE, priority = 999)



  # ------------ Monitors Input changes ---------------

  # make expressions to register for input monitors
  lapply(names(container$input_update_levels), function(input_id){
    container$wrapper_env$observeEvent(input[[input_id]], {

      if(local_reactives$initialized && container$has_data){
        # rave_debug('{input_id} (changed)')
        now = Sys.time()
        input_update_level = max(local_map$input_update_level, container$input_update_levels[[input_id]])
        local_map$input_changed = now
        local_map$input_update_level = input_update_level
        local_reactives$input_changed = now
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE, priority = -990)
  })

  input_changed <- shiny::debounce(shiny::reactive({
    # raveutils::rave_debug('Detected input change')
    local_reactives$input_changed
  }), 300)

  render_outputs <- shiny::debounce(shiny::reactive({
    local_reactives$render_output
  }), local_map$delay_input)

  shiny::observeEvent(render_outputs(), {
    raveutils::rave_info('Sending signals to update outputs')
    dipsaus::set_shiny_input(session, inputId = '..rave_output_update_all..',
                             value = Sys.time(), priority = 'event')
  })

  calibrate_inputs <- function(){
    # Put input to container_data (session_data), and mask_env
    input <- shiny::isolate(shiny::reactiveValuesToList(input))
    dipsaus::list_to_fastmap2(input, container$container_data)
    list2env(input, envir = container$mask_env)
    list2env(input, envir = container$runtime_env)
  }

  execute_main <- function(){
    if(!isTRUE(local_map$last_loaded == container$last_loaded)){
      raveutils::rave_info('Detect data change. Scheduled to re-run all blocks')
      local_map$last_loaded = container$last_loaded
      local_map$data_changed = TRUE
      all = TRUE
    } else {
      raveutils::rave_info('No data change. Run smartly')
      if(isTRUE(local_map$data_changed)){
        all = TRUE
      } else {
        all = FALSE
      }
    }

    tryCatch({
      container$`@run`(all = all)
      container$auto_run = container$auto_run - 1
      local_map$data_changed = FALSE
    }, error = function(e){
      raveutils::rave_error(e$message)
    })

  }


  main_observer <- shiny::observeEvent(input_changed(), {
    if(!container$has_data){
      raveutils::rave_info('[{container$module_label}] Waiting to load data')
      return()
    }
    if(!local_reactives$initialized){
      raveutils::rave_info('[{container$module_label}] Waiting for instialization')
      return()
    }
    raveutils::rave_info('Module has data [{container$has_data}] and initialized [{local_reactives$initialized}]')
    # Run main!!! finally

    calibrate_inputs()

    # If auto-run is enabled
    # and input_update_level is at least 2
    update_levels = local_map$input_update_level
    if(update_levels >= 2){
      # will require run main.R
      if(container$auto_run <= 0){
        raveutils::rave_debug("auto-run is off, only render outputs")
      } else {
        shiny::isolate(execute_main())
      }
      local_reactives$render_output = Sys.time()
    } else if (update_levels == 1){
      raveutils::rave_debug("[{container$module_label}] No input that affects main.R detected, re-render outputs")
      # only render
      local_reactives$render_output = Sys.time()
    }
  }, priority = -999)


  container$wrapper_env$observe({
    input_changed()
    if(!isFALSE(local_map$main_suspended) && isTRUE(local_reactives$initialized) && container$has_data){
      local_map$main_suspended = FALSE
      later::later(function(){
        rave_debug("Main is resumed")
        main_observer$resume()
        local_map$main_suspended = FALSE
      }, delay = 1)
    }
  }, priority = -1000)

  # -------------- document ready ------------------
  check_data <- function(close_if_pass = FALSE){
    # main_observer$suspend()
    # local_map$main_suspended = TRUE
    rave_debug("Main is suspended")
    rave_debug("{container$module_label} is checking data...")
    # local_reactives$initialized = FALSE
    container$has_data = FALSE
    container$run_data_check()
    if(isFALSE(container$has_data)){
      container$open_data_selector(session = session)
      container$run_loader_interface(session = session)
      # shiny::updateTextInput(session, inputId = '..rave_import_data_ui_show..', value = Sys.time())
      dipsaus::set_shiny_input(session = session, inputId = '..rave_import_data_ui_show..',
                               value = Sys.time(), priority = 'event')
    } else if(close_if_pass){
      raveutils::rave_debug('Automatically hide selector')
      container$close_data_selector(session = session)
      # shiny::updateTextInput(session, inputId = '..rave_data_loaded..', value = Sys.time())
      dipsaus::set_shiny_input(session = session, inputId = '..rave_data_loaded..',
                               value = Sys.time(), priority = 'event')
    } else {
      container$open_data_selector(session = session)
      container$run_loader_interface(session = session)
      # shiny::updateTextInput(session, inputId = '..rave_import_data_ui_show..', value = Sys.time())
      dipsaus::set_shiny_input(session = session, inputId = '..rave_import_data_ui_show..',
                               value = Sys.time(), priority = 'event')
    }

  }


  container$`@shiny_resume` <- function(){
    check_data(close_if_pass = TRUE)
  }


  root_session <- session$rootScope()
  root_session$output[[sprintf('%s_UI', container$module_id)]] <- shiny::renderUI({
    # if(is.null(reactive_data[[module_id_uppercase]])){
    #   return('The module has not been initialized')
    # }
    shiny::div(
      class = 'rave-module-container open',
      `rave-module` = container$module_id,
      shiny::div(
        class = 'rave-dashboard-loader-wrapper',
        shiny::div(
          class = 'rave-dashboard-loader',
          shiny::uiOutput(container$ns("..rave_data_loader..")),
          shiny::div(
            class = 'hidden'
            # shiny::textInput(container$ns('..rave_import_data_ui_show..'), ''),
            # shiny::textInput(container$ns('..rave_data_loaded..'), '')
          )
        )
      ),
      shiny::div(
        class = 'rave-module-main-wrapper',
        shiny::div(
          class = 'rave-module-main',
          shiny::fluidRow(
            shiny::column(
              width = 3L,
              shiny::fluidRow(
                container$`@input_panel`()
              )
            ),
            shiny::column(
              width = 9L,
              shiny::fluidRow(
                container$`@output_panel`()
              )
            )
          )
        )

      )

    )
  })


}