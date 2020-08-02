shinirize <- function(input, output, session, container, app_data, adapter){
  if(!inherits(container, 'RAVEContainer')){ return() }

  # Local data, override observers
  local_reactives = shiny::reactiveValues()
  local_map <- dipsaus::fastmap2()
  local_map$delay_input <- app_data$delay_input
  local_reactives$initialized <- FALSE

  # Register shiny session
  container$`@register_shinysession`(session)


  # ------------ Load dynamic scripts ------------
  rave_info('[{container$module_label}] Loading dynamic scripts...')

  lapply(container$dynamic_script, function(s){
    tryCatch({
      if(rlang::is_quosure(s)){
        dipsaus::eval_dirty(s, env = container$runtime_env)
      } else {
        source(s, local = container$runtime_env)
      }
    }, error = function(e){
      rave_error('Error found in script: \n {s} \nReason: {e$message}')
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


  # Cancel loading data
  shiny::observeEvent(input$..rave_import_data_btn_cancel.., {
    container$`@safe_close_selector`()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)


  # Need to initialize under two conditions
  # 1. new module
  # 2. new data loaded
  initialize_module <- function(){
    rave_debug('Initialize inputs')

    local_map$initialize_error = FALSE

    # Check and update inputs
    for(expr in container$init_script){
      # initialize inputs
      tryCatch({
        dipsaus::eval_dirty(expr, env = container$mask_env)
      }, error = function(e){
        rave_error(e$message)
        print(expr)
        print(e$call)
        module_notification('Error while initializing module.')
        local_map$initialize_error = TRUE
      })

    }

    list2env(as.list(container$mask_env), container$runtime_env)

    # update inputs
    for(comp in container$input_components){
      tryCatch({
        comp$hook()
      }, error = function(e){
        rave_error(e$message)
        print(e$call)
        module_notification('Error while updating input: ', comp$input_id)
        local_map$initialize_error = TRUE
      })
    }

    if(!local_map$initialize_error){
      local_reactives$initialized = TRUE
      container$last_loaded = app_data$last_loaded
    } else {
      local_reactives$initialized = FALSE
    }


  }

  # When data load succeeds
  shiny::observeEvent(container$container_reactives$..rave_data_loaded.., {
    rave_debug('Data loaded signal received')
    if(!isFALSE(container$has_data)){
      container$close_data_selector(session = session)

      if(!isTRUE(container$last_loaded == app_data$last_loaded)){
        initialize_module()
        # If container auto recaluclate is false, force re-render outputs
        if(!(container$auto_run > 0)){
          local_reactives$render_output <- Sys.time()
        }
      }

    }
  }, ignoreNULL = TRUE, priority = -1000)



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
    }, ignoreNULL = FALSE, ignoreInit = TRUE, label = sprintf('rave-input-%s-%s', container$module_id, input_id))
  })

  input_changed <- shiny::debounce(shiny::reactive({
    rave_debug('Detected input change')
    local_reactives$input_changed
  }), 50)

  render_outputs <- shiny::debounce(shiny::reactive({
    local_reactives$render_output
  }), local_map$delay_input)

  container$wrapper_env$observeEvent(render_outputs(), {
    rave_info('Sending signals to update outputs')
    dipsaus::set_shiny_input(session, inputId = '..rave_output_update_all..',
                             value = Sys.time(), priority = 'event')
  }, label = sprintf('rave-observer-%s-update_outputs', container$module_id))

  calibrate_inputs <- function(){
    # Put input to container_data (session_data), and mask_env
    input <- shiny::isolate(shiny::reactiveValuesToList(input))
    dipsaus::list_to_fastmap2(input, container$container_data)
    list2env(input, envir = container$mask_env)
    list2env(input, envir = container$runtime_env)
  }

  # Returns true/false whether main.R is terminated (false) or not (true)
  execute_main <- function(){
    if(!isTRUE(local_map$last_loaded == app_data$last_loaded)){
      rave_info('Detect data change. Scheduled to re-run all blocks')
      local_map$last_loaded = app_data$last_loaded
      local_map$data_changed = TRUE
      all = TRUE
    } else {
      rave_info('No data change. Run smartly')
      if(isTRUE(local_map$data_changed)){
        all = TRUE
      } else {
        all = FALSE
      }
    }

    tryCatch({
      # tryCatch({
      #   stop(123)
      # }, error = function(e){
      #   rave_fatal('asdasd')
      # })
      container$`@run`(all = all)
      container$auto_run <- container$auto_run - 1
      local_map$data_changed = FALSE
      TRUE
    }, raveExitMain = function(e){
      rave_debug('Module early terminated main.R.')
      FALSE
    }, error = function(e){
      rave_debug('Module run into error.')
      rave_error(e$message)
      FALSE
    }, raveFatal = function(e){
      rave_debug('Module run into error. Debug information is highlighted above.')
      FALSE
    })
  }


  container$wrapper_env$observeEvent(input_changed(), {
    if(!container$has_data){
      rave_info('[{container$module_label}] Waiting to load data')
      return()
    }
    if(!local_reactives$initialized){
      rave_info('[{container$module_label}] Waiting for instialization')
      return()
    }
    rave_debug('Module has data [{container$has_data}] and initialized [{local_reactives$initialized}]')
    # Run main!!! finally

    calibrate_inputs()

    # If auto-run is enabled
    # and input_update_level is at least 2
    update_levels = local_map$input_update_level
    if(update_levels >= 2){
      # will require run main.R
      succeed <- TRUE
      if(container$auto_run <= 0){
        rave_debug("auto-run is off, only render outputs")
      } else {
        # shiny::isolate(execute_main())
        succeed <- execute_main()
      }
      # TODO: Re-think whether early terminate main.R should result in
      # output being re-rendered?
      if( succeed ){
        local_reactives$render_output = Sys.time()
      }
    } else if (update_levels == 1){
      rave_debug("[{container$module_label}] No input that affects main.R detected, re-render outputs")
      # only render
      local_reactives$render_output = Sys.time()
    }
    local_map$input_update_level <- 0
  }, priority = -999, label = sprintf('rave-main-%s', container$module_id))


  # container$wrapper_env$observe({
  #   input_changed()
  #   if(!isFALSE(local_map$main_suspended) && isTRUE(local_reactives$initialized) && container$has_data){
  #     local_map$main_suspended = FALSE
  #     later::later(function(){
  #       rave_debug("Main is resumed")
  #       main_observer$resume()
  #       local_map$main_suspended = FALSE
  #     }, delay = 1)
  #   }
  # }, priority = -1000)


  # -------------- document ready ------------------
  check_data <- function(close_if_pass = FALSE){
    # main_observer$suspend()
    # local_map$main_suspended = TRUE
    # rave_debug("Main is suspended")
    rave_debug("{container$module_label} is checking data...")
    # local_reactives$initialized = FALSE
    # container$has_data = FALSE

    container$run_data_check()
    if(isFALSE(container$has_data)){
      container$open_data_selector(session = session)
      container$run_loader_interface(session = session)
      # dipsaus::set_shiny_input(session = session, inputId = '..rave_import_data_ui_show..',
      #                          value = Sys.time(), priority = 'event')
      container$container_reactives$..rave_import_data_ui_show.. <- Sys.time()
    } else if(close_if_pass){
      rave_debug('Automatically hide selector')
      container$close_data_selector(session = session)
      # dipsaus::set_shiny_input(session = session, inputId = '..rave_data_loaded..',
      #                          value = Sys.time(), priority = 'event')
      container$container_reactives$..rave_data_loaded.. <- Sys.time()
    } else {
      container$open_data_selector(session = session)
      container$run_loader_interface(session = session)
      # dipsaus::set_shiny_input(session = session, inputId = '..rave_import_data_ui_show..',
      #                          value = Sys.time(), priority = 'event')
      container$container_reactives$..rave_import_data_ui_show.. <- Sys.time()
    }

  }


  container$`@shiny_resume` <- function(close_if_pass = TRUE){

    rave_info('Resuming module')

    check_data(close_if_pass = close_if_pass)

    # # In case other modules has loaded data
    # if(container$has_data){
    #   # initialize ?
    #   if(!isTRUE(app_data$last_loaded == container$last_loaded)){
    #     initialize_module()
    #
    #   }
    # }

  }

  # If rave_running
  shiny::observeEvent({
    input$..rave_import_data_btn..
  }, {
    container$`@shiny_run_loader`(shiny::reactiveValuesToList(input))
    app_data$last_loaded = Sys.time()
  }, ignoreInit = TRUE)


  # ------------ Data loader UI components ---------------

  # Whenever data is missing or user wants to change data
  output$..rave_data_loader.. <- shiny::renderUI({
    base::print(local_reactives$initialized)
    base::print(container$container_reactives$..rave_import_data_ui_show..)

    rave_info('Rendering data panel')
    modal_info <- container$`@display_loader`()
    est_loadtime <- modal_info$expectedloadingtime
    if(length(est_loadtime) == 1){
      est_loadtime <- shiny::tags$small(
        'Expected load: ', as.character(est_loadtime), ' seconds')
    } else {
      est_loadtime <- ''
    }
    local_reactives$rebind <- Sys.time()
    shiny::tagList(
      modal_info$ui,
      shiny::hr(),
      est_loadtime
    )
  })
  shiny::observeEvent(container$container_reactives$..rave_import_data_ui_show.., {
    session$sendCustomMessage(type = 'rave_rebindall', message = list())
  })





  # -------------- Register UI ------------------
  shiny::insertUI(
    selector = sprintf('#shiny-tab-%s', stringr::str_to_upper(container$module_id)),
    where = 'afterBegin', immediate = TRUE, multiple = FALSE,
    ui =
      shiny::div(
        class = 'rave-module-container open',
        `rave-module` = container$module_id,
        shiny::div(
          class = 'rave-dashboard-loader-wrapper',
          shiny::div(
            class = 'rave-dashboard-loader',
            shiny::column(
              class = 'rave-dashboard-loader-content',
              width = 12L,
              shiny::h4(sprintf('Select data to load for module - %s', container$module_label)),
              shiny::hr(),
              shiny::uiOutput(container$ns("..rave_data_loader..")),
              shiny::div(
                style = 'padding: 10px 0; float: right;',
                shiny::actionButton(
                  container$ns('..rave_import_data_btn_cancel..'), "Cancel"),
                shiny::span(dipsaus::html_asis(' ')),
                dipsaus::actionButtonStyled(
                  container$ns('..rave_import_data_btn..'), "Load Data")
              )
            ),
            shiny::div(
              class = 'hidden'
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
  )
}