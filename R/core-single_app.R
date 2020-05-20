#' @export
instantiate_module <- function(package, module_id, debug = FALSE){

  raveutils::rave_condition('default')
  self = parse_module(package, module_id, debug = debug)
  module_label = self$module$module_label

  module_ui <- {
    rave_dash_page(
      header = rave_dash_header(title = 'RAVE Dashboard'),
      control = rave_dash_control(disable = TRUE),
      sidebar = rave_dash_sidebar(
        width = '280px',
        shinydashboard::sidebarMenu(
          id = 'sidebar',
          self$`@sidebar_menu`(standalone = TRUE)
          # shinydashboard::menuItemOutput(self$ns("..rave_data_loader.."))
        )
      ),
      body = rave_dash_body(
        shiny::div(
          class = 'rave-module-container open',
          `rave-module` = self$module_id,
          shiny::div(
            class = 'rave-dashboard-loader-wrapper',
            shiny::div(
              class = 'rave-dashboard-loader',
              shiny::uiOutput(self$ns("..rave_data_loader..")),
              shiny::div(
                class = 'hidden',
                shiny::textInput(self$ns('..rave_import_data_ui_show..'), '')
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
                    self$`@input_panel`()
                  )
                ),
                shiny::column(
                  width = 9L,
                  shiny::fluidRow(
                    self$`@output_panel`()
                  )
                )
              )
            )

          )

        )
      )
    )
  }

  module_server <- function(input, output, session){
    root_session <- session$rootScope()
    internal_observers = dipsaus::fastmap2()
    observe = make_observe(internal_observers, error_handler = function(e){
      raveutils::rave_error("[Module ERROR] {e$message}")
    })
    observeEvent = make_observeEvent(internal_observers, error_handler = function(e){
      raveutils::rave_error("[Module ERROR] {e$message}")
    })

    # Need to create module with session present
    self = parse_module(package, module_id)
    raveutils::add_to_session(session, key = 'rave_instance', val = self, override = TRUE)
    self$register_context('rave_running')
    assign('self', self, envir = globalenv())


    self$`@register_shinysession`(session)

    local_reactives = shiny::reactiveValues()
    local_map = dipsaus::fastmap2()

    local_map$delay_input = ravecore::rave_options('delay_input')

    # one time proc
    raveutils::rave_info('[{self$module_label}] Loading dynamic scripts...')
    lapply(self$dynamic_script, function(s){
      tryCatch({
        if(rlang::is_quosure(s)){
          dipsaus::eval_dirty(s, env = self$runtime_env)
        } else {
          source(s, local = self$runtime_env)
        }
      }, error = function(e){
        raveutils::rave_error('Error found in script: \n {s} \nReason: {e$message}')
        traceback(e)
      })
      invisible()
    })
    list2env(as.list(self$runtime_env), envir = self$static_env)

    # ------------ Register RAVE module output renderers ---------------
    lapply(names(self$output_components), function(output_id){
      output[[output_id]] <- self$output_components[[output_id]]$renderer()
      invisible()
    })
    # local({
    #   output = getDefaultReactiveOutput()
    #   for(output_id in names(self$output_components)){
    #     output[[output_id]] <- self$output_components[[output_id]]$renderer()
    #   }
    # })

    # ------------ Data loader UI components ---------------

    # Whenever data is missing or use wants to change data
    output$..rave_data_loader.. <- shiny::renderUI({
      input$..rave_import_data_ui_show..
      raveutils::rave_debug('Showing data panel')
      self$`@display_loader`()
    })

    # Cancel loading data
    observeEvent(input$..rave_import_data_btn_cancel.., {
      if(self$has_data){
        module_remove_notification()
        self$close_data_selector()
      } else {
        module_notification('This module needs to load data to proceed...')
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # When data load succeeds
    observeEvent(input$..rave_data_loaded.., {
      if(!isFALSE(self$has_data)){
        # Check and update inputs
        for(expr in self$init_script){
          # initialize inputs
          dipsaus::eval_dirty(expr, env = self$mask_env)
        }

        list2env(as.list(self$mask_env), self$runtime_env)

        # update inputs
        for(comp in self$input_components){
          raveutils::rave_debug('[self$module_label] Registering UI component - {comp$input_id}')
          comp$hook()
        }
        local_map$input_flag = shiny::isolate(local_reactives$input_changed)
        later::later(function(){
          if(isTRUE(local_map$input_flag == shiny::isolate(local_reactives$input_changed))){
            local_reactives$input_changed = Sys.time()
          }
        }, delay = 0.5)


      }
    }, ignoreNULL = TRUE)

    observeEvent(local_reactives$data_changed, {

    })


    # ------------ Monitors Input changes ---------------

    # make expressions to register for input monitors
    lapply(names(self$input_update_levels), function(input_id){
      observeEvent(input[[input_id]], {
        # rave_debug('{input_id} (changed)')
        now = Sys.time()

        # Put input to container_data (session_data), and mask_env
        val <- input[[input_id]]
        self$container_data[[input_id]] <- val
        self$mask_env[[input_id]] <- val
        self$runtime_env[[input_id]] <- val

        input_update_level = max(local_map$input_update_level, self$input_update_levels[[input_id]])

        local_map$input_changed = now
        local_map$input_update_level = input_update_level

        local_reactives$input_changed = now
      }, ignoreNULL = FALSE, ignoreInit = FALSE)
    })

    # listen to user-defined observers
    self$`@_invalidate` <- function(...){
      local_reactives$observer_triggered = Sys.time()
    }

    input_changed <- shiny::debounce(shiny::reactive({
      local_reactives$input_changed
    }), local_map$delay_input)


    observeEvent(input_changed(), {
      if(!self$has_data){
        raveutils::rave_debug('[{self$module_label}] No data')
        return()
      }
      # Run main!!! finally
      if(!isTRUE(local_map$last_loaded == self$last_loaded)){
        raveutils::rave_info('Detect data change. Scheduled to re-run all blocks')
        print(local_map$last_loaded)
        local_map$last_loaded = self$last_loaded
        local_map$data_changed = TRUE
        all = TRUE
      } else {
        # raveutils::rave_info('No data change. Run smartly')
        if(isTRUE(local_map$data_changed)){
          all = TRUE
        } else {
          all = FALSE
        }
      }

      # If auto-run is enabled
      # and input_update_level is at least 2
      update_levels = local_map$input_update_level
      if(update_levels >= 2){
        # will require run main.R
        if(self$auto_run <= 0){
          raveutils::rave_debug("[{self$module_label}] auto-run is off, only render outputs")
        } else {
          self$`@run`(all = all)
          self$auto_run = self$auto_run - 1
          local_map$data_changed = FALSE
        }
        dipsaus::set_shiny_input(session, inputId = '..rave_output_update_all..',
                                 value = Sys.time(), priority = 'event')
      } else if (update_levels == 1){

        if(isTRUE(local_map$data_changed)){
          # TODO: detect data change for multiple tabs/ switch modules
          raveutils::rave_debug("[{self$module_label}] detects data change. ")
        } else {
          raveutils::rave_debug("[{self$module_label}] No input that affects main.R detected, re-render outputs")
        }

        # only render
        dipsaus::set_shiny_input(session, inputId = '..rave_output_update_all..',
                                 value = Sys.time(), priority = 'event')

      }


    })



    # observe focus. if focused, then set context
    {
      # check data
      self$dynamic_script

    }






    # -------------- document ready ------------------
    # everytime process
    # data checks
    check_data <- function(close_if_pass = FALSE){
      rave_debug("{module_label} is checking data...")
      self$run_data_check()
      if(isFALSE(self$has_data)){
        self$open_data_selector()
        self$run_loader_interface()
        dipsaus::set_shiny_input(session = session, inputId = '..rave_import_data_ui_show..',
                                 value = Sys.time(), priority = 'event')
      } else if(close_if_pass){
        raveutils::rave_debug('Automatically hide selector')
        self$close_data_selector()
        dipsaus::set_shiny_input(session = session, inputId = '..rave_data_loaded..', value = Sys.time())
      } else {
        self$open_data_selector()
        self$run_loader_interface()
        dipsaus::set_shiny_input(session = session, inputId = '..rave_import_data_ui_show..',
                                 value = Sys.time(), priority = 'event')
      }

    }

    observeEvent(root_session$input$data_select, {
      check_data(close_if_pass = FALSE)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    check_data(TRUE)



  }

  return(list(
    module_ui = module_ui,
    module_server = module_server
  ))
}

#' @export
launch_single_module <- function(package, module_id, debug = FALSE){
  module_comp <- instantiate_module(package, module_id, debug)
  app <- shiny::shinyApp(
    ui = module_comp$module_ui, server = function(input, output, session){

      shiny::callModule(module_comp$module_server, module_id)
    },
    options = list(
      launch.browser=TRUE
    )
  )
  print(app)
}

