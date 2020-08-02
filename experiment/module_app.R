devtools::load_all()

package = 'testproj'; module_id = 'module_id'
self = parse_module(package, module_id)

launch_single_module <- function(package, module_id){

  ravecore::rave_condition('default')
  self = parse_module(package, module_id)
  module_label = self$module$module_label

  module_ui <- {
    rave_dash_page(
      header = rave_dash_header(title = 'RAVE Dashboard'),
      control = rave_dash_control(disable = TRUE),
      sidebar = rave_dash_sidebar(
        width = '280px',
        shinydashboard::sidebarMenu(
          id = 'sidebar',
          # self$`@sidebar_menu`(standalone = TRUE)
          shinydashboard::menuItemOutput(self$ns("..rave_data_loader.."))
        )
      ),
      body = rave_dash_body(
        shiny::div(
          class = 'rave-module-container',
          `rave-module` = self$module_id,
          # shiny::div(
          #   class = 'rave-dashboard-loader-wrapper',
          #   shiny::div(
          #     class = 'rave-dashboard-loader',
          #     shiny::uiOutput(self$ns("..rave_data_loader.."))
          #   )
          # ),
          # shiny::div(
          #   class = 'hidden',
          #   shiny::textInput('..rave_import_data_ui_show..', ''),
          #   shiny::textInput('..rave_data_loaded..', '')
          # ),
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

    # Need to create module with session present
    # self = parse_module(package, module_id)
    dipsaus::add_to_session(session, key = 'rave_instance', val = self, override = TRUE)
    self$register_context('rave_running')

    self$`@register_shinysession`(session)

    local_reactives = shiny::reactiveValues()
    local_map = dipsaus::fastmap2()

    local_map$delay_input = ravecore::rave_options('delay_input')

    # one time proc
    lapply(self$dynamic_script, function(s){
      tryCatch({
        if(rlang::is_quosure(s)){
          dipsaus::eval_dirty(s, env = self$runtime_env)
        } else {
          source(s, local = self$runtime_env)
        }
      }, error = function(e){
        ravecore::rave_error('Error found in script: \n {s} \nReason: {e$message}')
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
    output$..rave_data_loader.. <- shinydashboard::renderMenu({
      input$..rave_import_data_ui_show..
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
        rave_debug('{input_id} (changed)')
        now = Sys.time()

        input_update_level = max(local_map$input_update_level, self$input_update_levels[[input_id]])

        local_map$input_changed = now
        local_map$input_update_level = input_update_level

        local_reactives$input_changed = now
      })
    })

    input_changed <- shiny::debounce(shiny::reactive({
      local_reactives$input_changed
    }), local_map$delay_input)


    observeEvent(input_changed(), {
      if(!self$has_data){
        return()
      }
      # Run main!!! finally
      if(!isTRUE(local_map$last_loaded == self$last_loaded)){
        local_map$last_loaded = self$last_loaded
        all = TRUE
      } else {
        all = FALSE
      }

      # If auto-run is enabled
      # and input_update_level is at least 2
      if(self$auto_run && local_map$input_update_level >= 2){
        self$`@run`(all = all)
      }

      dipsaus::set_shiny_input(session, inputId = '..rave_output_update_all..',
                               value = Sys.time(), priority = 'event')

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
      } else if(close_if_pass){
        self$close_data_selector()
      }
      self$run_loader_interface()
    }

    observeEvent(session$rootScope()$input$rave_sidebar_collapsed, {
      check_data()
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    check_data(TRUE)



  }

  return(list(
    module_ui = module_ui,
    module_server = module_server
  ))
}




module_comp <- launch_single_module(package, module_id)
shiny::shinyApp(
  ui = module_comp$module_ui, server = function(input, output, session){
    shiny::callModule(module_comp$module_server, module_id)
  },
  options = list(
    launch.browser=TRUE
  )
)
