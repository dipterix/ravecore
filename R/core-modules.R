# Function to load all modules


module_exists <- function(package, module_id){

  if(!requireNamespace(package, quietly = FALSE)){
    return(FALSE)
  }

  # get rave.yaml
  conf <- system.file('rave2.yaml', package = package, mustWork = FALSE)
  if(!file.exists(conf)){
    return(FALSE)
  }

  conf <- raveutils::load_yaml(conf)
  # package version is too low
  ver <- conf$rave_version
  ver %?<-% '0.0.0'
  if(compare_rave_version(ver, strict = TRUE)){
    # TODO: Add more detailed instructions
    raveutils::rave_warn('Package ', package, ' requires at least version', ver, '. Your rave core version might be too low. Please update RAVE.')
    return(FALSE)
  }

  # Whether comp and main.R exist
  module_path <- system.file('modules', package = package, mustWork = FALSE)

  if(missing(module_id)){
    return(TRUE)
  }

  comp_path <- file.path(module_path, module_id, 'comp.R')

  if(!file.exists(comp_path)){
    raveutils::rave_warn('Cannot find comp.R in module path - ', module_id)
    return(FALSE)
  }

  main_path <- file.path(module_path, module_id, 'main.R')
  if(!file.exists(main_path)){
    raveutils::rave_warn('Cannot find main.R in module path - ', module_id)
    return(FALSE)
  }

  return(TRUE)


}

get_module <- function(package, module_id, debug = FALSE, session = shiny::getDefaultReactiveDomain()){

  # make sure module exist
  if(!module_exists(package = package, module_id = module_id)){
    return(NULL)
  }

  # Create module
  shiny::withReactiveDomain(session, {
    container <- parse_module(package = package, module_id = module_id, debug = debug)
  })

  container
}

app_ui_env <- new.env(parent = emptyenv())

find_modules <- function(packages){
  if(missing(packages)){
    lib_paths <- .libPaths()
    packages <- unlist(lapply(lib_paths, function(path){
      pkg <- list.dirs(path, full.names = FALSE, recursive = FALSE)
      pkg[file.exists(file.path(path, pkg, 'rave2.yaml'))]
    }))
  }
  packages <- unique(packages)
  modules <- dipsaus::fastmap2()
  lapply(packages, function(pkg){
    conf <- raveutils::load_yaml(system.file('rave2.yaml', package = pkg))
    for(minfo in conf$modules){
      minfo$package = pkg
      minfo$notes = ''
      minfo$active %?<-% FALSE
      modules[[minfo$module_id]] <- minfo
    }
  })
  colsettings <- list(
    'module_id' = list(name = 'ID', default = NA),
    'module_label' = list(name = 'Name', default = 'Untitled'),
    'group_name' = list(name = 'Group', default = '______'),
    'order' = list(name = 'Order', default = 99999999),
    'package' = list(name = 'Package', default = NA),
    'active' = list(name = 'Active', default = TRUE),
    'notes' = list(name = 'Notes', default = ''),
    'icon' = list(name = 'Icon', default = '')
  )
  col_names <- vapply(colsettings, '[[', '', 'name')
  # override using rave_options
  module_csv <- rave_options("module_lookup_file")
  if(file.exists(module_csv)){
    tryCatch({
      mtbl <- utils::read.csv(module_csv, colClasses = 'character')
      exist_names <- col_names %in% colnames(mtbl)
      tmp_names <- col_names[exist_names]
      mtbl <- mtbl[, tmp_names]
      names(mtbl) <- names(colsettings)[exist_names]

      if('active' %in% names(mtbl)){
        mtbl$active = !stringr::str_detect(mtbl$active, '^[fF]')
      }
      if('order' %in% names(mtbl)){
        mtbl$order = as.numeric(mtbl$order)
      }

      for(ii in seq_len(nrow(mtbl))){
        row <- as.list(mtbl[ii, ])
        old <- as.list(modules[[row$module_id]])
        for(nm in names(row)){
          old[[nm]] <- row[[nm]]
          modules[[old$module_id]] <- old
        }
      }
    }, error = function(e){
      raveutils::rave_warn('An error found while reading file {module_csv}. ',
                           'The customized settings are ignored.')
    })

  }

  modules <- as.list(modules)
  minfos <- lapply(modules, function(minfo){
    for(nm in names(colsettings)){
      if(length(minfo[[nm]]) != 1){
        minfo[[nm]] <- colsettings[[nm]][['default']]
      }
    }
    as.data.frame(minfo)
  })
  minfos <- do.call('rbind', unname(minfos))
  minfos <- minfos[stats::complete.cases(minfos),]
  minfos <- minfos[order(minfos$order),]
  return(minfos)

}



app_ui <- function(adapter, theme = 'purple', token = NULL){
  req <- list()
  dipsaus::new_function2(alist(req=), {
    qstr = req$QUERY_STRING
    url_info = shiny::parseQueryString(qstr)

    get_ui <- function(str, ifnfound = NULL, ...){
      f <- get0(str, inherits = FALSE, mode = 'function', ifnotfound = NULL, envir = app_ui_env)
      if(is.function(f)){
        return(f(...))
      } else {
        ifnfound
      }
    }

    if(!!length(token)){
      if (!length(url_info$token) || !any(url_info$token %in% !!token)) {
        # 404 Page
        return(get_ui('rave-404-page', NULL))
      }
    }

    if (isTRUE(url_info$type == "3dviewer")) {
      return(get_ui('rave-3d-viewer', '3D viewer not ready', url_info$globalId, url_info$sessionId))
    }
    nomodal <- raveutils::get_val2(url_info, 'nomodal', default = FALSE, min_len = 1, max_len = 1)

    if(!isTRUE(url_info$theme %in% c("purple", "red", "green", "blue", "white"))){
      url_info$theme = !!theme
    }

    return(get_ui('rave-main-app', 'Application is not ready',
                  adapter = !!adapter,
                  theme = url_info$theme,
                  has_modal = !isTRUE(nomodal == 'true')))
  }, env = asNamespace('ravecore'))
}

app_ui_env[['rave-main-app']] <- function(adapter, theme = 'purple', ...){
  title = "R Analysis and Visualization of ECoG/iEEG Data"
  header = sprintf('RAVE (%s)', as.character(utils::packageVersion("ravecore")))

  adapter$module_list %?<-% find_modules()
  minfos <- adapter$module_list
  minfos <- minfos[minfos$active, ]
  groups <- unique(minfos$group_name)
  groups <- groups[groups != '______']

  items <- lapply(groups, function(group){
    rows <- minfos[minfos$group_name == group, ]

    sub_items <- lapply(seq_len(nrow(rows)), function(ii){
      row <- as.list(rows[ii, ])
      if(raveutils::is_valid_ish(row$icon, max_len = 1,
                                 mode = 'character', na = TRUE,
                                 blank = TRUE)){
        row$icon <- shiny::icon(row$icon)
      } else {
        row$icon <- NULL
      }
      as.call(list(
        quote(shinydashboard::menuSubItem),
        text = row$module_label,
        icon = row$icon,
        tabName = stringr::str_to_upper(row$module_id)
      ))
    })

    as.call(c(list(
      quote(shinydashboard::menuItem),
      text = group
    ), unname(sub_items)))

  })

  rows <- minfos[minfos$group_name == '______', ]
  if(nrow(rows)){
    misc_items <- lapply(seq_len(nrow(rows)), function(ii){
      row <- as.list(rows[ii, ])
      if(raveutils::is_valid_ish(row$icon, max_len = 1,
                                 mode = 'character', na = TRUE,
                                 blank = TRUE)){
        row$icon <- shiny::icon(row$icon)
      } else {
        row$icon <- NULL
      }
      as.call(list(
        quote(shinydashboard::menuItem),
        text = row$module_label,
        icon = row$icon,
        tabName = stringr::str_to_upper(row$module_id)
      ))
    })
    items <- c(items, misc_items)
  }

  sidebar_call <- as.call(c(list(
    quote(shinydashboard::sidebarMenu),
    id = '..rave_sidebar..'
  ), items))

  body_items <- lapply(minfos$module_id, function(module_id){
    as.call(list(
      quote(shinydashboard::tabItem),
      tabName = stringr::str_to_upper(module_id)
      # as.call(list(
      #   quote(shiny::uiOutput),
      #   sprintf('%s_UI', module_id)
      # ))
    ))
  })
  body_call <- as.call(c(list(
    quote(shinydashboard::tabItems)
  ), body_items))

  rave_dash_page(
    skin = theme,
    title = title,
    header = rave_dash_header(title = header, btn_text_right = ' '),
    control = rave_dash_control(disable = TRUE),
    sidebar = rave_dash_sidebar(
      # width = '280px',
      eval(sidebar_call)
    ),
    body = rave_dash_body(
      eval(body_call)
    )
  )
}


app_server_main <- function(input, output, session, adapter){

  rave_id <- raveutils::add_to_session(session, 'rave_id')
  adapter$module_list %?<-% find_modules()

  test_mode <- isTRUE(adapter$test.mode)

  module_list <- adapter$module_list
  module_list <- module_list[module_list$active, ]
  containers <- dipsaus::fastmap2()

  app_data <- dipsaus::fastmap2()
  app_data$delay_input = ravecore::rave_options('delay_input')


  internal_observers = dipsaus::fastmap2()
  if(test_mode){
    observe = shiny::observe
    observeEvent = shiny::observeEvent
  } else {
    observe = make_observe(internal_observers, error_handler = function(e){
      raveutils::rave_error("[Module ERROR] {e$message}")
    })
    observeEvent = make_observeEvent(internal_observers, error_handler = function(e){
      raveutils::rave_error("[Module ERROR] {e$message}")
    })
  }


  get_container <- function(module_id){
    module_id <- stringr::str_to_upper(module_id)
    sel <- stringr::str_to_upper(module_list$module_id) %in% module_id
    if(!any(sel)){
      return(NULL)
    }
    minfo <- module_list[sel, ]
    minfo <- minfo[1, ]

    if(test_mode){
      if(!inherits(containers[[module_id]], 'RAVEContainer')){
        container <- parse_module(
          package = minfo$package,
          module_id = minfo$module_id,
          debug = test_mode)

        container$with_context('rave_running', {
          shiny::callModule(shinirize, id = container$module_id,
                            container = container,
                            app_data = app_data,
                            adapter = adapter)
        })

        # shinirize(input, output, session, container, app_data)

        containers[[module_id]] <- container
      }
      return(containers[[module_id]])
    } else {
      tryCatch({
        if(!inherits(containers[[module_id]], 'RAVEContainer')){
          container <- parse_module(
            package = minfo$package,
            module_id = minfo$module_id,
            debug = test_mode)

          container$with_context('rave_running', {
            shiny::callModule(shinirize, id = container$module_id,
                              container = container,
                              app_data = app_data)
          })


          # shinirize(input, output, session, container, app_data)

          containers[[module_id]] <- container
        }
        return(containers[[module_id]])

      }, error = function(e){
        raveutils::rave_error('Cannot parse module {module_id} for the following reasons:')
        cat(e$message, '\n')
        traceback(e)
      })
    }


  }

  switch_container <- function(module_id){
    container <- get_container(module_id)
    if(!inherits(container, 'RAVEContainer')){ return() }

    shiny::removeNotification('..rave_error..', session)

    # assign('aaa', container, envir = globalenv())
    # assign('session', session, envir = globalenv())
    raveutils::add_to_session(session, key = 'rave_instance', val = container, override = TRUE)
    container$register_context('rave_running')

    raveutils::rave_info('Switched to module - {container$module_label}')

    if(!isTRUE(app_data$last_module_id == container$module_id)){

      previous_module <- get_container(app_data$last_module_id)
      if(inherits(previous_module, 'RAVEContainer')){
        # TODO: suspend all observers
        # previous_module$suspend()
      }

      app_data$last_module_id <- container$module_id
    }


    container$`@shiny_resume`()
  }

  remove_container <- function(module_id){
    module_id <- stringr::str_to_upper(module_id)
    container <- containers[[module_id]]

    # remove from ravecore:::rave_loaded_modules$module_id
    .subset2(container$module$containers, 'remove')(rave_id)

    raveutils::clear_env(container$user_observers)
    raveutils::clear_env(container$runtime_env)
    raveutils::clear_env(container$mask_env)
    raveutils::clear_env(container$static_env)
    raveutils::clear_env(container$wrapper_env)
    raveutils::clear_env(container$container_data)

    rm(container)
  }

  current_container <- function(){
    module_id <- raveutils::from_rave_context('module_id')
    if(length(module_id)){
      return(get_container(module_id))
    }
    NULL
  }

  # ---- Register to adapter
  adapter$switch_container <- switch_container

  # ---- When modules are switching back and forth
  shiny::observeEvent(input$..rave_sidebar.., {
    # make sure the corresponding module top the session
    module_id_uppercase <- stringr::str_to_upper(input$..rave_sidebar..)

    # Create module

    container <- get_container(module_id_uppercase)
    if(is.null(container)){
      if(length(app_data$last_module_id)){
        module_link <- shiny::tagList(
          shiny::br(),
          shiny::actionLink('..rave_switch_back..', '[click here to return to previous module]')
        )
      } else {
        module_link = ''
      }
      shiny::showNotification(
        ui = shiny::p(
          'Cannot find/parse requested module - ID', module_id_uppercase, module_link),
        id = '..rave_error..', type = 'error', closeButton = FALSE, duration = NULL)
    } else {
      switch_container(module_id_uppercase)
    }
  }, priority = Inf)

  shiny::observeEvent(input$..rave_switch_back.., {
    switch_container(app_data$last_module_id)
  })

  # ---- Control containers
  shiny::observeEvent(input$data_select, {

    container <- current_container()
    if(inherits(container, 'RAVEContainer')){

      if(container$data_selector_opened){
        container$`@safe_close_selector`()
      } else {
        raveutils::rave_debug('Open up data selector')
        container$`@shiny_resume`(close_if_pass = FALSE)
      }
    } else {
      raveutils::rave_debug('Cannot find container to open up data selector')
    }
  })


  session$onSessionEnded(function() {

    if(isTRUE(adapter$test.mode)){
      adapter$active_session = adapter$active_session - 1L
      if(adapter$active_session == 0){
        raveutils::rave_info('No active shiny session - Reset context')
        # set context
        ctx <- adapter$context
        if(isTRUE(ctx$context == 'rave_module_debug')){
          raveutils::rave_context('rave_module_debug', ctx$package, ctx$module_id, .force = TRUE)
        } else {
          raveutils::rave_context('default', .force = TRUE)
        }
        rm(ctx)
      }
      adapter$containers <- containers
      return()
    }

    raveutils::rave_debug('Session ended')
    # clear containers
    for(nm in names(containers)){
      remove_container(nm)
    }

    adapter$active_session = adapter$active_session - 1L
    if(adapter$active_session == 0){
      raveutils::rave_info('No active shiny session - Reset context')
      # set context
      ctx <- adapter$context
      if(isTRUE(ctx$context == 'rave_module_debug')){
        raveutils::rave_context('rave_module_debug', ctx$package, ctx$module_id, .force = TRUE)
      } else {
        raveutils::rave_context('default', .force = TRUE)
      }
      rm(ctx)
    }

    raveutils::clear_env(session$userData)

  })

}

#' @export
start_rave <- function(host = '127.0.0.1', port = NULL, launch_browser=TRUE,
                       test_mode = FALSE, token = NULL, theme = 'purple',
                       .adapter = dipsaus::fastmap2()){
  adapter <- .adapter
  adapter$test.mode = isTRUE(test_mode)
  adapter$context <- raveutils::rave_context()
  adapter$active_session <- 0L

  ui <- app_ui(adapter = adapter, theme = theme, token = token)

  server <- function(input, output, session){
    adapter$active_session <- adapter$active_session + 1L
    app_server_main(input, output, session, adapter)
  }

  app <- shiny::shinyApp(ui, server, options = list(
    test.mode = test_mode,
    launch.browser=launch_browser, host = host, port = port))

  print(app)
}

