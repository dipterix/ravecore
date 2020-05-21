#' @export
define_input <- raveutils::rave_context_generics(
  fun_name = 'define_input', alist(
    definition=, init_args=, init_expr=, keyword = "inputId",
    update_level=2, ...=
  ))


#' @export
UPDATE_LEVEL <- list(
  render_only = 1L,
  run_all = 2L,
  manual = 0L
)

#' @export
define_input.rave_compile <- function(definition, init_args, init_expr, keyword = "inputId",
                                      update_level = 2, update_fun, recursive_ns = FALSE, ...) {

  ctx = raveutils::rave_context()
  definition = substitute(definition)

  module_id = ctx$module_id
  ns = shiny::NS(module_id)
  # Sometimes the widget comes with load_script which adds some dynamic reactives
  eval(definition, envir = ctx$instance$runtime_env)

  input_id <- NULL
  call <- dipsaus::match_calls(
    definition, quoted = TRUE, envir = ctx$instance$wrapper_env,
    replace_args = structure(list(function(arg, call){
      input_id <<- arg
      ns(arg)
    }), names = keyword), recursive = recursive_ns)

  update_hook <- do_nothing
  if(!missing(init_args) && length(init_args)){
    init_expr = substitute(init_expr)

    update_fun = substitute(update_fun)
    if(missing(update_fun)){
      # guess the update function
      update_fun <- str2lang(raveutils::guess_shiny_update(call, parse = FALSE))
    }

    update_hook <- dipsaus::new_function2(body = {
      # once data loaded, update the UI
      ..env = new.env()
      with(..env, !!init_expr)

      call = as.call(list( quote(!!update_fun) ))
      call[['session']] <- quote(shiny::getDefaultReactiveDomain())
      call[[!!keyword]] <- !!input_id
      for(nm in !!init_args){
        call[[nm]] <- ..env[[nm]]
      }
      # print(call)

      eval(call)
#
#       update_fun = eval(!!update_fun)
#       if(length(update_fun)){
#         do.call(update_fun, structure(c(
#           list(shiny::getDefaultReactiveDomain(), !!input_id), args
#         ), names = c('session', !!keyword, !!init_args)))
#       }
    }, env = ctx$instance$runtime_env)

  } else {
    special_case = unlist(as.character(as.list(definition[[1]])))
    special_case = special_case[length(special_case)]
    if(special_case == 'customizedUI'){
      update_hook <- dipsaus::new_function2(body = {
        # once data loaded, update the UI
        # try to get function

        output[[!!input_id]] <- shiny::renderUI({
          f <- get0(!!input_id, mode = 'function', inherits = TRUE)

          if(is.function(f)){
            f()
          } else {
            NULL
          }
        })

      }, env = ctx$instance$runtime_env)
    }
  }


  # Add call
  ctx$instance$input_components[[input_id]] = list(
    generator = dipsaus::new_function2(body = { !!call }, env = ctx$instance$wrapper_env),
    hook = update_hook,
    update_level = update_level,
    input_id = input_id
  )

}

#' @export
define_input.rave_module_debug <- function(definition, init_args, init_expr, keyword = "inputId",
                                           update_level = 2, update_fun, ...){
  ctx = raveutils::rave_context()
  definition = substitute(definition)
  eval(definition, envir = new.env(parent = parent.frame()))

  module_id = ctx$module_id
  ns = shiny::NS(module_id)

  input_id <- NULL
  call <- dipsaus::match_calls(
    definition, quoted = TRUE, envir = ctx$instance$wrapper_env,
    replace_args = structure(list(function(arg, call){
      input_id <<- arg
      ns(arg)
    }), names = keyword))

  raveutils::rave_info('Modified expression: ')
  print(call)


  val_name = c('value', 'selected')

  if( !missing(init_args) ){
    val_name = val_name[val_name %in% init_args]
    if(length(val_name)){
      init_expr = substitute(init_expr)
      env <- new.env(parent = .GlobalEnv)
      eval(init_expr , envir = env)
      .GlobalEnv[[input_id]] = env[[val_name]]
      raveutils::rave_debug('[DEBUG]: Assigned {input_id} as {deparse1(env[[val_name]])}')
    }
  } else {
    val_name = val_name[val_name %in% names(call)]
    if(length(val_name)){
      .GlobalEnv[[input_id]] = eval(call[[val_name]], envir = parent.frame())
      raveutils::rave_debug('[DEBUG]: Assigned {input_id} as {deparse1(.GlobalEnv[[input_id]])}')
    }
  }

  invisible()
}

#' @export
define_output <- raveutils::rave_context_generics(
  fun_name = 'define_output', alist(
    definition=, title = '', width = 12L, order = Inf,
    keyword = 'outputId', watch_reactive = 'input$..rave_output_update_all..', ...=
  ))

#' @export
define_output.rave_compile <- function(definition, title = '', width = 12L, order = Inf,
                                       keyword = 'outputId',
                                       watch_reactive = 'input$..rave_output_update_all..',
                                       render_fun, ...){
  ctx = raveutils::rave_context()
  definition = substitute(definition)

  module_id = ctx$module_id
  ns = shiny::NS(module_id)
  # Sometimes the widget comes with load_script which adds some dynamic reactives
  eval(definition, envir = ctx$instance$runtime_env)

  output_id <- NULL
  call <- dipsaus::match_calls(
    definition, quoted = TRUE, envir = ctx$instance$wrapper_env,
    replace_args = structure(list(function(arg, call){
      output_id <<- arg
      ns(arg)
    }), names = keyword))

  # special case
  if(!length(output_id) && 'inputId' %in% names(call)){
    # well, it could be customizedInput
    output_id <- eval(call[['inputId']])
    call[['inputId']] <- ns(output_id)
  }


  render_fun = substitute(render_fun)
  if(missing(render_fun)){
    # guess the update function
    render_fun <- str2lang(raveutils::guess_shiny_output(call, parse = FALSE))
  }
  watch_reactive <- c(list(quote(`{`)), lapply(watch_reactive, str2lang))
  watch_reactive <- as.call(watch_reactive)


  renderer <- dipsaus::new_function2(body = {
    # once data loaded, update the UI
    input <- getDefaultReactiveInput()
    renderer_expr <- as.call(list(
      quote(!!render_fun),
      quote({
        input[[!!sprintf('%s__update', output_id)]]
        !!watch_reactive

        # find function
        f <- get0(!!output_id)
        if(is.function(f)){

          raveutils::rave_debug(!!sprintf('Rendering - %s', output_id))

          if(raveutils::test_farg(f, c('results'))){
            # combatible mode
            results = list(get_value = function(key, ifnotfound = NULL){
              get0(key, ifnotfound = ifnotfound)
            })
            f(results)
          } else{
            args = c(TRUE, raveutils::test_farg(f, c('session_data', 'package_data', 'global_data')))
            args = list(quote(f),
                        session_data = quote(ravecore:::getDefaultSessionData()),
                        package_data = quote(ravecore:::getDefaultPackageData()),
                        global_data = quote(ravecore:::getDefaultDataRepository()))[args]
            eval(as.call(args))
          }


        } else{
          stop(raveutils::rave_condition(
            !!sprintf('Function %s not found', sQuote(output_id)),
            class = c('shiny.silent.error', 'validation', 'simpleError', 'error', 'comdition')))
        }

      })
    ))
    eval(renderer_expr)

  }, env = ctx$instance$runtime_env)

  ctx$instance$output_components[[output_id]] = list(
    generator = dipsaus::new_function2(body = { !!call }, env = ctx$instance$wrapper_env),
    renderer = renderer,
    title = title, width = width, order = order,
    watch_reactive = watch_reactive
  )

}

#' @export
define_output.rave_module_debug <- function(definition, title = '', width = 12L, order = Inf,
                                            keyword = 'outputId',
                                            watch_reactive = 'input$..rave_output_update_all..', ...){
  ctx = raveutils::rave_context()
  definition = substitute(definition)

  module_id = ctx$module_id
  package = ctx$package
  ns = shiny::NS(module_id)
  # Sometimes the widget comes with load_script which adds some dynamic reactives
  eval(definition, envir = new.env(parent = parent.frame()))

  output_id <- NULL
  call <- dipsaus::match_calls(
    definition, quoted = TRUE, envir = parent.frame(),
    replace_args = structure(list(function(arg, call){
      output_id <<- arg
      ns(arg)
    }), names = keyword))

  # special case
  if(!length(output_id) && 'inputId' %in% names(call)){
    # well, it could be customizedInput
    output_id <- eval(call[['inputId']])
    call[['inputId']] <- ns(output_id)
  }
  if(!length(output_id)){
    print(call)
    raveutils::rave_fatal('Cannot find outputId for the above definition. Please specify keyword')
  }


  render_fun = substitute(render_fun)
  if(missing(render_fun)){
    # guess the update function
    render_fun <- str2lang(raveutils::guess_shiny_output(call, parse = FALSE))
  }
  watch_reactive <- c(list(quote(`{`)), lapply(watch_reactive, str2lang))
  watch_reactive <- as.call(watch_reactive)

  # look for output_id function
  output_fun <- get0(output_id, envir = parent.frame(), mode = 'function', ifnotfound = NULL)
  # TODO: Do we need to search from package?
  output_fun %?<-% get0(output_id, envir = asNamespace(package), mode = 'function', ifnotfound = NULL, inherits = FALSE)

  output <- NULL
  call <- as.call(list(render_fun))
  call[[2]] <- as.call(list(str2lang(output_id)))
  call[[2]][[2]] = quote(...)

  expr <- rlang::quo({
    output[[!!output_id]] <- !!call
  })

  raveutils::rave_info('Renderer for output {sQuote(output_id)} ({title}):')
  print(rlang::quo_squash(expr))

  invisible()
}


#' @export
customizedUI <- function(inputId, width = 12L, ...){
  shiny::uiOutput(inputId, ...)
}



