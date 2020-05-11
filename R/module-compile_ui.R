#' @export
define_input <- raveutils::rave_context_generics(
  fun_name = 'define_input', alist(
    definition=, init_args=, init_expr=, keyword = "inputId",
    update_level=2, ...=
  ))

#' @export
define_input.rave_compile <- function(definition, init_args, init_expr, keyword = "inputId",
                                      update_level = 2, update_fun, ...) {

  ctx = raveutils::rave_context()
  definition = substitute(definition)
  init_expr = substitute(init_expr)

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
    }), names = keyword))

  update_hook <- do_nothing
  if(length(init_args)){
    update_fun = substitute(update_fun)
    if(missing(update_fun)){
      # guess the update function
      update_fun <- str2lang(raveutils::guess_shiny_update(call, parse = FALSE))
    }

    update_hook <- raveutils::new_function2(body = {
      # once data loaded, update the UI
      ..env = new.env()
      with(..env, !!init_expr)
      args = sapply(!!init_args, function(arg){ ..env[[arg]] }, USE.NAMES = TRUE, simplify = FALSE)
      update_fun = eval(!!update_fun)
      if(length(update_fun)){
        do.call(update_fun, structure(c(
          list(shiny::getDefaultReactiveDomain(), !!input_id), args
        ), names = c('session', !!keyword, !!init_args)))
      }
    }, env = ctx$instance$runtime_env)

  }


  # Add call
  ctx$instance$input_components[[input_id]] = list(
    generator = raveutils::new_function2(body = { !!call }, env = ctx$instance$wrapper_env),
    hook = update_hook,
    update_level = update_level,
    input_id = input_id
  )

}





#' @export
define_output <- raveutils::rave_context_generics(
  fun_name = 'define_output', alist(
    definition=, title = '', width = 12L, order = Inf,
    keyword = 'outputId', watch_reactive = 'input$..rave_output_update_all..', ...=
  ))

#' @export
define_output.rave_compile <- function(definition, title = '', width = 12L, order = Inf,
                                       keyword = 'outputId', watch_reactive = 'input$..rave_output_update_all..',
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


  render_fun = substitute(render_fun)
  if(missing(render_fun)){
    # guess the update function
    render_fun <- str2lang(raveutils::guess_shiny_output(call, parse = FALSE))
  }
  watch_reactive <- c(list(quote(`{`)), lapply(watch_reactive, str2lang))
  watch_reactive <- as.call(watch_reactive)


  renderer <- raveutils::new_function2(body = {
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
          args = c(TRUE, raveutils::test_farg(f, c('session_data', 'module_data', 'global_data')))
          args = list(quote(f),
                      session_data = quote(ravecore:::getDefaultSessionData()),
                      module_data = quote(ravecore:::getDefaultModuleData()),
                      global_data = quote(ravecore:::getDefaultDataRepository()))[args]
          eval(as.call(args))
        } else{
          stop(raveutils::rave_condition(
            !!sprintf('Function %s not found', sQuote(output_id)),
            class = c('shiny.silent.error', 'validation', 'simpleError', 'error', 'comdition')))
        }

      })
    ))
    eval(renderer_expr)

  }, env = ctx$instance$static_env)

  ctx$instance$output_components[[output_id]] = list(
    generator = raveutils::new_function2(body = { !!call }, env = ctx$instance$wrapper_env),
    renderer = renderer,
    title = title, width = width, order = order,
    watch_reactive = watch_reactive
  )

}