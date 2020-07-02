
#' @export
view_layout <- function(host = '127.0.0.1', port = NULL, theme = 'red'){

  ctx <- raveutils::rave_context()

  ctx$module_id

  adapter <- dipsaus::fastmap2()
  adapter$test.mode = TRUE
  adapter$context <- raveutils::rave_context()
  adapter$active_session <- 0L
  adapter$module_list <- data.frame(
    stringsAsFactors = FALSE,
    module_id = ctx$module_id,
    module_label = 'Debug Mode',
    group_name = '______',
    order = 0L,
    package = ctx$package,
    notes = '',
    active = TRUE,
    icon = 'eye-slash'
  )

  ui <- app_ui(adapter = adapter, theme = theme, token = NULL)

  server <- function(input, output, session){
    adapter$active_session <- adapter$active_session + 1L
    app_server_main(input, output, session, adapter)
  }

  app <- shiny::shinyApp(ui, server, options = list(
    test.mode = TRUE,
    launch.browser = TRUE, host = host, port = port))


  print(app)
}

