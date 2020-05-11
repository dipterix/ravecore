ui <- rave_dash_page(
  header = rave_dash_header(title = 'RAVE Dashboard'),
  control = rave_dash_control(disable = TRUE),
  sidebar = rave_dash_sidebar(disable = TRUE),
  body = rave_dash_body(
    shiny::div(
      class = 'rave-module-container loading',
      shiny::div(
        class = 'rave-dashboard-loader-wrapper',
        shiny::div(
          class = 'rave-dashboard-loader',
          shiny::inputPanel(
            shiny::htmlOutput("rave_data_loader")
          )
        )
      ),
      shiny::div(
        class = 'rave-module-main-wrapper',
        shiny::div(
          class = 'rave-module-main',
          shiny::actionButton('ok', 'OK')
        )

      )

    )
  )
)


shiny::shinyApp(
  ui = ui, server = function(input, output, session){
    observeEvent(input$ok, {

    })
  },
  options = list(
    launch.browser=TRUE
  )
)
