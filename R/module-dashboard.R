# Rewrite of shinydashboard page such that control panel is also included
# Also default RAVE icon
rave_dash_page <- function(
  header, sidebar, control, body, title = NULL,
  skin = c("blue", "black", "purple", "green", "red", "yellow"), controlbar_opened = FALSE,
  initial_mask = NULL
) {
  skin <- match.arg(skin)
  extractTitle <- function(header) {
    x <- header$children[[2]]
    if (x$name == "span" && !is.null(x$attribs$class) &&
        x$attribs$class == "logo" && length(x$children) != 0) {
      x$children[[1]]
    } else {
      ""
    }
  }
  title <- ifelse(is.null(title), extractTitle(header), title)
  content <- shiny::div(class = "wrapper", header, sidebar, body, control)

  findAttribute = function (x, attr, val) {
    if (is.atomic(x))
      return(FALSE)
    if (!is.null(x$attribs[[attr]])) {
      if (identical(x$attribs[[attr]], val))
        return(TRUE)
      else return(FALSE)
    }
    if (length(x$children) > 0) {
      return(any(unlist(lapply(x$children, findAttribute, attr,val))))
    }
    return(FALSE)
  }


  collapsed <- findAttribute(sidebar, "data-collapsed", "true")



  # Modified addDeps:
  addDeps = function (x)
  {
    if (getOption("shiny.minified", TRUE)) {
      adminLTE_js <- "app.min.js"
      shinydashboard_js <- "shinydashboard.min.js"
      adminLTE_css <- c("AdminLTE.min.css", "_all-skins.min.css")
    }
    else {
      adminLTE_js <- "app.js"
      shinydashboard_js <- "shinydashboard.js"
      adminLTE_css <- c("AdminLTE.css", "_all-skins.css")
    }
    dashboardDeps <- list(
      # Load customized style and scripts
      htmltools::htmlDependency(
        "dipsaus", "0.1.7",
        c(file = system.file('shiny-addons/dipsaus', package = 'dipsaus')),
        script = c(
          'dipsaus-dipterix-lib.js'
        ),
        stylesheet = 'dipsaus.css'
      ),
      htmltools::htmlDependency(
        "Dipterix", "0.0.1",
        c(file = system.file('assets/', package = 'ravecore')),
        script = c(
          'dipterix.js',
          'dipterix_inputs.js'
        ),
        stylesheet = 'dipterix.css'
      ),
      # load AdminLTE
      htmltools::htmlDependency(
        "AdminLTE", "2.0.6",
        c(file = system.file("AdminLTE", package = "shinydashboard")),
        script = adminLTE_js, stylesheet = adminLTE_css),
      htmltools::htmlDependency(
        "shinydashboard",
        as.character(utils::packageVersion("shinydashboard")),
        c(file = system.file(package = "shinydashboard")),
        script = shinydashboard_js,
        stylesheet = "shinydashboard.css"))

    appendDependencies = function (x, value)
    {
      if (inherits(value, "html_dependency"))
        value <- list(value)
      old <- attr(x, "html_dependencies", TRUE)
      htmltools::htmlDependencies(x) <- c(old, value)
      x
    }

    appendDependencies(x, dashboardDeps)
  }


  if(controlbar_opened){
    cls = ' control-sidebar-open'
  }else{
    cls = ''
  }

  addDeps(
    shiny::tags$body(
      class = paste0("skin-", skin, cls), # if you want control-sidebar to be opened, add " control-sidebar-open"

      style = "min-height: 611px;",
      shiny::tags$head(shiny::tags$link(
        rel = "icon", type = "image/x-icon",
        href = dipsaus::to_datauri(system.file('assets/images/favicon.ico', package = 'ravecore')))),
      shiny::bootstrapPage(
        raveutils::register_js(),
        shiny::div(
          id = '__rave__mask__',
          class = ifelse(is.null(initial_mask), 'hidden', ''),
          shiny::div(class = 'loading_info',
                     initial_mask)
        ),
        content,
        title = title
      )
    )
  )
}


# to support control panel
rave_dash_header <- function (
  ..., title = NULL, titleWidth = NULL, disable = FALSE,
  btn_text_right = 'Controls', .list = NULL)
{
  items <- .list
  titleWidth <- htmltools::validateCssUnit(titleWidth)
  custom_css <- NULL
  # if (!is.null(titleWidth)) {
  #   custom_css <- shiny::tags$head(shiny::tags$style(shiny::HTML(
  #     gsub("_WIDTH_", titleWidth, fixed = TRUE, "\n      @media (min-width: 768px) {\n        .main-header > .navbar {\n          margin-left: _WIDTH_;\n        }\n        .main-header .logo {\n          width: _WIDTH_;\n        }\n      }\n    "))))
  # }
  shiny::tags$header(
    class = "main-header", custom_css, style = if (disable) "display: none;",
    shiny::tags$nav(
      class = "navbar navbar-fixed-top",
      role = "navigation", shiny::span(shiny::icon("bars"), style = "display:none;"),
      shiny::span(class = "logo", title),
      shiny::div(
        class = 'navbar-collapse pull-left collapse',
        id="navbar-collapse", `aria-expanded`="false",
        shiny::tags$ul(
          class = 'nav navbar-nav',
          shiny::tags$li(
            shiny::a(
              href = "#", class = "nav-item nav-link force-recalculate",
              id = "rave_nav_sidebar",
              `data-toggle` = "offcanvas",
              role = "button", shiny::span(class = "sr-only", "Toggle navigation"),
              shiny::icon('th')
              # shiny::span('Switch Dataset')
              # shiny::textOutput('..rave_data_nav..', inline = TRUE),
              # `hover-text` = 'Change Loaded Data'
            )
          ),
          shiny::tags$li(
            # shiny::a(href = "#", class = "nav-item nav-link force-recalculate",
            #          `data-toggle` = "rave-toggle-inputs",
            #          role = "button", shiny::span(class = "sr-only", "Toggle input panel"),
            #          shiny::icon('keyboard-o'), shiny::span('Input Panels')
            # )
            shiny::actionLink("data_select", "Select Data",
            icon = shiny::icon("tasks"), role = "button", class = "nav-item nav-link")
          ),
          ...
        )
      ),

      shiny::div(
        class = "navbar-custom-menu",
        shiny::tags$ul(
          class = "nav navbar-nav", shiny::tagList( items ),
          shiny::tags$li(
            shiny::a(href = "#", class = "nav-item nav-link force-recalculate",
                     `data-toggle` = "control-sidebar",
                     role = "button",
                     shiny::span(class = "sr-only", "Toggle control"),
                     shiny::span(btn_text_right)
            )
          )
        )
      )
    )
  )
}




rave_dash_control = function (...,
                             disable = FALSE,
                             collapsed = FALSE)
{
  dataValue <- shiny::restoreInput(id = "sidebarCollapsed",
                                   default = collapsed)
  if (disable)
    dataValue <- TRUE
  dataValueString <- if (dataValue)
    "true"
  else
    "false"
  shiny::tagList(
    shiny::tags$aside(
      class = "control-sidebar control-sidebar-dark",
      `data-collapsed` = dataValueString,
      shiny::div(
        class = 'tab-content',
        ...
      )
    ),
    shiny::div(class = "control-sidebar-bg")
  )
}


rave_dash_sidebar <- shinydashboard::dashboardSidebar

rave_dash_body <- shinydashboard::dashboardBody


# allow customized header color and default to full width
box <- function (..., title = NULL, footer = NULL, status = NULL, solidHeader = FALSE,
                background = NULL, width = 12L, height = NULL, collapsible = FALSE,
                collapsed = FALSE, headerColor = '#f4f4f4')
{
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    boxClass <- paste0(boxClass, " bg-", background)
  }
  style <- sprintf('border-top-color: %s; ', headerColor)
  if (!is.null(height)) {
    style <- paste0("height: ", htmltools::validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- shiny::h3(class = "box-title", title)
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status
    buttonStatus %?<-% "default"
    collapseIcon <- if (collapsed)
      "plus"
    else "minus"
    collapseTag <- shiny::div(class = "box-tools pull-right", shiny::tags$button(class = paste0("btn btn-box-tool"),
                                                                   `data-widget` = "collapse", shiny::icon(collapseIcon)))
  }
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- shiny::div(class = "box-header", titleTag, collapseTag, style = sprintf('background-color: %s; ', headerColor))
  }
  shiny::div(class = if (!is.null(width))
    paste0("col-sm-", width), shiny::div(class = boxClass, style = if (!is.null(style))
      style, headerTag, shiny::div(class = "box-body", ...), if (!is.null(footer))
        shiny::div(class = "box-footer", footer)))
}