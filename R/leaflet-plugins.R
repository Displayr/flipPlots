#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
leafletPlugins <- function(message, width = NULL, height = NULL) {
    # forward options using x
    x = list(
        message = message
    )

    # create widget
    htmlwidgets::createWidget(
        name = 'leaflet-plugins',
        x,
        width = width,
        height = height,
        package = 'flipPlots'
    )
}

#' Shiny bindings for leaflet-plugins
#'
#' Output and render functions for using leaflet-plugins within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a leaflet-plugins
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name leaflet-plugins-shiny
#'
#' @export
leafletPluginsOutput <- function(outputId, width = '100%', height = '400px'){
    htmlwidgets::shinyWidgetOutput(outputId, 'leaflet-plugins', width, height, package = 'flipPlots')
}

#' @rdname leaflet-plugins-shiny
#' @export
renderLeafletPlugins <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) { expr <- substitute(expr) } # force quoted
    htmlwidgets::shinyRenderWidget(expr, leafletPluginsOutput, env, quoted = TRUE)
}
