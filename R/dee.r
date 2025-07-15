#' Build an object of class "dee"
#'
#' `dee()` casts a string to an object of class "dee".
#'
#' @param x A character vector.
#' @return An object of class "dee".
#' @examples dee("M 10,30") + dee("V 30")
#' @export
dee <- function(x) {
    x <- as.character(x)
    class(x) <- c("dee", "character")
    x
}

#' @export
`+.dee` <- function(e1, e2) {
    dee(paste(e1, e2))
}

#' @export
c.dee <- function(...) {
    dee(NextMethod())
}

#' @export
format.dee <- function(x, ...) {
    x <- as.character(x)
    x <- gsub("([Z|z] )", "\\1\n", x)
    x
}

#' @export
print.dee <- function(x, ...) {
    check_dots_empty()
    s <- format.dee(x, ...) |> paste(collapse = "\n")
    header <- paste0("<dee[", length(x), "]>\n")
    cat(header, s, "\n", sep = "")
    invisible(x)
}

#' Plot an object of class "dee"
#'
#' `plot()` an object of class "dee" using `omgsvg::SVG()`, `omsvg::svg_path()` and `svgparser::read_svg()`.
#' @param x An object of class "x".
#' @param ... Passed to [omsvg::svg_path()].
#' @param height,width svg width and height.
#' @param background_color Background color.
#' @param stroke,stroke_width,fill,attrs Passed to [omsvg::svg_path()].
#'
#' @return `invisible(NULL)`
# # https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d#example
#' @examples
#' d <- M(10, 30) +
#'      A(20, 20, 0, 0, 1, 50, 30) +
#'      A(20, 20, 0, 0, 1, 90, 30) +
#'      Q(90, 60, 50, 90) +
#'      Q(10, 60, 10, 30) +
#'      Z()
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(d, height = 100, width = 100, background_color = "cyan",
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' @export
plot.dee <- function(x, ...,
                     height = getOption("dee.height"),
                     width = getOption("dee.width"),
                     background_color = getOption("dee.background_color", "none"),
                     stroke = getOption("dee.stroke"),
                     stroke_width = getOption("dee.stroke_width"),
                     fill = getOption("dee.fill"),
                     attrs = getOption("dee.attrs")) {
    stopifnot(is.numeric(height), 
              is.numeric(width),
              requireNamespace("omsvg", quietly = TRUE),
              requireNamespace("svgparser", quietly = TRUE))
    bg <- MZ(x = c(0, 0, width, width),
             y = c(0, height, height, 0))
    svg <- omsvg::SVG(width = width, height = height, viewbox = TRUE) |>
            omsvg::svg_path(bg, fill = background_color, stroke = "none")
    for (d in x) {
        svg <- svg |> omsvg::svg_path(d,
            stroke = stroke, stroke_width,
            fill = fill, attrs = attrs, ...)
    }
    g <- as.character(svg) |>
        paste(collapse = "\n") |>
        svgparser::read_svg()
    grid::grid.newpage()
    grid::grid.draw(g)
    invisible(NULL)
}
