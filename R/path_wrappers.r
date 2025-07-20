#' Rectangle path convenience wrapper
#'
#' `MZ_RECT()` is a wrapper around `MZ()` to create
#' rectangle shaped paths.
#' @inheritParams M
#' @param w,h The width and height of the rectangle.
#' @param ... Passed to [MZ()].
#' @return A [dee()] object.
#' @examples
#' RECT(x = 5, y = 5, w = 4, h = 6)
#' @export
RECT <- function(x, y = NULL, w, h, ...) {
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    .mapply(RECT_HELPER, list(x = x, y = y, w = w, h = h), list(...)) |>
        Reduce(`+.dee`, x = _)
}

RECT_HELPER <- function(x, y, w, h, ...) {
    xl <- x - 0.5 * w
    xr <- x + 0.5 * w
    yb <- y - 0.5 * h
    yt <- y + 0.5 * h
    x <- c(xl, xl, xr, xr)
    y <- c(yb, yt, yt, yb)
    MZ(x, y, ...)
}

#' Circle path convenience wrapper
#'
#' `MAZ_CIRCLE()` is a wrapper around `M()` and `AZ()` to create
#' circle shaped paths.
#' @inheritParams M
#' @param r The radius of the circle.
#' @param ... Passed to [M()] and [AZ()].
#' @return A [dee()] object.
#' @examples
#' CIRCLE(x = 5, y = 5, r = 2)
#' @export
CIRCLE <- function(x, y = NULL, r, ...) {
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    .mapply(CIRCLE_HELPER, list(x = x, y = y, r = r), list(...)) |>
        Reduce(`+.dee`, x = _)
}

CIRCLE_HELPER <- function(x, y, r, ...) {
    M(x, y + r, ...) + 
        AZ(r, r, 0, 0, 0, x, c(y - r, y + r), ...)
}
