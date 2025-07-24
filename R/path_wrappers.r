#' Rectangle path convenience wrapper
#'
#' `RECT()` is a wrapper around `MZ()` to create
#' rectangle shaped paths.
#' @inheritParams M
#' @param w,h The width and height of the rectangle.
#' @param ... Passed to [MZ()].
#' @return A [dee()] object.
#' @examples
#' RECT(x = 5, y = 5, w = 4, h = 6)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(RECT(x = 5, y = 5, w = 4, h = 6),
#'        height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
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

#' Ellipse path convenience wrapper
#'
#' `ELLIPSE()` is a wrapper around `M()` and `AZ()` to create
#' ellipse shaped paths.
#' `CIRCLE()` is a special case to create circle shaped paths.
#' @inheritParams M
#' @param r The radius of the circle.
#' @param rx,ry The radii of the ellipse.
#' @param ... Passed to [M()] and [AZ()].
#' @return A [dee()] object.
#' @examples
#' CIRCLE(x = 5, y = 5, r = 2)
#' ELLIPSE(x = 5, y = 5, rx = 2, ry = 3)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(ELLIPSE(x = 5, y = 5, rx = 2, ry = 3),
#'        height = 10, width = 10,
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' @export
ELLIPSE <- function(x, y = NULL, rx, ry = rx, ...) {
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    .mapply(ELLIPSE_HELPER,
            list(x = x, y = y, rx = rx, ry = ry),
            list(...)) |>
        Reduce(`+.dee`, x = _)
}

#' @rdname ELLIPSE
#' @export
CIRCLE <- function(x, y = NULL, r, ...) {
    ELLIPSE(x, y, r, r, ...)
}

ELLIPSE_HELPER <- function(x, y, rx, ry, ...) {
    M(x, y + ry, ...) + 
        AZ(rx, ry, 0, 0, 0, x, c(y - ry, y + ry), ...)
}

#' Polygon path convenience wrapper
#'
#' `POLYGON()` is a wrapper around `MZ()` to create
#' polygon shaped paths.  
#' If the argument `offset` is nonzero
#' will use [polyclip::polyoffset()] to compute an offset region.
#' @inheritParams M
#' @param ... Passed to [MZ()].
#' @param offset If a positive number the distance for *outward* polygon offsetting.  If a negative number the distance for *inward* polygon offsetting.
#' @param linejoin If `offset` is nonzero the type of join operation
#'                 to use at each vertex when computing the offset region.
#' @param miterlimit Tolerance parameter if `offset` is nonzero
#'                   and `linejoin = "miter"`.  See [polyclip::polyoffset()].
#' @return A [dee()] object.
#' @examples
#' l <- list(x = c(2, 5, 8, 5), y = c(5, 8, 5, 2))
#' po <- POLYGON(l, offset = 1)
#' p <- POLYGON(l)
#' pi <- POLYGON(l, offset = -1)
#' if (requireNamespace("omsvg", quietly = TRUE) &&
#'     requireNamespace("svgparser", quietly = TRUE)) {
#'   plot(po + p + pi, height = 10, width = 10,
#'        attrs = list(fill_rule = "evenodd"),
#'        fill = "red", stroke = "black", stroke_width = 4)
#' }
#' @seealso [polyclip::polyoffset()] for details on computing the offset region when `offset` is non-zero.
#' @export
POLYGON <- function(x, y = NULL, ...,
                    offset = 0,
                    linejoin = c("miter", "round"),
                    miterlimit = 4) {
    if (offset == 0)
        return (MZ(x, y, ...))
    stopifnot(requireNamespace("polyclip", quietly = TRUE))
    p <- as_coords(x, y)
    x <- p$x
    y <- p$y
    jointype <- match.arg(linejoin)
    xy <- polyclip::polyoffset(list(x = x, y = y), offset,
                               jointype = jointype, miterlim = miterlimit)
    MZ(xy, ...)
}
